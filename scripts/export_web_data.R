# export_web_data.R
# Export countries' Kontur H3 hexagons + map settings for the web explorer in
# web/ (deck.gl H3HexagonLayer). Uses the same countries.R config as the
# rayshader maps; web-only settings live in each country's optional `web` list.
#
# Writes, per country, to web/data/{key}/:
#
#   hexes.json (columnar, so ~150k hexes stay small and parse fast)
#     { "key": "bangladesh", "name": "Bangladesh", "resolution": 8,
#       "total": 173889934, "max": 37876,
#       "palette": ["#a8ded5", ...],
#       "views":  [{ "id", "label", "bounds": [west, south, east, north] }],
#       "labels": [{ "name", "kind": "region"|"district",
#                    "position": [lon, lat] }],
#       "places": [[subdistrict, district, region], ...],
#       "h3": ["88...fffff", ...], "population": [79, ...],
#       "place": [12, ...] }          # index into places, -1 if none
#
#   boundaries.geojson
#     Region and district outlines (property "kind"), simplified and clipped
#     to land.
#
# Then rebuilds, from every exported country folder:
#   web/data/countries.json  the index behind the search box (countries, their
#                            city and region views, summary facts)
#   web/index.html           landing page  } from scripts/templates/
#   web/{slug}/index.html    country pages } explorer.html
#   web/sitemap.xml          every page, with the date its content changed
#
# Usage (from repo root):
#   Rscript scripts/export_web_data.R bangladesh
#   Rscript scripts/export_web_data.R bangladesh nepal
#   Rscript scripts/export_web_data.R --manifest   # countries.json + pages

library(sf)
library(jsonlite)

source("./scripts/countries.R")

web_data_dir <- "web/data"
simplify_metres <- 150
# Decimal places in boundaries.geojson (4 ≈ 11 m).
coordinate_precision <- 4
# Average H3 resolution 8 cell area, for "people per km²" in summaries.
hex_area_km2 <- 0.7373276

# Defaults for each country's `web` list in countries.R.
# levels: Kontur admin_level per tier (subdistrict levels are tried in order,
# e.g. Bangladesh's level 10 "Dhaka Metropolitan" fills gaps in level 8).
# palette: low → high; no near-white band, so it reads on the light basemap.
web_defaults <- list(
  levels = list(region = "4", district = "6", subdistrict = c("8", "10")),
  palette = c("#b5e3e6", "#7acdd3", "#3db0bd", "#1a91a8", "#1e6b98",
              "#373f80", "#572c74", "#7f1f62")
)

# Merged field by field: modifyList() would recurse into the unnamed `views`
# list and drop it.
web_settings <- function(cfg) {
  web <- if (is.null(cfg$web)) list() else cfg$web
  list(
    levels  = modifyList(web_defaults$levels, as.list(web$levels)),
    views   = web$views,
    palette = if (is.null(web$palette)) web_defaults$palette else web$palette,
    crop    = web$crop,
    bounds  = web$bounds,
    land_km2 = web$land_km2
  )
}

slugify <- function(x) gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(x)))

bbox_vec <- function(geom) {
  b <- st_bbox(st_transform(geom, 4326))
  round(unname(c(b["xmin"], b["ymin"], b["xmax"], b["ymax"])), 3)
}

# Admin polygons at level 2 (the country) and the tiers in `levels` only:
# the full files run to hundreds of MB (France: 500), mostly finer levels
# the explorer never uses.
read_admin <- function(file, levels) {
  layer <- st_layers(file)$name[1]
  wanted <- paste0("'", unique(c("2", unlist(levels))), "'", collapse = ", ")
  st_read(file, quiet = TRUE, query = sprintf(
    'SELECT * FROM "%s" WHERE CAST(admin_level AS TEXT) IN (%s)', layer, wanted
  ))
}

# Lon/lat box (west, south, east, north) as a polygon in `crs`.
bounds_box <- function(bounds, crs) {
  box <- st_bbox(setNames(bounds, c("xmin", "ymin", "xmax", "ymax")),
                 crs = 4326)
  st_transform(st_as_sfc(box), crs)
}

# Hexagons (by centroid, so none are cut in half) and admin polygons inside
# `bounds`, for countries whose files reach far beyond the area worth mapping:
# France's overseas territories span the globe. Planar maths: s2 rejects some
# Kontur polygons, and the box edges should follow lon/lat lines anyway.
crop_country <- function(hex, admin, bounds) {
  old <- suppressMessages(sf_use_s2(FALSE))
  on.exit(suppressMessages(sf_use_s2(old)))
  centres <- suppressWarnings(st_centroid(st_geometry(hex)))
  inside <- st_intersects(centres, bounds_box(bounds, st_crs(hex)))
  hex <- hex[lengths(inside) > 0, ]
  box <- bounds_box(bounds, st_crs(admin))
  admin <- admin[lengths(suppressMessages(st_intersects(admin, box))) > 0, ]
  admin <- suppressWarnings(suppressMessages(
    st_crop(st_make_valid(admin), st_bbox(box))
  ))
  list(hex = hex, admin = admin)
}

# English name where Kontur has one, else the local name. Some files (the
# Israel/Palestine "topology" boundaries) carry no names at all: every area
# is then NA and the country exports without regions, labels or places.
area_names <- function(admin) {
  local <- if ("name" %in% names(admin)) admin$name else NA_character_
  if ("name_en" %in% names(admin)) {
    ifelse(is.na(admin$name_en), local, admin$name_en)
  } else {
    rep_len(local, nrow(admin))
  }
}

# Polygons at the given admin levels, with a `label` name column.
admin_level <- function(admin, levels) {
  admin$label <- area_names(admin)
  admin[as.character(admin$admin_level) %in% levels & !is.na(admin$label), ]
}

# Region and district polygons in EPSG:3857 (metres, matching the land mask),
# tagged with kind = "region" | "district".
outlined_areas <- function(admin, levels) {
  areas <- admin_level(admin, c(levels$region, levels$district))
  areas$kind <- ifelse(as.character(areas$admin_level) %in% levels$region,
                       "region", "district")
  st_make_valid(st_transform(areas, 3857))
}

# Country first, then extra presets from countries.R, then regions in
# alphabetical order.
build_views <- function(cfg, web, admin) {
  # No st_union() first: same box, but slow for files with 10k+ polygons.
  bounds <- if (is.null(web$bounds)) {
    bbox_vec(st_geometry(admin))
  } else {
    round(web$bounds, 3)
  }
  country <- list(list(id = slugify(cfg$name), label = cfg$name,
                       bounds = bounds))

  extra <- lapply(web$views, function(v) {
    list(id = v$id, label = v$label, bounds = round(v$bounds, 3))
  })

  regions <- admin_level(admin, web$levels$region)
  regions <- regions[order(regions$label), ]
  region_views <- lapply(seq_len(nrow(regions)), function(i) {
    list(id = slugify(regions$label[i]), label = regions$label[i],
         bounds = bbox_vec(regions[i, ]))
  })

  views <- c(country, extra, region_views)
  # Ids are URL hashes, so a preset can't share a region's slug.
  ids <- vapply(views, `[[`, "", "id")
  if (anyDuplicated(ids)) {
    stop(cfg$name, ": duplicate view id ", ids[anyDuplicated(ids)])
  }
  views
}

# Kontur admin polygons include maritime zones, so outlines and labels are
# clipped to a land mask (EPSG:3857) made of two parts:
# - Natural Earth 1:50m land: covers unpopulated land (Bhutan's mountains,
#   Lapland) that the hexagons alone would leave out;
# - the populated hexagons (rasterised at 1 km, padded, holes filled): follow
#   the coast more closely than 1:50m where people live.
build_land_mask <- function(hex) {
  # Kontur files are EPSG:3857, but derived ones may not be (merged Israel &
  # Palestine is 3106), so project to match outlined_areas().
  hex <- st_transform(hex["population"], 3857)
  grid <- stars::st_rasterize(hex, dx = 1000, dy = 1000)
  grid[[1]][!is.na(grid[[1]])] <- 1
  populated <- st_union(st_as_sf(grid, merge = TRUE))
  populated <- st_cast(st_buffer(populated, 1500), "POLYGON")
  outer <- lapply(populated, function(p) st_polygon(p[1]))
  populated <- st_union(st_sfc(outer, crs = st_crs(populated)))

  # Crop in lon/lat before projecting: polar shapes break in Web Mercator.
  area <- st_bbox(st_transform(st_buffer(populated, 50000), 4326))
  land <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  land <- suppressWarnings(suppressMessages(
    st_crop(st_make_valid(st_geometry(land)), area)
  ))
  land <- st_union(st_transform(land, 3857))
  st_union(c(land, populated))
}

# Map labels for regions and districts, placed inside each polygon's land part.
build_labels <- function(areas, land) {
  areas <- suppressWarnings(st_intersection(areas, land))
  pts <- suppressWarnings(st_point_on_surface(areas))
  xy <- round(st_coordinates(st_transform(pts, 4326)), 4)
  lapply(seq_len(nrow(areas)), function(i) {
    list(name = areas$label[i], kind = areas$kind[i],
         position = unname(xy[i, ]))
  })
}

# Name of the polygon each point falls in, NA outside all of them.
# Earlier levels win where several match (subdistrict before metro area).
locate <- function(points, admin, levels) {
  out <- rep(NA_character_, nrow(points))
  for (lvl in levels) {
    polys <- admin_level(admin, lvl)
    hit <- st_join(points, polys["label"], join = st_within, left = TRUE)
    hit <- hit[!duplicated(hit$row_id), ]
    fill <- is.na(out) & !is.na(hit$label)
    out[fill] <- hit$label[fill]
  }
  out
}

# Tag each hexagon (by centroid) with subdistrict, district and region, and
# store the distinct combinations once so the per-hex column is just an index.
build_places <- function(hex, admin, levels) {
  admin <- st_transform(st_make_valid(admin), st_crs(hex))
  pts <- st_sf(row_id = seq_len(nrow(hex)),
               geometry = suppressWarnings(st_centroid(st_geometry(hex))))
  combos <- data.frame(
    subdistrict = locate(pts, admin, levels$subdistrict),
    district    = locate(pts, admin, levels$district),
    region      = locate(pts, admin, levels$region)
  )
  key <- do.call(paste, c(combos, sep = "|"))
  none <- rowSums(!is.na(combos)) == 0
  uniq <- unique(key[!none])
  place <- ifelse(none, -1L, match(key, uniq) - 1L)
  places <- unname(lapply(uniq, function(k) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    as.list(ifelse(parts == "NA", NA, parts))
  }))
  list(place = as.integer(place), places = places)
}

# Drop line parts that collapse to one point at the GeoJSON's coordinate
# precision: GDAL writes those as points inside a GeometryCollection, which
# deck.gl's GeoJsonLayer rejects, taking every outline in the file with it.
# Points and collections from the clipping become empty lines; untouched
# geometries are returned as they are.
drop_specks <- function(geom, precision) {
  keep <- function(m) {
    nrow(unique(round(m[, 1:2, drop = FALSE], precision))) > 1
  }
  st_sfc(lapply(geom, function(g) {
    parts <- switch(class(g)[2],
                    LINESTRING = list(unclass(g)),
                    MULTILINESTRING = unclass(g),
                    list())
    kept <- Filter(keep, parts)
    if (length(kept) == length(parts) && length(parts) > 0) return(g)
    if (length(kept) == 0) return(st_linestring())
    st_multilinestring(kept)
  }), crs = st_crs(geom))
}

write_boundaries <- function(areas, land, out_file) {
  # Outlines only, clipped to land so maritime zone edges don't cross the sea.
  areas <- st_simplify(areas, preserveTopology = TRUE,
                       dTolerance = simplify_metres)
  lines <- suppressWarnings(st_intersection(st_boundary(areas), land))
  lines <- st_transform(lines, 4326)
  st_geometry(lines) <- drop_specks(st_geometry(lines), coordinate_precision)
  lines <- lines[!st_is_empty(lines), ]
  out <- st_sf(name = lines$label, kind = lines$kind,
               geometry = st_geometry(lines))
  if (file.exists(out_file)) file.remove(out_file)
  st_write(out, out_file, driver = "GeoJSON", quiet = TRUE,
           layer_options = c(
             paste0("COORDINATE_PRECISION=", coordinate_precision),
             "RFC7946=YES"
           ))
}

# Land area for the "people per km²" figure: web$land_km2 if set, else the
# country's Natural Earth 1:50m polygons (both, for "Israel & Palestine"), cut
# to web$crop like the hexagons. Matched by name: some Kontur file names carry
# no ISO code. NA, with a warning, if nothing matches.
land_area_km2 <- function(cfg, ne) {
  web <- web_settings(cfg)
  if (!is.null(web$land_km2)) return(web$land_km2)
  parts <- strsplit(cfg$name, " & ", fixed = TRUE)[[1]]
  hit <- ne$admin %in% parts | ne$name_long %in% parts | ne$name %in% parts
  geom <- st_geometry(ne[hit, ])
  if (!length(geom)) {
    warning(cfg$name, ": no Natural Earth match, so no people per km²")
    return(NA)
  }
  if (!is.null(web$crop)) {
    # Planar crop, as in crop_country(); s2 back on for the area itself.
    old <- suppressMessages(sf_use_s2(FALSE))
    geom <- suppressWarnings(suppressMessages(
      st_crop(st_make_valid(geom), st_bbox(bounds_box(web$crop, 4326)))
    ))
    suppressMessages(sf_use_s2(old))
  }
  sum(as.numeric(st_area(geom))) / 1e6
}

# Facts for the summary card on each country's page: people per km² of land,
# where the densest hexagon is, and the most populous regions (districts if a
# country's file has no named regions). `d` is a hexes.json read with
# simplifyVector = FALSE, so a missing place name is NULL.
country_facts <- function(d, land_km2) {
  pop <- unlist(d$population)
  idx <- unlist(d$place) + 1L
  idx[idx == 0] <- NA # -1: outside every area
  tier <- function(i) {
    vapply(d$places, function(p) if (is.null(p[[i]])) NA_character_ else p[[i]],
           "")[idx]
  }
  densest <- idx[which.max(pop)]
  if (!is.na(densest)) {
    # unique(): city districts often share the city's name ("Labutta, Labutta").
    densest <- paste(unique(unlist(d$places[[densest]])), collapse = ", ")
  }
  named <- Filter(function(a) any(!is.na(a)),
                  list(region = tier(3), district = tier(2)))
  top <- if (length(named)) {
    people <- sort(tapply(pop, named[[1]], sum), decreasing = TRUE)
    lapply(head(names(people), 5), function(n) {
      list(name = n, people = unname(people[[n]]))
    })
  } else {
    list()
  }
  list(land_km2 = signif(land_km2, 3),
       per_km2 = round(signif(d$total / land_km2, 3)),
       max = d$max, max_per_km2 = signif(d$max / hex_area_km2, 2),
       densest = densest, top_kind = if (length(named)) names(named)[1] else NA,
       top = top)
}

# Index of exported countries for the search box and the country pages,
# rebuilt from the folders so a deleted folder drops out on the next export.
# Each country lists its views (minus the country itself) so search can reach
# a city or region without fetching every hexes.json; presets from
# countries.R are tagged "city".
write_manifest <- function(data_dir = web_data_dir) {
  files <- Sys.glob(file.path(data_dir, "*", "hexes.json"))
  ne <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  entries <- lapply(files, function(f) {
    d <- fromJSON(f, simplifyVector = FALSE)
    presets <- vapply(countries[[d$key]]$web$views, `[[`, "", "id")
    views <- lapply(d$views[-1], function(v) {
      list(id = v$id, label = v$label,
           kind = if (v$id %in% presets) "city" else "region")
    })
    c(list(key = d$key, name = d$name, total = d$total,
           hexagons = length(d$h3)),
      country_facts(d, land_area_km2(countries[[d$key]], ne)),
      list(views = views))
  })
  entries <- entries[order(vapply(entries, `[[`, "", "name"))]
  write_json(entries, file.path(data_dir, "countries.json"),
             auto_unbox = TRUE, pretty = TRUE, na = "null")
  message("countries.json: ", paste(vapply(entries, `[[`, "", "name"),
                                    collapse = ", "))
  invisible(entries)
}

# ---- Pages ----
# The explorer is one app, but search engines ignore URL hashes, so every
# country also gets its own page (web/{slug}/index.html) with its own title,
# description and summary. The landing page (web/index.html) targets
# "population density map" and shows the default country. All of them come
# from one template; web/app.js renders the same summary and stat line when
# the visitor switches country, so keep summary_html() and stat_line() in
# step with renderSummary() and setCountry().

site_url <- "https://niloy.tech/Population-Density-Map/"
page_template <- "scripts/templates/explorer.html"
# Shown by the landing page; must match DEFAULT_COUNTRY in web/app.js.
default_country <- "bangladesh"
# Page folder for a country key: hyphens, since search engines read "_" as
# part of a word ("sri_lanka").
page_slug <- function(key) gsub("_", "-", key)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

fmt_int <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Like Intl.NumberFormat('en', { notation: 'compact' }) in app.js: 173.9M.
fmt_short <- function(x) {
  units <- c(B = 1e9, M = 1e6, K = 1e3)
  unit <- units[x >= units][1]
  if (is.na(unit)) return(fmt_int(x))
  number <- formatC(x / unit, format = "f", digits = 1)
  paste0(sub("\\.0$", "", number), names(unit))
}

fmt_words <- function(x) {
  if (x < 1e6) return(fmt_int(x))
  paste(formatC(x / 1e6, format = "f", digits = 1), "million")
}

# "A", "A and B", "A, B and C"
join_and <- function(x) {
  if (length(x) < 2) return(x)
  paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))
}

# Under the country name; same as setCountry() in app.js.
stat_line <- function(e) {
  sprintf("%s people · %s", fmt_short(e$total), if (is.na(e$per_km2)) {
    paste(fmt_short(e$hexagons), "hexagons")
  } else {
    paste(fmt_int(e$per_km2), "per km²")
  })
}

# Inside <details data-summary>; same markup as renderSummary() in app.js.
summary_html <- function(e) {
  density <- if (is.na(e$per_km2)) {
    ""
  } else {
    sprintf(" on about %s km² of land, roughly %s per km²",
            fmt_int(e$land_km2), fmt_int(e$per_km2))
  }
  densest <- if (is.na(e$densest)) "" else paste0(", in ", e$densest, ",")
  text <- sprintf(paste0(
    "%s has %s people%s. They live in %s populated hexagons; the densest%s ",
    "holds %s people, about %s per km²."
  ), e$name, fmt_int(e$total), density, fmt_int(e$hexagons), densest,
  fmt_int(e$max), fmt_int(e$max_per_km2))
  out <- c(
    sprintf('<summary class="summary__toggle">%s in numbers</summary>',
            html_escape(e$name)),
    sprintf('<p class="summary__text">%s</p>', html_escape(text))
  )
  if (length(e$top)) {
    items <- vapply(e$top, function(a) {
      share <- round(100 * a$people / e$total)
      sprintf(paste0('<li><span class="summary__name">%s</span>',
                     '<span class="summary__value">%s · %s</span></li>'),
              html_escape(a$name), fmt_short(a$people),
              if (share < 1) "&lt;1%" else paste0(share, "%"))
    }, "")
    out <- c(out,
             sprintf('<p class="summary__title">Most populous %ss</p>',
                     e$top_kind),
             '<ol class="summary__list">', items, "</ol>")
  }
  paste0("                        ", out, collapse = "\n")
}

# Plain links so crawlers can reach every country page; app.js makes them
# switch country without a reload.
country_links <- function(entries, root) {
  vapply(entries, function(e) {
    sprintf('%s<li><a href="%s%s/" data-country="%s">%s</a></li>',
            strrep(" ", 28), root, page_slug(e$key), e$key,
            html_escape(e$name))
  }, "") |> paste(collapse = "\n")
}

json_ld <- function(name, url, description, extra = list()) {
  c(list(
    "@context" = "https://schema.org",
    "@type" = "WebApplication",
    name = name,
    url = url,
    description = description,
    image = paste0(site_url, "og.jpg"),
    applicationCategory = "ReferenceApplication",
    operatingSystem = "Any",
    browserRequirements = "Requires JavaScript and WebGL",
    isAccessibleForFree = TRUE,
    offers = list("@type" = "Offer", price = "0", priceCurrency = "USD")
  ), extra, list(
    creator = list("@type" = "Person", name = "Niloy Biswas",
                   url = "https://niloy.tech/"),
    # Google lists this as a Dataset item of its own, so it needs the
    # Dataset required fields (description: 50 to 5000 characters).
    isBasedOn = list(
      "@type" = "Dataset",
      name = "Kontur Population Dataset (2023)",
      description = paste(
        "Global population counts on H3 hexagons at resolution 8",
        "(about 400 m across, 0.74 km² each), built by Kontur from GHSL,",
        "Meta HRSL, Microsoft building footprints, Copernicus land cover",
        "and OpenStreetMap. Released on the Humanitarian Data Exchange",
        "with per-country subsets."
      ),
      url = "https://data.humdata.org/dataset/kontur-population-dataset",
      creator = list("@type" = "Organization", name = "Kontur"),
      license = "https://creativecommons.org/licenses/by/4.0/"
    ),
    sameAs = "https://github.com/niloy-biswas/Population-Density-Map"
  ))
}

# niloy.tech › Population Density Map › <Country>, shown by Google in place
# of the raw URL.
breadcrumbs <- function(name, url) {
  crumbs <- list(c("niloy.tech", "https://niloy.tech/"),
                 c("Population Density Map", site_url), c(name, url))
  list(
    "@context" = "https://schema.org",
    "@type" = "BreadcrumbList",
    itemListElement = lapply(seq_along(crumbs), function(i) {
      list("@type" = "ListItem", position = i, name = crumbs[[i]][1],
           item = crumbs[[i]][2])
    })
  )
}

# One object, or an array of them, for the <script type="application/ld+json">.
ld_script <- function(...) {
  objects <- list(...)
  json <- toJSON(if (length(objects) == 1) objects[[1]] else objects,
                 auto_unbox = TRUE, pretty = 4)
  # "</" would end the <script> block early.
  gsub("</", "<\\/", json, fixed = TRUE)
}

fill_template <- function(template, values) {
  for (field in names(values)) {
    template <- gsub(paste0("{{", field, "}}"), values[[field]], template,
                     fixed = TRUE)
  }
  if (grepl("{{", template, fixed = TRUE)) {
    stop(page_template, " has a placeholder with no value")
  }
  template
}

page_html <- function(template, entries, e, landing) {
  root <- if (landing) "" else "../"
  country_names <- vapply(entries, `[[`, "", "name")
  # "Israel & Palestine" is a combined map, not another country.
  n_countries <- sum(!grepl(" & ", country_names, fixed = TRUE))
  if (landing) {
    title <- sprintf(
      "Population Density Map: Interactive 3D Map of %d Countries",
      n_countries
    )
    url <- site_url
    description <- sprintf(paste0(
      "Population density map of %d countries in interactive 3D: ",
      "Bangladesh, Pakistan, Japan, the UK, Germany, France and more. ",
      "Hover any ~0.74 km² hexagon for its population."
    ), n_countries)
    ld <- ld_script(json_ld("3D Population Density Map", url, paste0(
      "Interactive 3D population density maps built from Kontur H3 ",
      "hexagons (resolution 8, about 0.74 km² each). Covers ",
      join_and(country_names), "."
    ), list(keywords = paste(c(
      "population density map", "3D population map", "H3 hexagons",
      "Kontur", country_names
    ), collapse = ", "))))
  } else {
    title <- sprintf("%s Population Density Map (Interactive 3D)", e$name)
    url <- paste0(site_url, page_slug(e$key), "/")
    areas <- vapply(head(e$top, 3), `[[`, "", "name")
    density <- if (is.na(e$per_km2)) {
      sprintf(" in %s hexagons of ~0.74 km²", fmt_int(e$hexagons))
    } else {
      sprintf(", about %s per km²", fmt_int(e$per_km2))
    }
    description <- paste0(
      sprintf("Interactive 3D population density map of %s: %s people%s ",
              e$name, fmt_words(e$total), density),
      "(Kontur 2023).",
      if (length(areas)) {
        sprintf(" Explore %s and more.", paste(areas, collapse = ", "))
      }
    )
    ld <- ld_script(
      json_ld(paste(e$name, "Population Density Map"), url, description,
              list(spatialCoverage = list("@type" = "Place", name = e$name))),
      breadcrumbs(e$name, url)
    )
  }
  fill_template(template, list(
    title = html_escape(title),
    description = html_escape(description),
    url = url,
    root = root,
    json_ld = ld,
    country_links = country_links(entries, root),
    map_label = html_escape(paste(
      "Interactive 3D population density map of", e$name
    )),
    name = html_escape(e$name),
    stat = stat_line(e),
    summary = summary_html(e),
    expand_label = html_escape(sprintf("%s · %s people", e$name,
                                       fmt_short(e$total)))
  ))
}

# Writes `file` only if its content changes; TRUE if it did (for lastmod).
write_page <- function(html, file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(tmpdir = dirname(file))
  con <- file(tmp, "wb")
  writeLines(enc2utf8(html), con, useBytes = TRUE)
  close(con)
  if (file.exists(file) && tools::md5sum(tmp) == tools::md5sum(file)) {
    unlink(tmp)
    return(FALSE)
  }
  file.rename(tmp, file)
}

# lastmod is the day a page's content last changed. Unchanged pages keep the
# date from the previous sitemap, so a re-run doesn't redate every page and
# search engines can keep trusting it.
write_sitemap <- function(urls, changed, file) {
  old <- if (file.exists(file)) readLines(file) else character(0)
  old <- regmatches(old, regexec("<loc>(.+)</loc><lastmod>(.+)</lastmod>",
                                 old))
  old <- Filter(length, old)
  previous <- setNames(vapply(old, `[`, "", 3), vapply(old, `[`, "", 2))
  lastmod <- unname(ifelse(changed | is.na(previous[urls]),
                           format(Sys.Date()), previous[urls]))
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    sprintf("  <url><loc>%s</loc><lastmod>%s</lastmod></url>", urls, lastmod),
    "</urlset>"
  ), file)
}

# The landing page plus one page per country, and the sitemap listing them.
# Folders of countries no longer exported are removed, but only if they hold
# nothing besides index.html.
write_pages <- function(entries, site_dir = dirname(web_data_dir)) {
  template <- paste(readLines(page_template, encoding = "UTF-8"),
                    collapse = "\n")
  keys <- vapply(entries, `[[`, "", "key")
  landing <- entries[[match(default_country, keys, nomatch = 1)]]
  changed <- c(
    write_page(page_html(template, entries, landing, TRUE),
               file.path(site_dir, "index.html")),
    vapply(entries, function(e) {
      write_page(page_html(template, entries, e, FALSE),
                 file.path(site_dir, page_slug(e$key), "index.html"))
    }, TRUE)
  )
  write_sitemap(c(site_url, paste0(site_url, page_slug(keys), "/")), changed,
                file.path(site_dir, "sitemap.xml"))
  for (dir in list.dirs(site_dir, recursive = FALSE)) {
    if (basename(dir) %in% c(page_slug(keys), basename(web_data_dir))) next
    if (identical(list.files(dir, all.files = TRUE, no.. = TRUE),
                  "index.html")) {
      unlink(dir, recursive = TRUE)
      message("Removed stale page ", dir)
    }
  }
  message("Pages: index.html + ", length(entries), " countries (",
          sum(changed), " changed) + sitemap.xml")
}

export_web_data <- function(country_key, data_dir = web_data_dir) {
  cfg <- countries[[country_key]]
  if (is.null(cfg)) stop("Unknown country key: ", country_key)
  web <- web_settings(cfg)

  admin <- read_admin(cfg$admin_file, web$levels)
  hex <- st_read(cfg$hex_file, quiet = TRUE)
  if (!all(c("h3", "population") %in% names(hex))) {
    stop(cfg$hex_file, " has no h3/population columns")
  }
  hex <- hex[hex$population > 0, ]
  if (!is.null(web$crop)) {
    cropped <- crop_country(hex, admin, web$crop)
    hex <- cropped$hex
    admin <- cropped$admin
  }
  # Tallest columns last, so they draw over their neighbours at low pitch.
  hex <- hex[order(hex$population), ]

  places <- build_places(hex, admin, web$levels)
  land <- build_land_mask(hex)
  areas <- outlined_areas(admin, web$levels)

  out <- list(
    key        = country_key,
    name       = cfg$name,
    resolution = 8L,
    total      = round(sum(hex$population)),
    max        = max(hex$population),
    palette    = web$palette,
    views      = build_views(cfg, web, admin),
    labels     = build_labels(areas, land),
    places     = places$places,
    h3         = hex$h3,
    population = as.integer(round(hex$population)),
    place      = places$place
  )

  out_dir <- file.path(data_dir, country_key)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  hexes_file <- file.path(out_dir, "hexes.json")
  write_json(out, hexes_file, auto_unbox = TRUE, digits = NA, na = "null")
  boundaries_file <- file.path(out_dir, "boundaries.geojson")
  write_boundaries(areas, land, boundaries_file)

  message(sprintf(
    "%s: %d hexes (%d untagged), %d places, %d views -> %s (%.1f MB)",
    cfg$name, nrow(hex), sum(places$place < 0), length(places$places),
    length(out$views), out_dir,
    (file.size(hexes_file) + file.size(boundaries_file)) / 1e6
  ))
  invisible(out_dir)
}

keys <- commandArgs(trailingOnly = TRUE)
if (length(keys) == 0) keys <- "bangladesh"
# --manifest: only rebuild countries.json and the pages from the existing
# folders.
for (k in setdiff(keys, "--manifest")) export_web_data(k)
write_pages(write_manifest())
