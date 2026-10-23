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
# Then rebuilds web/data/countries.json, the index behind the country picker,
# from every exported country folder.
#
# Usage (from repo root):
#   Rscript scripts/export_web_data.R bangladesh
#   Rscript scripts/export_web_data.R bangladesh nepal

library(sf)
library(jsonlite)

source("./scripts/countries.R")

web_data_dir <- "web/data"
simplify_metres <- 150

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
    palette = if (is.null(web$palette)) web_defaults$palette else web$palette
  )
}

slugify <- function(x) gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(x)))

bbox_vec <- function(geom) {
  b <- st_bbox(st_transform(geom, 4326))
  round(unname(c(b["xmin"], b["ymin"], b["xmax"], b["ymax"])), 3)
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
  country <- list(list(id = slugify(cfg$name), label = cfg$name,
                       bounds = bbox_vec(st_union(admin))))

  extra <- lapply(web$views, function(v) {
    list(id = v$id, label = v$label, bounds = round(v$bounds, 3))
  })

  regions <- admin_level(admin, web$levels$region)
  regions <- regions[order(regions$label), ]
  region_views <- lapply(seq_len(nrow(regions)), function(i) {
    list(id = slugify(regions$label[i]), label = regions$label[i],
         bounds = bbox_vec(regions[i, ]))
  })

  c(country, extra, region_views)
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

write_boundaries <- function(areas, land, out_file) {
  # Outlines only, clipped to land so maritime zone edges don't cross the sea.
  areas <- st_simplify(areas, preserveTopology = TRUE,
                       dTolerance = simplify_metres)
  lines <- suppressWarnings(st_intersection(st_boundary(areas), land))
  lines <- st_transform(lines[!st_is_empty(lines), ], 4326)
  out <- st_sf(name = lines$label, kind = lines$kind,
               geometry = st_geometry(lines))
  if (file.exists(out_file)) file.remove(out_file)
  st_write(out, out_file, driver = "GeoJSON", quiet = TRUE,
           layer_options = c("COORDINATE_PRECISION=4", "RFC7946=YES"))
}

# Index of exported countries for the picker, rebuilt from the folders so a
# deleted folder drops out on the next export.
write_manifest <- function(data_dir = web_data_dir) {
  files <- Sys.glob(file.path(data_dir, "*", "hexes.json"))
  entries <- lapply(files, function(f) {
    d <- fromJSON(f, simplifyVector = FALSE)
    list(key = d$key, name = d$name, total = d$total,
         hexagons = length(d$h3))
  })
  entries <- entries[order(vapply(entries, `[[`, "", "name"))]
  write_json(entries, file.path(data_dir, "countries.json"),
             auto_unbox = TRUE, pretty = TRUE)
  message("countries.json: ", paste(vapply(entries, `[[`, "", "name"),
                                    collapse = ", "))
}

export_web_data <- function(country_key, data_dir = web_data_dir) {
  cfg <- countries[[country_key]]
  if (is.null(cfg)) stop("Unknown country key: ", country_key)
  web <- web_settings(cfg)

  admin <- st_read(cfg$admin_file, quiet = TRUE)
  hex <- st_read(cfg$hex_file, quiet = TRUE)
  if (!all(c("h3", "population") %in% names(hex))) {
    stop(cfg$hex_file, " has no h3/population columns")
  }
  hex <- hex[hex$population > 0, ]
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
for (k in keys) export_web_data(k)
write_manifest()
