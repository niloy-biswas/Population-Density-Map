# AGENTS.md

Quick reference for AI agents working in this repo. Read this before exploring; it should save you most of the reading.

## What this repo is

3D population density maps from [Kontur](https://data.humdata.org/organization/kontur) H3 hexagon data (resolution 8, ~0.74 km² per hexagon), in two independent outputs that share one config:

| Output | Tech | Entry point | Result |
|--------|------|-------------|--------|
| Rendered maps | R + rayshader (ray-traced PNGs) | `run_country.R` | `output/plots/`, `output/annotated/` |
| Web explorer | Static HTML/JS, deck.gl + MapLibre | `web/index.html` (generated) | Live at https://niloy.tech/Population-Density-Map/ |

Both read `scripts/countries.R`. Making a rendered map never touches `web/`; the explorer never needs rayshader.

The repo is linked from a Medium tutorial, so the R scripts must keep working at their current paths.

## Layout

```
run_country.R                 # Rendered maps: run_country("bangladesh")
scripts/
  countries.R                 # SINGLE CONFIG: one list entry per country (both outputs)
  helpers.R                   # create_palette() (rayshader only), safe_image_read()
  create_pop_matrix.R         # hexagons → raster matrix
  plot_population_3d.R        # rayshader plot_3d + render_highquality
  annotate_population_map.R   # magick title/credits on the PNG
  check_boundary_plot.R       # (see Known issues: currently a no-op)
  export_web_data.R           # Web data + page generator (all of web/data and the pages)
  templates/explorer.html     # Template for web/index.html and every country page
  download_kontur.R           # Fetch Kontur files by ISO code → data/kontur/
  setup_packages.R            # install.packages for everything
  legacy/                     # Old per-country scripts. Reference only, don't edit
data/                         # Kontur .gpkg inputs (~218 MB, committed). Lowercase `data/`
  kontur/                     # Downloaded by download_kontur.R. GITIGNORED (~3 GB)
output/                       # Rendered images (README gallery). Committed
web/
  app.js, style.css, og.jpg, apple-touch-icon.png
  index.html                  # GENERATED: landing page ("population density map", shows Bangladesh)
  <slug>/index.html           # GENERATED: one page per country (slug = key with hyphens: sri-lanka)
  sitemap.xml                 # GENERATED: every page + the day its content last changed
  data/countries.json         # GENERATED: search index + summary facts (countries, views, per km², top regions)
  data/<key>/hexes.json       # GENERATED: hexagons, palette, views, labels, places
  data/<key>/tiles/z/x/y.json # GENERATED, tiled countries only (US, China, India): the hexagons
  data/<key>/boundaries.geojson  # GENERATED: region/district outlines
.github/workflows/pages.yml   # Deploys web/ only, on push to main touching web/**
```

Never hand-edit `web/data/` or any `web/**/index.html`. Edit `scripts/templates/explorer.html` (or the page functions in the exporter) and regenerate with `--manifest`. The pages repeat the same markup on purpose (see Gotchas); don't try to deduplicate them.

## Commands

```bash
# Rendered map (slow: 500 ray samples by default; pauses at a readline prompt)
Rscript -e 'source("run_country.R"); run_country("bangladesh")'

# Fetch Kontur files (population + boundaries) by ISO code; skips existing
Rscript scripts/download_kontur.R GB DE

# Web data for one or more countries (also rebuilds web/data/countries.json)
Rscript scripts/export_web_data.R bangladesh nepal
Rscript scripts/export_web_data.R --manifest   # only rebuild countries.json + pages (~7 s)

# Preview the explorer
python3 -m http.server 8000 -d web      # → http://localhost:8000/bangladesh/

# Quick checks (no test suite exists)
node --check web/app.js
Rscript -e 'lintr::lint("scripts/export_web_data.R")'
```

Deploy = push to `main`. GitHub Pages source is "GitHub Actions" (already enabled). The repo is served under the owner's user-site custom domain, hence `niloy.tech/Population-Density-Map/`.

## Config: `scripts/countries.R`

Each entry is `key = list(...)`. The rayshader fields (`crs`, `size`, `palette`, `zscale`, `camera`, `light_dir`, `width`, `height`, `solid`, `shadow_depth`, …) are documented in the file's header comment. The web exporter only reads `name`, `hex_file`, `admin_file` and the optional `web` list:

```r
web = list(
  levels  = list(region = "4", district = "6", subdistrict = c("8", "10")),  # Kontur admin_level per tier
  views   = list(list(id = "dhaka-city", label = "Dhaka city", bounds = c(w, s, e, n))),
  palette = c("#b5e3e6", ...),  # low → high; default is the shared teal ramp
  crop    = c(w, s, e, n),       # drop hexagons/areas outside (France, NL, Norway, UK)
  bounds  = c(w, s, e, n),       # country view only; islands kept (Spain, Portugal, Japan)
  land_km2 = 735,                # land area override for people per km² (Maldives, Singapore, Canada)
  mask_km  = 2,                  # land-mask raster cell, default 1 (Canada 3; US, China, India 2)
  simplify_m = 300,              # outline simplification, default 150 m (US 400; China, India 300)
  tiles    = TRUE                # tile pyramid instead of one hexes.json array (US, China, India)
)
```

- The 24 countries added in the second batch (Europe, Japan, Pakistan, Nigeria, Singapore, ...) are **web-only**: just `name`/`hex_file`/`admin_file`/`web`, with files in `data/kontur/`. `run_country()` stops with a clear error for them until rayshader fields are added.

- Canada, China and the US are web-only too. India has both: its rayshader entry now points at `data/kontur/`.
- Defaults (`web_defaults` in `export_web_data.R`) are Bangladesh's levels and the teal palette.
- `character(0)` means "this tier doesn't exist in the file".
- Admin levels **differ per country**. Check before adding one: `table(sf::st_read(admin_file)$admin_level)`. Current values are in the README table and in each entry.

## Web explorer architecture (`web/app.js`)

- **No build step.** Libraries load from jsDelivr with pinned versions + SRI hashes in `index.html`: MapLibre 5.24.0, h3-js 4.5.0, deck.gl 9.4.0. If you bump a version, recompute its `integrity`: `curl -s URL | openssl dgst -sha384 -binary | openssl base64 -A`. MapLibre 6 is ESM-only (no UMD file to pin with SRI), which is why it stays on 5.x.
- **Boot:** fetch `data/countries.json` → `buildSearchIndex()` → `initMap()` (once) → `loadCountry(key, viewId)`. A `loadToken` guards against out-of-order responses when switching quickly.
- **UI (Google Maps style):** top-left column (`.panel`, used by `framePadding()`) holds the search combobox, a scrolling chip row of the country's views, and a summary card with the legend. The card's chevron hides chips + card (`.panel__details`) behind a "<Country> · N people" pill; open by default, remembered in `localStorage` (`pdm-panel-collapsed`). The hamburger opens an About popover. Bottom-right, `.view-ctrl` is added as a MapLibre control above zoom/compass: a layers popover (column height, tilt, rotate, reset) and a 3D/flat toggle. Defaults: height 1.5×, every view opens at `DEFAULT_PITCH` = 30°.
- **Search** (`buildSearchIndex`, `searchPlaces`): matches countries + manifest views, accent-insensitive; ranks name-start, then word-start, then mid-word, then country > city > region, then shorter names. Until the visitor types, the box shows the current place and the list shows all countries (it doubles as the country dropdown). Picking a view of another country calls `loadCountry(key, viewId)`.
- **URLs:** `<slug>/` or `<slug>/#<viewId>`, one real page per country so search engines index each (they ignore hashes). `ROOT` (the folder of `app.js`) resolves `data/` and links from any page, so the same JS runs at `/` locally and `/Population-Density-Map/` live. The landing page shows `DEFAULT_COUNTRY` (Bangladesh, matching `og.jpg`; first country if it is ever removed) and keeps its URL and title until the visitor goes elsewhere; `syncUrl()` then `replaceState`s to the country page and sets its title and canonical. Old `#<key>` / `#<key>/<viewId>` links on the landing page redirect. Don't rely on manifest order: it is alphabetical, so Austria became the landing page by accident once.
- **Country pages** share one template; the exporter fills in title, description, canonical, JSON-LD (`WebApplication`, plus a `BreadcrumbList` on country pages), the h1 (eyebrow + country name, so keep the eyebrow visible on phones: Google indexes the mobile layout) and stat, and a closed `<details>` "<Country> in numbers" summary (from `countries.json` facts). `renderSummary()` in `app.js` rebuilds that summary on country switch, so keep it in step with `summary_html()` in the exporter, and the page title with `pageTitle()`. The About menu's country list is plain `<a>` links (crawlable); `bindCountryLinks()` turns clicks into in-place switches and makes the hrefs absolute.
- **Layers** (deck `MapboxOverlay`, `interleaved: true`):
  - `H3HexagonLayer` columns, inserted `beforeId` the basemap's first symbol layer so city names stay on top
  - `GeoJsonLayer` outlines, drawn with `depthCompare: 'always'` so they show over the columns, inserted `beforeId` our label layers
- **Labels are MapLibre symbol layers** (`pdm-labels-region` above `pdm-labels-district`, one GeoJSON source), added on load and refilled per country by `labelFeatures()`. MapLibre's collision detection thins them like the basemap's own names; `symbol-sort-key` ranks each label by the people in its area (summed from the hexes; a tiled country's labels carry `people` from the exporter), so bigger areas win. The region layer sits above, so regions are placed before districts. They are styled like the basemap's own place names (Noto Sans Regular, black, 1 px blurred white halo, normal case); regions are just 2 px larger.
- **`renderLayers(animate)`** rebuilds the columns and outlines. Nothing rebuilds on zoom.
- **Tiled countries** (`country.tiles`, see `---- Tiles ----`): `tileLayer()` is a deck `TileLayer` whose `getTileData` (`fetchTile`) loads `data/<key>/tiles/z/x/y.json` and whose sub-layers are `hexLayer()`s. Levels: res 6 from map zoom 3, res 7 from 6, res 8 from 8; a level's files are cut at its top zoom, so lower zooms fetch child tiles (`tiles.index` lists the files that exist, so the sea is never requested). `refinementStrategy: 'best-available'` keeps coarse tiles up while finer ones load; `extent` keeps showing zoom-3 tiles when zoomed further out. `TileLayer` loads tiles on viewport change by itself, which is not the rebuild the zoom gotcha warns about. Sub-layers re-render through `updateTriggers.renderSubLayers` (palette, height, view). h3 ids are stored without the shared "8"+res prefix and trailing "f"s; `fetchTile` restores them.
- **Column height** = `diagonal(view.bounds) × 0.07 / max × slider`, so city views get shorter columns; `max` is `country.max`, or the tile level's `tiles.max[res]`, so the tallest column keeps its height across levels. **Colour** = palette at `sqrt(pop / max)`, because the data is heavily skewed. The legend (`renderLegend()`) follows the tile level in view (`tileRes`, set by `onViewportLoad`): "People per hexagon (~36 km²)" at res 6. The palette is the visitor's pick from `PALETTES` in `app.js` (Colours in the layers popover, remembered as `pdm-palette`): "Teal" is the country's own `hexes.json` palette, the other three copy the Bangladesh, India and Nepal renders (`create_palette()` output for their `countries.R` palettes). `applyPalette()` sets `palette` (RGB) for the `getFillColor` accessor, so the hex layer needs `updateTriggers.getFillColor`.
- **Camera framing** (`cameraFor`) uses its own Web Mercator maths plus panel-aware `padding`.
- **Basemap:** OpenFreeMap `bright`. On load, label `text-field`s are forced to English, and `label_state`, road shields and POI layers are hidden.
- `style.css` holds design tokens mirroring niloy.tech (Inter/Fraunces, teal accent `#0bceaf`, dark glass panel).

## Exporter pipeline (`scripts/export_web_data.R`)

Per country: read hexagons + admin polygons (`read_admin()` loads only level 2 and the configured tiers via an SQL query; the full files reach 500 MB), apply `web$crop` if set (`crop_country()`), then:

1. **`build_places()`** tags each hexagon centroid with subdistrict/district/region (`locate()`: `st_intersects(polygons, points)`, ~30× faster than `st_join(points, polygons)`). Distinct combinations are stored once in `places`; the per-hex `place` column is an index (-1 = none).
2. **`build_land_mask()`** builds Natural Earth 1:50m land (via `rnaturalearth`) ∪ the populated hexagons (`web$mask_km` raster, default 1 km, 1.5 cells buffer, holes filled), in EPSG:3857.
3. **`outlined_areas()`** gives the region + district polygons, tagged `kind = "region" | "district"`. Outlines and labels are clipped to the land mask.
4. **`build_views()`** produces the country, then `web$views`, then regions alphabetically.
   - **Tiled countries** (`web$tiles`): `read_hex_points()` reads only `h3` + `population` and makes centre points with `h3jsr` (polygons would exhaust memory: Canada's 600k peaked at 3.4 GB). `write_tiles()` sums cells into parents per `tile_levels` (`aggregate_cells()`; a parent takes its most populous child's place), cuts each level into XYZ tiles by cell centre and returns the `tiles` manifest. `hexes.json` then has no hex arrays; it carries `facts` (from `hex_facts()`) and labels with `people` (`area_people()`). The US takes ~30 min and writes ~1,300 tiles (~95 MB).
5. **`write_manifest()`** rescans `web/data/*/hexes.json` to rebuild `countries.json`, including each country's views (`id`, `label`, `kind` = `"city"` for `web$views` presets, else `"region"`) for search and `country_facts()` for the summary and stat line (from the hex arrays via `hex_facts()`, or a tiled country's stored `facts`): people per km² of land, the densest hexagon's place, top 5 regions (districts if there are no named regions). Land area comes from `land_area_km2()`: Natural Earth 1:50m matched by **name** (Bangladesh's and Sri Lanka's file names carry no ISO code; "Israel & Palestine" sums both), cut to `web$crop`. It is within a few % of official figures except for microstates, where 1:50m drops most of the land, and Canada, where it counts the lakes (9.87M vs 8.79M km²); those set `web$land_km2`. Ukraine comes out ~6% under the official figure (Natural Earth likely leaves out Crimea), left as is. Check a new country's figure against its official land area.
6. **`write_pages()`** fills `scripts/templates/explorer.html` into `web/index.html` and `web/<slug>/index.html`, writes `web/sitemap.xml`, and removes page folders of countries no longer exported. To remove a country, delete its `web/data/` folder and re-run any export.
   - `write_page()` only rewrites a page whose content changed, and `write_sitemap()` keeps the previous `lastmod` for unchanged pages, so re-running the export doesn't redate everything. Don't switch it to "today" for all pages: search engines stop trusting a `lastmod` that always changes.
   - A page whose only change is the About-menu country list (every page, whenever a country is added) keeps its `lastmod` too: `write_page()` compares pages without their `data-country` lines (`own_content()`).
   - Prose uses `prose_name` from `countries.json` ("the United States"; the list is `names_with_the` in the exporter), in page descriptions, `summary_html()` and `renderSummary()`. Populations of a billion or more read "1.43 billion" (`fmt_words()`).
   - The sitemap lives under the project path, so search engines only find it via Search Console or a `Sitemap:` line in niloy.tech's `robots.txt` (the user-site repo, not this one).

The JS only knows `kind`, never admin-level numbers. Keep it that way.

## Gotchas (learned the hard way)

- **`modifyList()` drops unnamed lists.** That's why `web_settings()` merges `levels`/`views`/`palette` field by field. Don't "simplify" it back.
- **Natural Earth must be cropped in lon/lat before `st_transform(3857)`.** Polar shapes throw GEOS TopologyExceptions otherwise.
- **Not every hexagon file is EPSG:3857.** `data/merged_population_20231101.gpkg` (Israel & Palestine, built by `legacy/IS&PL.R`) is EPSG:3106. The land mask transforms explicitly.
- **The Israel, Palestine and Israel & Palestine boundary files have no name columns** (only `id`, `admin_level`, `maritime`). They export with hexagons + country view only. This is expected, not a bug.
- **`name_en` can be NA.** `area_names()` falls back to `name`, then to NA.
- **deck.gl `CollisionFilterExtension` hides every label in interleaved mode.** It was tried and reverted; that's why labels moved to MapLibre symbol layers instead of a deck `TextLayer`. Don't move them back.
- **No `mask-image` on the chip row.** A masked parent breaks the chips' `backdrop-filter` and paints a pale box behind the whole row.
- **Popovers inside the search pill use `--glass-solid`, not blur.** A `backdrop-filter` nested in another one can't see the chips/card behind it, so a translucent list showed them through. `.search` also needs `z-index: 1`: each blurred piece is its own stacking context, and later siblings would paint over the results.
- **Don't rebuild the column layer on zoom events.** Rebuilding mid-`flyTo` cancels the elevation transition and leaves columns at the previous view's height.
- **`map.cameraForBounds()` factors in the current pitch,** so presets framed inconsistently. `cameraFor()` replaces it; don't switch back.
- **The landing page's country count skips names with " & "** ("Israel & Palestine" is a combined map), and its description names a few countries by hand in `page_html()`.
- **The ~40 generated pages repeat the same search box, controls and country list on purpose.** The single source is `scripts/templates/explorer.html`; the copies are build output, committed like `web/data/`. Options weighed and rejected:
  - Building the country list in JS: it is the crawlable link to every country page, so it has to be in the static HTML.
  - Building the search box and controls in JS: no SEO gain, adds JS and an empty-UI flash on load.
  - Server-side includes: GitHub Pages has none.
  - Generating the pages in CI and gitignoring them: the deploy would need R, plus the page code split out of the exporter (which loads `sf`), to save only diff noise.

  The cost is that one template edit shows up as ~40 changed files; commit those in the data batch.
- **District labels** appear at the country's default zoom + 1.2 (a relative threshold, set per country with `map.setLayerZoomRange()`), not at a fixed zoom. Past that, collision decides which show.
- **Button and label names drop admin suffixes** (`ADMIN_SUFFIX` in `app.js`: "Division", "Prefecture", "State", ...). A city preset named like a region then shows two identical buttons ("Lagos" vs "Lagos State"), so presets are called "<City> city".
- **Case sensitivity:** the folder is `data/` (lowercase). It was once tracked as `Data/` and broke Linux/CI. Keep paths lowercase.
- **`data/kontur/` is gitignored.** A fresh clone has none of the newer countries' inputs; re-run `download_kontur.R` with the ISO codes from the `hex_file` paths before exporting them. Don't commit those files: France's boundaries are 500 MB.
- **Overseas territories wreck the country view and the land mask.** France's hexagon file spans lon -178 to 168; the 1 km land-mask raster over that extent would exhaust memory. Check a new country's hexagon extent; if it reaches far beyond the mainland, set `web$crop`. Spain (Canaries), Portugal (Azores) and Japan are wide but kept whole on purpose. The US file also spans the globe (Guam, the far Aleutians west of 180°); its crop keeps lon -180 to -64, north of 17°N.
- **High latitudes blow up the Web Mercator land mask and the first view.** Canada reaches 83°N: a 1 km grid there runs to 100M+ cells (hence `mask_km = 3`), and framing the Arctic islands pushes the south, where almost everyone lives, off screen (hence `bounds`).
- **GDAL turns tiny outline pieces into GeometryCollections.** With `COORDINATE_PRECISION=4` (~11 m), a short clipped segment collapses to one point and GDAL writes it as a MultiPoint inside a GeometryCollection. deck.gl's GeoJsonLayer then rejects the whole `boundaries.geojson` (no outlines at all), and logs it only once per session, so a multi-country browser run under-reports it. `drop_specks()` removes those parts before writing; keep it, and check exported geometry types are only LineString/MultiLineString.
- **View ids are URL hashes and must be unique.** A preset named after a region (Japan's "Tokyo" preset vs Tokyo Prefecture) collides; `build_views()` now stops on duplicates.
- **Don't edit `export_web_data.R` while an export is running.** Rscript reads the file as it goes, so an edit mid-run makes it fail with a parse error after the current country.
- **Git LFS:** `data/kontur_boundaries_IN_20230628.gpkg.zip` is an LFS pointer, and git-lfs may not be installed. India now reads `data/kontur/` instead (`download_kontur.R IN`).
- **The land-mask union can hit invalid geometry.** China's failed after ~25 min with a GEOS "side location conflict": the rebuilt outer rings and the projected Natural Earth crop are now passed through `st_make_valid()` (a no-op for valid input: Bangladesh re-exports byte-identical). The mask is the slowest step for big extents (China ~9 min, the US most of its ~30 min); a `terra` raster version (rasterize, `focal` dilate, `as.polygons(dissolve = TRUE)`, `fillHoles`) took 2.4 min for China but changes every country's outlines slightly, so it wasn't adopted.
- **Tiled exports need memory and patience.** Keep `read_hex_points()` for them: loading the US's 4.2M polygons would exhaust 16 GB. `h3jsr` runs on V8 (~7 s per million cells per call).
- **Running `run_country()` overwrites** `output/plots/<Name>.png` and `output/annotated/<Name>_population_density.png`, which are committed README images. For smoke tests, copy the config under a throwaway name with low `size`/`samples`.
- **Rendered-map side effects:** `create_pop_matrix()` calls `readline()`. Under `Rscript` it returns "" and continues. `annotate_population_map()` calls `swatchplot()`, which leaves an `Rplots.pdf` at the repo root under `Rscript`; delete it, don't commit it.

## Known issues in the R pipeline (not fixed; don't fix unless asked)

- `crs = 3106` (Bangladesh TM, 90°E) is used for Bhutan, Nepal, India, Myanmar, Maldives, Sri Lanka and Israel/Palestine, so maps far from 90°E are distorted.
- `create_pop_matrix.R` uses `h_ratio <- 1.1` for tall countries, which stretches them 10% vertically.
- `check_boundary_plot()` builds a ggplot that is never printed, and maps `fill = boundary$population` (wrong column), so it does nothing.
- The annotation font "Philosopher" is never registered, so magick falls back silently.

## Testing approach

There's no test suite. What has worked:

- **Web:** serve `web/` locally and drive headless Chrome over the DevTools protocol with WebGL enabled: `--headless=new --use-angle=swiftshader --enable-unsafe-swiftshader`. Set exact viewport size with `Emulation.setDeviceMetricsOverride`, because `--window-size` includes browser chrome. Node 22+ has a global `WebSocket`, so no puppeteer is needed. Check:
  - `.chips__chip` buttons render and `[data-status]` becomes hidden
  - search: type "dhaka", Enter → `bangladesh/#dhaka-city`
  - the `.deck-tooltip` text on hover
  - routing: landing keeps `/`, `#nepal` → `nepal/`, `#bangladesh/dhaka-city` → `bangladesh/#dhaka-city`, About-menu country links switch without a reload. Serve under a subfolder (symlink `web` to `/tmp/site/Population-Density-Map`) to match production paths
  - screenshots look right
- **Exporter:** after exporting, validate that `countries.json` matches the folders, the per-hex array lengths are equal, `place` indexes are in range, and view ids are unique.
- **R maps:** a throwaway-name, low-quality `run_country()` render (see the Gotchas section).

## Conventions

- **R:** snake_case (lintr), 80-character lines, `|>` or `%>%` both appear, comments explain *why*. Use `sf` throughout.
- **JS:** a single IIFE, no modules or framework, `const`/`let`, small named functions grouped by section comments (`// ---- Map ----`).
- **Commits:** Conventional Commits, split into reviewable batches (exporter / data / web / ci / docs).
