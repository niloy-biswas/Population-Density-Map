# AGENTS.md

Quick reference for AI agents working in this repo. Read this before exploring; it should save you most of the reading.

## What this repo is

3D population density maps from [Kontur](https://data.humdata.org/organization/kontur) H3 hexagon data (resolution 8, ~0.74 km² per hexagon), in two independent outputs that share one config:

| Output | Tech | Entry point | Result |
|--------|------|-------------|--------|
| Rendered maps | R + rayshader (ray-traced PNGs) | `run_country.R` | `output/plots/`, `output/annotated/` |
| Web explorer | Static HTML/JS, deck.gl + MapLibre | `web/index.html` | Live at https://niloy.tech/Population-Density-Map/ |

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
  export_web_data.R           # Web data exporter (all web data comes from here)
  download_kontur.R           # Fetch Kontur files by ISO code → data/kontur/
  setup_packages.R            # install.packages for everything
  legacy/                     # Old per-country scripts. Reference only, don't edit
data/                         # Kontur .gpkg inputs (~218 MB, committed). Lowercase `data/`
  kontur/                     # Downloaded by download_kontur.R. GITIGNORED (~3 GB)
output/                       # Rendered images (README gallery). Committed
web/
  index.html, app.js, style.css, og.jpg, apple-touch-icon.png
  data/countries.json         # GENERATED: search index (countries + view ids/labels)
  data/<key>/hexes.json       # GENERATED: hexagons, palette, views, labels, places
  data/<key>/boundaries.geojson  # GENERATED: region/district outlines
.github/workflows/pages.yml   # Deploys web/ only, on push to main touching web/**
```

Never hand-edit `web/data/`. Regenerate it with the exporter.

## Commands

```bash
# Rendered map (slow: 500 ray samples by default; pauses at a readline prompt)
Rscript -e 'source("run_country.R"); run_country("bangladesh")'

# Fetch Kontur files (population + boundaries) by ISO code; skips existing
Rscript scripts/download_kontur.R GB DE

# Web data for one or more countries (also rebuilds web/data/countries.json)
Rscript scripts/export_web_data.R bangladesh nepal
Rscript scripts/export_web_data.R --manifest   # only rebuild countries.json

# Preview the explorer
python3 -m http.server 8000 -d web      # → http://localhost:8000/#bangladesh

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
  bounds  = c(w, s, e, n)        # country view only; islands kept (Spain, Portugal, Japan)
)
```

- The 24 countries added in the second batch (Europe, Japan, Pakistan, Nigeria, Singapore, ...) are **web-only**: just `name`/`hex_file`/`admin_file`/`web`, with files in `data/kontur/`. `run_country()` stops with a clear error for them until rayshader fields are added.

- Defaults (`web_defaults` in `export_web_data.R`) are Bangladesh's levels and the teal palette.
- `character(0)` means "this tier doesn't exist in the file".
- Admin levels **differ per country**. Check before adding one: `table(sf::st_read(admin_file)$admin_level)`. Current values are in the README table and in each entry.

## Web explorer architecture (`web/app.js`)

- **No build step.** Libraries load from jsDelivr with pinned versions + SRI hashes in `index.html`: MapLibre 5.24.0, h3-js 4.5.0, deck.gl 9.4.0. If you bump a version, recompute its `integrity`: `curl -s URL | openssl dgst -sha384 -binary | openssl base64 -A`. MapLibre 6 is ESM-only (no UMD file to pin with SRI), which is why it stays on 5.x.
- **Boot:** fetch `data/countries.json` → `buildSearchIndex()` → `initMap()` (once) → `loadCountry(key, viewId)`. A `loadToken` guards against out-of-order responses when switching quickly.
- **UI (Google Maps style):** top-left column (`.panel`, used by `framePadding()`) holds the search combobox, a scrolling chip row of the country's views, and a summary card with the legend. The card's chevron hides chips + card (`.panel__details`) behind a "<Country> · N people" pill; open by default, remembered in `localStorage` (`pdm-panel-collapsed`). The hamburger opens an About popover. Bottom-right, `.view-ctrl` is added as a MapLibre control above zoom/compass: a layers popover (column height, tilt, rotate, reset) and a 3D/flat toggle. Defaults: height 1.5×, every view opens at `DEFAULT_PITCH` = 30°.
- **Search** (`buildSearchIndex`, `searchPlaces`): matches countries + manifest views, accent-insensitive; ranks name-start, then word-start, then mid-word, then country > city > region, then shorter names. Until the visitor types, the box shows the current place and the list shows all countries (it doubles as the country dropdown). Picking a view of another country calls `loadCountry(key, viewId)`.
- **URL hash:** `#<country>` or `#<country>/<viewId>`. The bare link and unknown hashes open `DEFAULT_COUNTRY` (Bangladesh, matching `og.jpg`; first country if it is ever removed) and get rewritten. Don't rely on manifest order: it is alphabetical, so Austria became the landing page by accident once.
- **Layers** (deck `MapboxOverlay`, `interleaved: true`):
  - `H3HexagonLayer` columns, inserted `beforeId` the basemap's first symbol layer so city names stay on top
  - `GeoJsonLayer` outlines, drawn with `depthCompare: 'always'` so they show over the columns, inserted `beforeId` our label layers
- **Labels are MapLibre symbol layers** (`pdm-labels-region` above `pdm-labels-district`, one GeoJSON source), added on load and refilled per country by `labelFeatures()`. MapLibre's collision detection thins them like the basemap's own names; `symbol-sort-key` ranks each label by the people in its area (summed from the hexes), so bigger areas win. The region layer sits above, so regions are placed before districts. They are styled like the basemap's own place names (Noto Sans Regular, black, 1 px blurred white halo, normal case); regions are just 2 px larger.
- **`renderLayers(animate)`** rebuilds the columns and outlines. Nothing rebuilds on zoom.
- **Column height** = `diagonal(view.bounds) × 0.07 / country.max × slider`, so city views get shorter columns. **Colour** = palette at `sqrt(pop / max)`, because the data is heavily skewed.
- **Camera framing** (`cameraFor`) uses its own Web Mercator maths plus panel-aware `padding`.
- **Basemap:** OpenFreeMap `bright`. On load, label `text-field`s are forced to English, and `label_state`, road shields and POI layers are hidden.
- `style.css` holds design tokens mirroring niloy.tech (Inter/Fraunces, teal accent `#0bceaf`, dark glass panel).

## Exporter pipeline (`scripts/export_web_data.R`)

Per country: read hexagons + admin polygons (`read_admin()` loads only level 2 and the configured tiers via an SQL query; the full files reach 500 MB), apply `web$crop` if set (`crop_country()`), then:

1. **`build_places()`** tags each hexagon centroid with subdistrict/district/region. Distinct combinations are stored once in `places`; the per-hex `place` column is an index (-1 = none).
2. **`build_land_mask()`** builds Natural Earth 1:50m land (via `rnaturalearth`) ∪ the populated hexagons (1 km raster, 1.5 km buffer, holes filled), in EPSG:3857.
3. **`outlined_areas()`** gives the region + district polygons, tagged `kind = "region" | "district"`. Outlines and labels are clipped to the land mask.
4. **`build_views()`** produces the country, then `web$views`, then regions alphabetically.
5. **`write_manifest()`** rescans `web/data/*/hexes.json` to rebuild `countries.json`, including each country's views (`id`, `label`, `kind` = `"city"` for `web$views` presets, else `"region"`) for search. To remove a country, delete its folder and re-run any export.

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
- **`web/index.html` names countries by hand** in the meta descriptions and the JSON-LD block (the exporter doesn't touch it). Update them when adding a country.
- **District labels** appear at the country's default zoom + 1.2 (a relative threshold, set per country with `map.setLayerZoomRange()`), not at a fixed zoom. Past that, collision decides which show.
- **Button and label names drop admin suffixes** (`ADMIN_SUFFIX` in `app.js`: "Division", "Prefecture", "State", ...). A city preset named like a region then shows two identical buttons ("Lagos" vs "Lagos State"), so presets are called "<City> city".
- **Case sensitivity:** the folder is `data/` (lowercase). It was once tracked as `Data/` and broke Linux/CI. Keep paths lowercase.
- **`data/kontur/` is gitignored.** A fresh clone has none of the newer countries' inputs; re-run `download_kontur.R` with the ISO codes from the `hex_file` paths before exporting them. Don't commit those files: France's boundaries are 500 MB.
- **Overseas territories wreck the country view and the land mask.** France's hexagon file spans lon -178 to 168; the 1 km land-mask raster over that extent would exhaust memory. Check a new country's hexagon extent; if it reaches far beyond the mainland, set `web$crop`. Spain (Canaries), Portugal (Azores) and Japan are wide but kept whole on purpose.
- **GDAL turns tiny outline pieces into GeometryCollections.** With `COORDINATE_PRECISION=4` (~11 m), a short clipped segment collapses to one point and GDAL writes it as a MultiPoint inside a GeometryCollection. deck.gl's GeoJsonLayer then rejects the whole `boundaries.geojson` (no outlines at all), and logs it only once per session, so a multi-country browser run under-reports it. `drop_specks()` removes those parts before writing; keep it, and check exported geometry types are only LineString/MultiLineString.
- **View ids are URL hashes and must be unique.** A preset named after a region (Japan's "Tokyo" preset vs Tokyo Prefecture) collides; `build_views()` now stops on duplicates.
- **Don't edit `export_web_data.R` while an export is running.** Rscript reads the file as it goes, so an edit mid-run makes it fail with a parse error after the current country.
- **Git LFS:** `data/kontur_boundaries_IN_20230628.gpkg.zip` is an LFS pointer, and git-lfs may not be installed. India's population file isn't in `data/` at all, so India can't be rendered or exported.
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
  - search: type "dhaka", Enter → `#bangladesh/dhaka-city`
  - the `.deck-tooltip` text on hover
  - hash routing and country switching
  - screenshots look right
- **Exporter:** after exporting, validate that `countries.json` matches the folders, the per-hex array lengths are equal, `place` indexes are in range, and view ids are unique.
- **R maps:** a throwaway-name, low-quality `run_country()` render (see the Gotchas section).

## Conventions

- **R:** snake_case (lintr), 80-character lines, `|>` or `%>%` both appear, comments explain *why*. Use `sf` throughout.
- **JS:** a single IIFE, no modules or framework, `const`/`let`, small named functions grouped by section comments (`// ---- Map ----`).
- **Commits:** Conventional Commits, split into reviewable batches (exporter / data / web / ci / docs).
