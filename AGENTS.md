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
  setup_packages.R            # install.packages for everything
  legacy/                     # Old per-country scripts. Reference only, don't edit
data/                         # Kontur .gpkg inputs (~218 MB, committed). Lowercase `data/`
output/                       # Rendered images (README gallery). Committed
web/
  index.html, app.js, style.css, og.jpg
  data/countries.json         # GENERATED: picker index
  data/<key>/hexes.json       # GENERATED: hexagons, palette, views, labels, places
  data/<key>/boundaries.geojson  # GENERATED: region/district outlines
.github/workflows/pages.yml   # Deploys web/ only, on push to main touching web/**
```

Never hand-edit `web/data/`. Regenerate it with the exporter.

## Commands

```bash
# Rendered map (slow: 500 ray samples by default; pauses at a readline prompt)
Rscript -e 'source("run_country.R"); run_country("bangladesh")'

# Web data for one or more countries (also rebuilds web/data/countries.json)
Rscript scripts/export_web_data.R bangladesh nepal

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
  palette = c("#b5e3e6", ...)   # low → high; default is the shared teal ramp
)
```

- Defaults (`web_defaults` in `export_web_data.R`) are Bangladesh's levels and the teal palette.
- `character(0)` means "this tier doesn't exist in the file".
- Admin levels **differ per country**. Check before adding one: `table(sf::st_read(admin_file)$admin_level)`. Current values are in the README table and in each entry.

## Web explorer architecture (`web/app.js`)

- **No build step.** Libraries load from jsDelivr with pinned versions + SRI hashes in `index.html`: MapLibre 5.24.0, h3-js 4.5.0, deck.gl 9.4.0. If you bump a version, recompute its `integrity`: `curl -s URL | openssl dgst -sha384 -binary | openssl base64 -A`. MapLibre 6 is ESM-only (no UMD file to pin with SRI), which is why it stays on 5.x.
- **Boot:** fetch `data/countries.json` → country picker → `initMap()` (once) → `loadCountry(key, viewId)`. A `loadToken` guards against out-of-order responses when switching quickly.
- **URL hash:** `#<country>` or `#<country>/<viewId>`. Unknown hashes fall back to the first country and get rewritten.
- **Layers** (deck `MapboxOverlay`, `interleaved: true`):
  - `H3HexagonLayer` columns, inserted `beforeId` the basemap's first symbol layer so city names stay on top
  - `GeoJsonLayer` outlines + `TextLayer` labels, drawn with `depthCompare: 'always'` so they show over the columns
- **`renderLayers(animate)`** rebuilds the columns; **`renderOverlays()`** rebuilds only outlines/labels and reuses the current column layer.
- **Column height** = `diagonal(view.bounds) × 0.07 / country.max × slider`, so city views get shorter columns. **Colour** = palette at `sqrt(pop / max)`, because the data is heavily skewed.
- **Camera framing** (`cameraFor`) uses its own Web Mercator maths plus panel-aware `padding`.
- **Basemap:** OpenFreeMap `bright`. On load, label `text-field`s are forced to English, and `label_state`, road shields and POI layers are hidden.
- `style.css` holds design tokens mirroring niloy.tech (Inter/Fraunces, teal accent `#0bceaf`, dark glass panel).

## Exporter pipeline (`scripts/export_web_data.R`)

Per country: read hexagons + admin polygons, then:

1. **`build_places()`** tags each hexagon centroid with subdistrict/district/region. Distinct combinations are stored once in `places`; the per-hex `place` column is an index (-1 = none).
2. **`build_land_mask()`** builds Natural Earth 1:50m land (via `rnaturalearth`) ∪ the populated hexagons (1 km raster, 1.5 km buffer, holes filled), in EPSG:3857.
3. **`outlined_areas()`** gives the region + district polygons, tagged `kind = "region" | "district"`. Outlines and labels are clipped to the land mask.
4. **`build_views()`** produces the country, then `web$views`, then regions alphabetically.
5. **`write_manifest()`** rescans `web/data/*/hexes.json` to rebuild `countries.json`. To remove a country, delete its folder and re-run any export.

The JS only knows `kind`, never admin-level numbers. Keep it that way.

## Gotchas (learned the hard way)

- **`modifyList()` drops unnamed lists.** That's why `web_settings()` merges `levels`/`views`/`palette` field by field. Don't "simplify" it back.
- **Natural Earth must be cropped in lon/lat before `st_transform(3857)`.** Polar shapes throw GEOS TopologyExceptions otherwise.
- **Not every hexagon file is EPSG:3857.** `data/merged_population_20231101.gpkg` (Israel & Palestine, built by `legacy/IS&PL.R`) is EPSG:3106. The land mask transforms explicitly.
- **The Israel, Palestine and Israel & Palestine boundary files have no name columns** (only `id`, `admin_level`, `maritime`). They export with hexagons + country view only. This is expected, not a bug.
- **`name_en` can be NA.** `area_names()` falls back to `name`, then to NA.
- **deck.gl `CollisionFilterExtension` hides every label in interleaved mode.** It was tried and reverted, which is why the Maldives labels overlap at the whole-country view.
- **Don't rebuild the column layer on zoom events.** Rebuilding mid-`flyTo` cancels the elevation transition and leaves columns at the previous view's height. Use `renderOverlays()`.
- **`map.cameraForBounds()` factors in the current pitch,** so presets framed inconsistently. `cameraFor()` replaces it; don't switch back.
- **District labels** appear at the country's default zoom + 1.2 (a relative threshold), not at a fixed zoom.
- **Case sensitivity:** the folder is `data/` (lowercase). It was once tracked as `Data/` and broke Linux/CI. Keep paths lowercase.
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

- **Web:** serve `web/` locally and drive headless Chrome over the DevTools protocol with WebGL enabled: `--headless=new --use-angle=swiftshader --enable-unsafe-swiftshader`. Set exact viewport size with `Emulation.setDeviceMetricsOverride`, because `--window-size` includes browser chrome. Check:
  - `.panel__view` buttons render and `[data-status]` becomes hidden
  - the `.deck-tooltip` text on hover
  - hash routing and country switching
  - screenshots look right
- **Exporter:** after exporting, validate that `countries.json` matches the folders, the per-hex array lengths are equal, `place` indexes are in range, and view ids are unique.
- **R maps:** a throwaway-name, low-quality `run_country()` render (see the Gotchas section).

## Conventions

- **R:** snake_case (lintr), 80-character lines, `|>` or `%>%` both appear, comments explain *why*. Use `sf` throughout.
- **JS:** a single IIFE, no modules or framework, `const`/`let`, small named functions grouped by section comments (`// ---- Map ----`).
- **Commits:** Conventional Commits, split into reviewable batches (exporter / data / web / ci / docs).
