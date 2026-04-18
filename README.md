# Population Density Map

3D population density maps built with R and Rayshader, using Kontur population data.

## Project Structure

```
Population/
├── run_country.R          # Entry point — run maps from here
├── scripts/
│   ├── countries.R        # All country configs (edit here to tweak maps)
│   ├── helpers.R          # Palette creation utilities
│   ├── create_pop_matrix.R
│   ├── plot_population_3d.R
│   ├── annotate_population_map.R
│   ├── setup_packages.R   # Run once to install dependencies
│   └── legacy/            # Old per-country scripts (reference only)
├── data/                  # Kontur .gpkg population + boundary files
└── output/
    ├── plots/             # Raw 3D renders
    └── annotated/         # Final annotated maps
```

## Setup

**1. Install R**
Download from [cran.r-project.org](https://cran.r-project.org) (v4.4+)

**2. Install XQuartz** (macOS only — required for rgl)
Download from [xquartz.org](https://www.xquartz.org), install, then restart Mac.

**3. Install R packages**
Open `scripts/setup_packages.R` and run it once.

## Running a Map

Open `run_country.R`, uncomment the country you want, and source the file:

```r
run_country("bangladesh")
# run_country("finland")
# run_country("india")
# ... etc
```

Output saves automatically to `output/annotated/<Country>_population_density.png`.

## Adding a New Country

1. Get data from [Kontur](https://data.humdata.org/organization/kontur):
   - Population file: `kontur_population_XX_*.gpkg`
   - Boundaries file: `kontur_boundaries_XX_*.gpkg`
   - Place both in `data/`

2. Add a config block to `scripts/countries.R`:

```r
my_country = list(
  name       = "My Country",
  hex_file   = "data/kontur_population_XX_20231101.gpkg",
  admin_file = "data/kontur_boundaries_XX_20230628.gpkg",
  crs        = 3857,        # find at epsg.io
  size       = 3000,
  palette    = list(source = "metbrewer", name = "Benedictus", direction = -1),
  zscale     = 40,
  camera     = list(theta = 0, phi = 55, zoom = 0.75, fov = 100),
  light_dir  = 315,
  width      = 1400, height = 1800,
  solid      = FALSE, shadow_depth = 0
)
```

3. Uncomment in `run_country.R`:

```r
run_country("my_country")
```

See the comment block at the top of `scripts/countries.R` for what each parameter does.

## Maps

## Bangladesh

![Bangladesh](output/annotated/Bangladesh_population_density.png)

## Dhaka

![Dhaka](output/annotated/Dhaka_population_density.png)

## Chittagong

![Chittagong](output/annotated/Chittagong_population_density.png)

## Barishal

![Barishal](output/annotated/Barishal_population_density.png)

## India

![India](output/annotated/India%20population%20density%20Map.jpg)

## Nepal

![Nepal](output/annotated/Nepal%20population%20density%20Map.jpg)

## Bhutan

![Bhutan](output/annotated/Bhutan%20population%20density%20Map.jpg)

## Sri Lanka

![Sri Lanka](output/annotated/Sri_Lanka%20population%20density%20Map.jpg)

## Myanmar

![Myanmar](output/annotated/Myanmar%20population%20density%20Map.jpg)

## Maldives

![Maldives](output/annotated/MalDives%20population%20density%20Map.jpg)

## Israel

![Israel](output/annotated/Israel%20population%20density%20Map.png)

## Palestine

![Palestine](output/annotated/Palestine%20population%20density%20Map.png)

## Israel & Palestine

![Israel & Palestine](output/annotated/Israel%20&%20Palestine%20population%20density%20Map.png)

## Contributing

Fork the repo, add your country config to `scripts/countries.R`, and submit a pull request.
