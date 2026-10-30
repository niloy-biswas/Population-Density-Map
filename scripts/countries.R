# Country config reference:
#
#   crs         → Coordinate projection for the country.
#                 3106 = South Asia (most countries here)
#                 3067 = Finland (ETRS89/TM35FIN)
#                 Find yours: epsg.io
#
#   size        → Raster resolution. Higher = more detail + slower.
#                 1000 = fast preview, 3000-3500 = final quality
#
#   palette     → Color scheme for population spikes.
#                 source: "metbrewer" or "rcolorbrewer"
#                 name: palette name (e.g. "Blues", "Benedictus", "OKeeffe1")
#                 n: number of colors (rcolorbrewer only, 3-12)
#                 subset_indices: use only these colors e.g. 2:9
#                 reverse_subset: TRUE to flip color order
#
#   zscale      → Height of population spikes. Lower = taller spikes.
#                 Dense countries (BD): ~55. Sparse (FI): ~15.
#
#   camera      → 3D view angle.
#                 theta: rotation left/right (-180 to 180)
#                 phi:   tilt up/down (0=side, 90=top-down). 45-70 typical.
#                 zoom:  0.5=far, 1.0=close. 0.55-0.75 typical.
#                 fov:   field of view. 100 works for most.
#
#   light_dir      → Light source angle in degrees (0-360).
#                    0=north, 90=east, 180=south, 270=west, 315=northwest.
#
#   light_altitude → Light height angle c(main, fill). Default c(55, 80).
#                    Lower = more glare/shadows. Higher = flatter lighting.
#                    30 = dramatic, 60 = balanced, 80 = flat.
#
#   width/height → Output image size in pixels.
#                 Match country shape: wide country → wider image.
#
#   solid       → TRUE adds solid base beneath terrain (shows country shape).
#                 Useful for sparse/island countries.
#
#   solidcolor  → Color of the solid base sides. Default "grey20".
#                 Dark hex like "#1b2a3b" matches reference-style maps.
#
#   shadow_depth → Depth of solid base below terrain. Must be negative.
#                 0=no base, -50=medium, -200=thick platform (like reference image). # nolint
#
#   show_border → TRUE draws country outline on the base (helps sparse maps).
#
#   samples     → Ray tracing quality. 100=fast preview, 500=final.
#                 Omit to use default (500).
#
#   web         → Optional settings for the web explorer (web/), used only by
#                 scripts/export_web_data.R. Every field has a default there.
#     levels    → Kontur admin_level for each tier. Countries differ:
#                 Bangladesh 4/6/8, Nepal 3/7/8, Finland 6/7/8.
#                 list(region = "4", district = "6", subdistrict = c("8", "10"))
#                 Use character(0) for a tier the country's file lacks.
#     views     → Extra camera presets on top of the country and its regions
#                 (both added automatically).
#                 list(list(id = "dhaka-city", label = "Dhaka city",
#                           bounds = c(west, south, east, north)))
#     palette   → Column colours, low → high. Default: the shared teal ramp.
#     crop      → c(west, south, east, north): drop hexagons and areas outside,
#                 for files that reach far beyond the mainland (France's
#                 overseas territories, the Caribbean Netherlands, Svalbard).
#     bounds    → c(west, south, east, north) for the country view only, when
#                 far-off islands that stay in the map (Canaries, Azores,
#                 Okinawa) would shrink the mainland in the first view.
#     land_km2  → Land area for the "people per km²" figure, where Natural
#                 Earth 1:50m (the default) is far off: it simplifies away
#                 most of the Maldives' atolls and a third of Singapore.
#     mask_km   → Land-mask raster cell in km (default 1). Raise it (3-4) for
#                 countries reaching the Arctic, where the Web Mercator grid
#                 would otherwise run to 100M+ cells.
#     simplify_m → Outline simplification tolerance in metres (default 150).
#                 Raise it for countries with thousands of districts.
#     tiles     → TRUE for countries with millions of hexagons (US, China,
#                 India): exported as a tile pyramid that the browser loads
#                 a screen at a time, coarse when zoomed out. Needs h3jsr.
#
# Entries with only name/hex_file/admin_file/web are web-explorer only; their
# files come from `Rscript scripts/download_kontur.R <ISO>` into data/kontur/.

countries <- list(
  bangladesh = list(
    name = "Bangladesh",
    hex_file = "data/Kontur_Population_20231101.gpkg",
    admin_file = "data/Bangladesh_boundaries_20230628.gpkg",
    crs = 3106,
    size = 3500,
    palette = list(source = "metbrewer", name = "Benedictus", direction = -1),
    zscale = 250 / 4.5,
    camera = list(theta = 0, phi = 70, zoom = 0.55, fov = 100),
    light_dir = 55,
    width = 1400, height = 1580,
    solid = FALSE, shadow_depth = 0,
    web = list(
      views = list(
        list(id = "dhaka-city", label = "Dhaka city",
             bounds = c(90.2, 23.55, 90.6, 24.0))
      )
    )
  ),
  bhutan = list(
    name = "Bhutan",
    hex_file = "data/kontur_population_BT_20231101.gpkg",
    admin_file = "data/kontur_boundaries_BT_20230628.gpkg",
    crs = 3106,
    size = 3000,
    palette = list(source = "rcolorbrewer", name = "Blues", n = 9),
    zscale = 20,
    camera = list(theta = 0, phi = 45, zoom = 0.50, fov = 100),
    light_dir = 40,
    width = 1800, height = 1300,
    solid = FALSE, shadow_depth = 0.8,
    web = list(levels = list(region = "4", district = "6",
                             subdistrict = character(0)))
  ),
  nepal = list(
    name = "Nepal",
    hex_file = "data/kontur_population_NP_20231101.gpkg",
    admin_file = "data/kontur_boundaries_NP_20230628.gpkg",
    crs = 3106,
    size = 3000,
    palette = list(
      source = "rcolorbrewer", name = "PiYG", n = 12,
      subset_indices = 1:11, reverse_subset = TRUE
    ),
    zscale = 250 / 4.5,
    camera = list(theta = 0, phi = 45, zoom = 0.50, fov = 100),
    light_dir = 50,
    width = 1500, height = 1280,
    solid = FALSE, shadow_depth = 0.8,
    web = list(levels = list(region = "3", district = "7", subdistrict = "8"))
  ),
  india = list(
    name = "India",
    hex_file = "data/kontur/kontur_population_IN_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_IN_20230628.gpkg",
    crs = 3106,
    size = 2500,
    palette = list(source = "metbrewer", name = "OKeeffe1", direction = -1),
    zscale = 50,
    camera = list(theta = 0, phi = 75, zoom = 0.50, fov = 100),
    light_dir = 60,
    width = 2600, height = 2000,
    solid = FALSE, shadow_depth = 0.8,
    web = list(
      levels = list(region = "4", district = "5", subdistrict = "7"),
      views = list(
        list(id = "delhi-city", label = "Delhi city",
             bounds = c(76.84, 28.4, 77.35, 28.88)),
        list(id = "mumbai-city", label = "Mumbai city",
             bounds = c(72.77, 18.89, 73.05, 19.27)),
        list(id = "kolkata-city", label = "Kolkata city",
             bounds = c(88.2, 22.45, 88.5, 22.7))
      ),
      mask_km = 2,
      simplify_m = 300,
      tiles = TRUE  # 2.1M hexagons
    )
  ),
  myanmar = list(
    name = "Myanmar",
    hex_file = "data/kontur_population_MM_20231101.gpkg",
    admin_file = "data/kontur_boundaries_MM_20230628.gpkg",
    crs = 3106,
    size = 3000,
    palette = list(
      source = "rcolorbrewer", name = "Purples", n = 9,
      subset_indices = 2:9
    ),
    zscale = 80,
    camera = list(theta = -40, phi = 45, zoom = 0.50, fov = 100),
    light_dir = 80,
    width = 2600, height = 2000,
    solid = FALSE, shadow_depth = 0.8,
    web = list(levels = list(region = "3", district = "7", subdistrict = "9"))
  ),
  maldives = list(
    name = "Maldives",
    hex_file = "data/kontur_population_MaldiVes_20231101.gpkg",
    admin_file = "data/kontur_boundaries_MV_20230628.gpkg",
    crs = 3106,
    size = 2800,
    palette = list(
      source = "rcolorbrewer", name = "PiYG", n = 12,
      subset_indices = 1:11, reverse_subset = TRUE
    ),
    zscale = 250 / 4.5,
    camera = list(theta = 95, phi = 45, zoom = 0.60, fov = 100),
    light_dir = 230,
    width = 1500, height = 1080,
    solid = TRUE, shadow_depth = 0.2,
    web = list(levels = list(region = "4", district = character(0),
                             subdistrict = "9"),
               land_km2 = 298)
  ),
  sri_lanka = list(
    name = "Sri Lanka",
    hex_file = "data/kontur_population_Sri_Lanka_20231101.gpkg",
    admin_file = "data/kontur_boundaries_Sri_Lanka_20230628.gpkg",
    crs = 3106,
    size = 1000,
    palette = list(source = "rcolorbrewer", name = "BuPu", n = 9),
    zscale = 250 / 4.5,
    camera = list(theta = 150, phi = 32, zoom = 0.85, fov = 100),
    light_dir = 130,
    width = 1500, height = 1580,
    solid = FALSE, shadow_depth = 0,
    web = list(levels = list(region = "4", district = "6", subdistrict = "7"))
  ),
  israel = list(
    name = "Israel",
    hex_file = "data/kontur_population_IL_20231101.gpkg",
    admin_file = "data/kontur_topology_boundaries_IL_20230628.gpkg",
    crs = 3106,
    size = 3000,
    palette = list(source = "metbrewer", name = "OKeeffe1", direction = -1),
    zscale = 25,
    camera = list(theta = -50, phi = 70, zoom = 0.60, fov = 100),
    light_dir = 90,
    width = 1500, height = 1900,
    solid = FALSE, shadow_depth = 0.8
  ),
  palestine = list(
    name = "Palestine",
    hex_file = "data/kontur_population_PS_20231101.gpkg",
    admin_file = "data/kontur_topology_boundaries_PS_20230628.gpkg",
    crs = 3106,
    size = 3000,
    palette = list(source = "metbrewer", name = "OKeeffe1", direction = -1),
    zscale = 25,
    camera = list(theta = -50, phi = 50, zoom = 0.60, fov = 100),
    light_dir = 100,
    width = 1700, height = 1900,
    solid = FALSE, shadow_depth = 0.8
  ),
  israel_palestine = list(
    name = "Israel & Palestine",
    hex_file = "data/merged_population_20231101.gpkg",
    admin_file = "data/kontur_boundaries_IL&PS_20230628.gpkg",
    crs = 3106,
    size = 3000,
    palette = list(source = "metbrewer", name = "OKeeffe1", direction = -1),
    zscale = 25,
    camera = list(theta = -50, phi = 70, zoom = 0.60, fov = 100),
    light_dir = 100,
    width = 1500, height = 2000,
    solid = FALSE, shadow_depth = 0.8
  ),
  finland = list(
    name = "Finland",
    hex_file = "data/kontur_population_FI_20231101.gpkg",
    admin_file = "data/kontur_boundaries_FI_20230628.gpkg",
    crs = 3067,
    size = 3000,
    palette = list(source = "rcolorbrewer", name = "Blues", n = 9),
    zscale = 7,
    camera = list(theta = -10, phi = 35, zoom = 0.75, fov = 100),
    light_dir = 310,
    light_altitude = c(60, 80),
    width = 1400, height = 1900,
    solid = TRUE, solidcolor = "#6588b0", shadow_depth = -50,
    show_border = FALSE,
    samples = 500,
    web = list(levels = list(region = "6", district = "7", subdistrict = "8"))
  ),
  austria = list(
    name = "Austria",
    hex_file = "data/kontur/kontur_population_AT_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_AT_20230628.gpkg",
    web = list(levels = list(region = "4", district = "8", subdistrict = "9"))
  ),
  belgium = list(
    name = "Belgium",
    hex_file = "data/kontur/kontur_population_BE_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_BE_20230628.gpkg",
    web = list(levels = list(region = "4", district = "7", subdistrict = "9"))
  ),
  canada = list(
    name = "Canada",
    hex_file = "data/kontur/kontur_population_CA_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_CA_20230628.gpkg",
    web = list(
      # Level 8 mixes regional municipalities and big cities (York Region,
      # Toronto); 5 and 6 cover only parts of the country.
      levels = list(region = "4", district = "8", subdistrict = "10"),
      views = list(
        list(id = "toronto-city", label = "Toronto city",
             bounds = c(-79.65, 43.55, -79.1, 43.86)),
        list(id = "montreal-city", label = "Montreal city",
             bounds = c(-73.98, 45.4, -73.47, 45.71)),
        list(id = "vancouver-city", label = "Vancouver city",
             bounds = c(-123.3, 49.0, -122.5, 49.35))
      ),
      # Web Mercator stretches the Arctic islands until they push the south,
      # where almost everyone lives, off the first view.
      bounds = c(-139, 41.7, -52.6, 64),
      # Natural Earth counts the lakes (9.87M km²); Statistics Canada 2021.
      land_km2 = 8788702,
      mask_km = 3
    )
  ),
  china = list(
    name = "China",
    hex_file = "data/kontur/kontur_population_CN_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_CN_20230628.gpkg",
    web = list(
      levels = list(region = "3", district = "4", subdistrict = "6"),
      views = list(
        list(id = "shanghai-city", label = "Shanghai city",
             bounds = c(121.2, 30.95, 121.8, 31.45)),
        list(id = "beijing-city", label = "Beijing city",
             bounds = c(116.1, 39.7, 116.7, 40.15)),
        list(id = "pearl-river-delta", label = "Pearl River Delta",
             bounds = c(113.0, 22.45, 114.4, 23.35))
      ),
      # The South China Sea islands reach 9°N; the first view stops at Hainan.
      bounds = c(73.5, 18, 134.8, 53.6),
      mask_km = 2,
      simplify_m = 300,
      tiles = TRUE  # 3.1M hexagons
    )
  ),
  czechia = list(
    name = "Czechia",
    hex_file = "data/kontur/kontur_population_CZ_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_CZ_20230628.gpkg",
    web = list(levels = list(region = "7", district = "8", subdistrict = "9"))
  ),
  denmark = list(
    name = "Denmark",
    hex_file = "data/kontur/kontur_population_DK_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_DK_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "8", subdistrict = character(0))
    )
  ),
  france = list(
    name = "France",
    hex_file = "data/kontur/kontur_population_FR_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_FR_20230628.gpkg",
    web = list(
      levels = list(region = "5", district = "6", subdistrict = "8"),
      crop = c(-5.5, 41.2, 9.8, 51.3),  # metropolitan France and Corsica
      views = list(
        list(id = "paris", label = "Paris", bounds = c(2.1, 48.7, 2.6, 49.0))
      )
    )
  ),
  germany = list(
    name = "Germany",
    hex_file = "data/kontur/kontur_population_DE_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_DE_20230628.gpkg",
    web = list(levels = list(region = "4", district = "7", subdistrict = "10"))
  ),
  greece = list(
    name = "Greece",
    hex_file = "data/kontur/kontur_population_GR_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_GR_20230628.gpkg",
    web = list(levels = list(region = "5", district = "6", subdistrict = "9"))
  ),
  hungary = list(
    name = "Hungary",
    hex_file = "data/kontur/kontur_population_HU_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_HU_20230628.gpkg",
    web = list(levels = list(region = "6", district = "7", subdistrict = "9"))
  ),
  ireland = list(
    name = "Ireland",
    hex_file = "data/kontur/kontur_population_IE_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_IE_20230628.gpkg",
    web = list(levels = list(region = "6", district = "10", subdistrict = "11"))
  ),
  italy = list(
    name = "Italy",
    hex_file = "data/kontur/kontur_population_IT_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_IT_20230628.gpkg",
    web = list(levels = list(region = "4", district = "7", subdistrict = "9"))
  ),
  japan = list(
    name = "Japan",
    hex_file = "data/kontur/kontur_population_JP_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_JP_20230628.gpkg",
    web = list(
      levels = list(region = "5", district = "9", subdistrict = "10"),
      bounds = c(129.3, 30.9, 146.0, 45.6),  # main islands (Okinawa: own view)
      views = list(
        list(id = "tokyo-city", label = "Tokyo city",
             bounds = c(139.4, 35.5, 140.0, 35.85))
      )
    )
  ),
  netherlands = list(
    name = "Netherlands",
    hex_file = "data/kontur/kontur_population_NL_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_NL_20230628.gpkg",
    web = list(
      levels = list(region = "5", district = "10", subdistrict = "12"),
      crop = c(3.2, 50.7, 7.3, 53.7)  # European Netherlands
    )
  ),
  nigeria = list(
    name = "Nigeria",
    hex_file = "data/kontur/kontur_population_NG_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_NG_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "7", subdistrict = "9"),
      views = list(
        list(id = "lagos-city", label = "Lagos city",
             bounds = c(3.1, 6.4, 3.7, 6.7))
      )
    )
  ),
  norway = list(
    name = "Norway",
    hex_file = "data/kontur/kontur_population_NO_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_NO_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "8", subdistrict = character(0)),
      crop = c(4.0, 57.8, 31.5, 71.3)  # mainland, without Svalbard
    )
  ),
  pakistan = list(
    name = "Pakistan",
    hex_file = "data/kontur/kontur_population_PK_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_PK_20230628.gpkg",
    web = list(
      levels = list(region = "3", district = "6", subdistrict = "7"),
      views = list(
        list(id = "karachi", label = "Karachi",
             bounds = c(66.8, 24.75, 67.3, 25.1))
      )
    )
  ),
  poland = list(
    name = "Poland",
    hex_file = "data/kontur/kontur_population_PL_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_PL_20230628.gpkg",
    web = list(levels = list(region = "4", district = "8", subdistrict = "9"))
  ),
  portugal = list(
    name = "Portugal",
    hex_file = "data/kontur/kontur_population_PT_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_PT_20230628.gpkg",
    web = list(
      levels = list(region = c("4", "7"), district = "8", subdistrict = "10"),
      bounds = c(-9.6, 36.9, -6.1, 42.2)  # mainland
    )
  ),
  romania = list(
    name = "Romania",
    hex_file = "data/kontur/kontur_population_RO_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_RO_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = character(0), subdistrict = "9")
    )
  ),
  singapore = list(
    name = "Singapore",
    hex_file = "data/kontur/kontur_population_SG_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_SG_20230628.gpkg",
    web = list(
      levels = list(region = "7", district = character(0),
                    subdistrict = character(0)),
      land_km2 = 735
    )
  ),
  spain = list(
    name = "Spain",
    hex_file = "data/kontur/kontur_population_ES_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_ES_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "7", subdistrict = "9"),
      bounds = c(-9.4, 35.9, 4.4, 43.8)  # mainland and Balearics
    )
  ),
  sweden = list(
    name = "Sweden",
    hex_file = "data/kontur/kontur_population_SE_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_SE_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "8", subdistrict = character(0))
    )
  ),
  switzerland = list(
    name = "Switzerland",
    hex_file = "data/kontur/kontur_population_CH_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_CH_20230628.gpkg",
    web = list(levels = list(region = "4", district = "8", subdistrict = "11"))
  ),
  ukraine = list(
    name = "Ukraine",
    hex_file = "data/kontur/kontur_population_UA_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_UA_20230628.gpkg",
    web = list(levels = list(region = "4", district = "6", subdistrict = "7"))
  ),
  united_kingdom = list(
    name = "United Kingdom",
    hex_file = "data/kontur/kontur_population_GB_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_GB_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "7", subdistrict = "10"),
      crop = c(-8.8, 49.8, 2.0, 61.0),  # without Rockall
      views = list(
        list(id = "london", label = "London",
             bounds = c(-0.55, 51.28, 0.3, 51.7))
      )
    )
  ),
  united_states = list(
    name = "United States",
    hex_file = "data/kontur/kontur_population_US_20231101.gpkg",
    admin_file = "data/kontur/kontur_boundaries_US_20230628.gpkg",
    web = list(
      levels = list(region = "4", district = "6", subdistrict = "8"),
      views = list(
        list(id = "new-york-city", label = "New York City",
             bounds = c(-74.26, 40.49, -73.7, 40.92)),
        list(id = "los-angeles-city", label = "Los Angeles city",
             bounds = c(-118.67, 33.7, -117.9, 34.34)),
        list(id = "chicago-city", label = "Chicago city",
             bounds = c(-87.94, 41.64, -87.52, 42.03))
      ),
      # West of 180° (Guam, the Northern Marianas, the far Aleutians) would
      # wrap the map round the globe; American Samoa is south of the box.
      crop = c(-180, 17, -64, 72),
      bounds = c(-125, 24.4, -66.9, 49.4),  # lower 48; Alaska is a region
      mask_km = 2,
      simplify_m = 400,
      tiles = TRUE  # 4.2M hexagons
    )
  )
)
