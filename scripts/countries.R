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
#     views     → Extra camera presets on top of the country and its regions
#                 (both added automatically).
#                 list(list(id = "dhaka-city", label = "Dhaka city",
#                           bounds = c(west, south, east, north)))
#     palette   → Column colours, low → high. Default: the shared teal ramp.

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
    solid = FALSE, shadow_depth = 0.8
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
    solid = FALSE, shadow_depth = 0.8
  ),
  india = list(
    name = "India",
    hex_file = "data/kontur_population_IN_20231101.gpkg",
    admin_file = "data/kontur_boundaries_IN_20230628.gpkg.zip",
    crs = 3106,
    size = 2500,
    palette = list(source = "metbrewer", name = "OKeeffe1", direction = -1),
    zscale = 50,
    camera = list(theta = 0, phi = 75, zoom = 0.50, fov = 100),
    light_dir = 60,
    width = 2600, height = 2000,
    solid = FALSE, shadow_depth = 0.8
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
    solid = FALSE, shadow_depth = 0.8
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
    solid = TRUE, shadow_depth = 0.2
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
    solid = FALSE, shadow_depth = 0
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
    samples = 500
  )
)
