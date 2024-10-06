# 1. PACKAGES
setwd("C:/Users/B Shorea robusta/OneDrive/3D landcover Esri Bhutan")
libs <- c(
  "terra",
  "giscoR",
  "sf",
  "tidyverse",
  "ggtern",
  "elevatr",
  "png",
  "rayshader",
  "magick"
)
installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libraries == F)){
  install.packages(
    libs[!installed_libraries]
  )
}
invisible(
  lapply(
    libs, library, character.only = T
  )
)

# 2. COUNTRY BORDERS
country_sf <- giscoR::gisco_get_countries(
  country = "BT",
  resolution = "1"
)
plot(sf::st_geometry(country_sf))
png("bih-borders.png")
plot(sf::st_geometry(country_sf))
dev.off()
crs_info <- st_crs(country_sf)
print(crs_info)
# st_write(country_sf, "bhutan.shp")

# 3 DOWNLOAD ESRI LAND COVER TILES
# options(timeout = 600)
# urls <- c(
#   "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2023/46R_20230101-20240101.tif",
#   "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2023/45R_20230101-20240101.tif"
# )
# 
# 
# for(url in urls){
#   download.file(
#     url = url,
#     destfile = basename(url),
#     mode = "wb"
#   )
# }

# 4 LOAD TILES
raster_files <- list.files(
  path = getwd(),
  pattern = "20240101.tif$",
  full.names = T
)
print(raster_files)
# Define file paths
raster_file <- "C:/Users/B Shorea robusta/OneDrive/3D landcover Esri Bhutan/mosaic_bhutan.tif"
shapefile <- "C:/Users/B Shorea robusta/OneDrive/3D landcover Esri Bhutan/bhutan.shp"
# Load the raster and shapefile
r <- rast(raster_file)
shp <- st_read(shapefile)
# Ensure the CRS is the same as the raster
shp <- st_transform(shp, crs(r))
# Crop the raster using the shapefile
cropped_raster <- crop(r, vect(shp))
# Optionally, mask the raster to remove areas outside the shapefile
masked_raster <- mask(cropped_raster, vect(shp))
aggregated_raster <- aggregate(masked_raster, fact = 2)
plot(aggregated_raster)
# Save the aggregated raster
land_cover <- "C:/Users/B Shorea robusta/OneDrive/3Desrilandcovernepal/land_cover_bhutan.tif"
writeRaster(aggregated_raster, land_cover, overwrite = TRUE)

# 5 LOAD VIRTUAL LAYER
r_list <- list.files(
  path = getwd(),
  pattern = "_bhutan",
  full.names = T
)
land_cover_vrt <- terra::vrt(
  r_list,
  "bhutan_land_cover_vrt.vrt",
  overwrite = T
)
print(land_cover_vrt)

# 6 FETCH ORIGINAL COLORS
ras <- terra::rast(
  raster_files[[1]]
)
raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
)
head(raster_color_table)
hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)
# 7 ASSIGN COLORS TO RASTER
cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_bhutan <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_bhutan)
print(land_cover_bhutan)

# 8 DIGITAL ELEVATION MODEL
elev <- elevatr::get_elev_raster(
  locations = country_sf,
  z = 9, clip = "locations"
)

crs_lambert <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

land_cover_bhutan_resampled <- terra::resample(
  x = land_cover_bhutan,
  y = terra::rast(elev),
  method = "near"
) |>
  terra::project(crs_lambert)

terra::plotRGB(land_cover_bhutan_resampled)

img_file <- "land_cover_bhutan.png"

terra::writeRaster(
  land_cover_bhutan_resampled,
  img_file,
  overwrite = T,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE

elev_lambert <- elev |>
  terra::rast() |>
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
  elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 10,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      1000, 600
    ),
    zoom = .5,
    phi = 89.9,
    theta = 70
  )

rayshader::render_camera(
  zoom = .5)

# 10. RENDER OBJECT

# u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
# hdri_file <- basename(u)
# 
# download.file(
#   url = u,
#   destfile = hdri_file,
#   mode = "wb"
# )

filename <- "3d_land_cover_bhutan-dark1.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  light = T,
  environment_light = "C:/Users/B Shorea robusta/OneDrive/3D Landcover map/air_museum_playground_4k.hdr",
  intensity_env = 1,
  rotate_env = 90,
  interactive = F,
  parallel = T,
  width = 3500,
  height = 2500
)

# 11. PUT EVERYTHING TOGETHER

c(
  "#1A5BAB", "#358221", "#87D19E", 
  "#FFDB5C", "#ED022A", "#EDE9E4", 
  "#F2FAFF", "#C8C8C8", "#EFCFA8"
)

legend_name <- "land_cover_legend.png"
png(legend_name,  bg = "transparent")
par(family = "mono")

plot(
  NULL,
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  ylab = "",
  xlab = "",
  xlim = 0:1,
  ylim = 0:1,
  xaxs = "i",
  yaxs = "i"
)
legend(
  "topright",
  legend = c(
    "Water",
    "Forest",
    "Flooded Vegetation",
    "Crops",
    "Built area",
    "Bare Ground",
    "Snow/Ice",
    "clouds",
    "Rangeland"
  ),
  pch = 15,
  cex = 2,
  pt.cex = 2,
  bty = "n",
  col = cols,
  fill = cols,
  border = "black"
)
dev.off()

# Load the magick package
library(magick)
# Read the initial 3D map image
map1 <- magick::image_read("3d_land_cover_bhutan-dark1.png")
# Set text color
title_color <- "#226600"
text_color <- "grey20"
# Add title to the image
map2 <- magick::image_annotate(
  map1, "3D Land Cover Map of Bhutan",
  font = "Georgia",
  color = "black",
  size = 90, gravity = "northwest",
  location = "+1100+400"
)
# Add author caption
map3 <- magick::image_annotate(
  map2, "©2024 B Shorea robusta (https://bishalrayamajhi.com.np)",
  font = "Georgia",
  color = "grey90",
  size = 50, gravity = "southwest",
  location = "+1100+1200"
)
# Add data source caption
map4 <- magick::image_annotate(
  map3, "Data: Global land use/land cover with Sentinel-2",
  font = "Georgia",
  color = "grey90",
  size = 50, gravity = "southwest",
  location = "+1150+1150"
)
# Resize the image to increase resolution
map4_high_res <- magick::image_resize(map4, geometry = "3508x2480")
# Read the legend image
my_legend <- magick::image_read("land_cover_legend.png")
# Scale the legend
my_legend_scaled <- magick::image_scale(
  magick::image_flatten(magick::image_background(my_legend, "none")),
  "2500"
)
# Composite the legend onto the resized image
final_map <- magick::image_composite(
  magick::image_scale(map4_high_res, "x7000"),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+450+3000"
)
# Export the final high-resolution image with annotations and legend
magick::image_write(final_map, path = "3d_bhutan_land_cover_final1.png", format = "png")
