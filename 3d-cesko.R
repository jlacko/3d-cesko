library(sf)
library(curl)
library(stars)
library(units)
library(ggplot2)
library(rayshader)

# země, která nás zajímá (Čechy Čechům!)
zipak <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_CZ_20220630.gpkg.gz"

if(!file.exists("target.gz")) curl::curl_download(url = zipak, destfile = "target.gz")

# ze zipáku vykuchat geopackage
R.utils::gunzip("target.gz", destname = "target.gpkg", overwrite = TRUE, remove = FALSE)

# načíst geopackage jako {sf} objekt
country <- sf::st_read("target.gpkg") %>% 
  st_transform(3857) # web mercator, pro sichr

# Define aspect ratio based on bounding box
bb <- st_bbox(country)

# Retrieve bottom left/right coordinates to find width and height values
bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(country))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(country))

# Use distance between the two points to find width
width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(country))

# Use distance between bottom and top left to find heigh
height <- st_distance(bottom_left, top_left)

# Handle conditions of width or height being the longer side
if(width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
} 


# Convert to raster so we can then convert to matrix
size <- 1500

plot_src <- st_rasterize(country, 
                         nx = floor(size * w_ratio), 
                         ny = floor(size * h_ratio))

matice <- matrix(plot_src$population, 
                 nrow = floor(size * w_ratio), 
                 ncol = floor(size * h_ratio))

barvicky <- grDevices::colorRampPalette(MetBrewer::met.brewer(name="Homer1"))(256)

# vykreslit vlastní rayshading
height_shade(matice,
             texture = barvicky) %>%
  plot_3d(heightmap = matice,
          zscale = 50,
          solid = FALSE,
          shadow = FALSE,
          theta = 0,
          windowsize = c(9000, 6000),
          zoom = 0.5) 

# uložit pro budoucí generace...
render_snapshot(filename = "country.png")
