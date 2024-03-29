library(sf)
library(curl)
library(stars)
library(units)
library(ggplot2)
library(rayshader)

# země, která nás zajímá (Čechy Čechům!) - ale jiná by fungovala obdobně
zipak <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_CZ_20220630.gpkg.gz"

if(!file.exists("target.gz")) curl::curl_download(url = zipak, destfile = "target.gz")

# ze zipáku vykuchat geopackage
R.utils::gunzip("target.gz", destname = "target.gpkg", overwrite = TRUE, remove = FALSE)

# načíst geopackage jako {sf} objekt
country <- sf::st_read("target.gpkg") %>% 
  st_transform(3857) # web mercator, pro sichr

# z vektoru raster - menší stačí... pozor! formát 3:2 funguje pro Česko, není platný obecně
plot_src <- st_rasterize(country, 
                         nx = 900, 
                         ny = 600)

# z rasteru matici! velikost jako raster
matice <- matrix(plot_src$population, 
                 nrow = 900, 
                 ncol = 600)

# barvičky pro vykreslení / inspirováno https://www.metmuseum.org/art/collection/search/11145
barvicky <- grDevices::colorRampPalette(MetBrewer::met.brewer(name="Homer1"))(256)

# vykreslit obrázek
height_shade(matice,
             texture = barvicky) %>%
  plot_3d(heightmap = matice,
          zscale = 90,
          solid = FALSE,
          shadow = TRUE,
          shadowdepth = -1,
          shadow_darkness = 4/5,
          theta = 0,
          phi = 35,
          windowsize = c(2200, 1500),
          zoom = 0.5) 

# rayshade! pozor, není rychlé (ani trochu)
render_highquality(
  "country.png",
  parallel = TRUE, 
  samples = 500,
  light = TRUE, 
  lightdirection = 210,
  lightintensity = 750,
  interactive = FALSE,
  width = 3000, 
  height = 2000
)

