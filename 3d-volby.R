library(sf)
library(stars)
library(units)
library(dplyr)
library(readr)
library(RCzechia)
library(rayshader)

# data
raw_data <- read_csv2("pet1.csv",
                      col_types = cols(OBEC = col_character(),
                                       OKRSEK = col_character())) %>% 
  filter(OPRAVA == "0") %>% 
  mutate(pavel = HLASY_04,
         nerudova = HLASY_06,
         babis = HLASY_07)
         
clean_data <- volebni_okrsky("low") %>% 
  st_transform(3035) %>% 
  mutate(OBEC = coalesce(MomcKod, ObecKod)) %>% # mor na ty vaše rody!!!
  rename(OKRSEK = Cislo) %>% 
  inner_join(raw_data, by = c("OKRSEK", "OBEC"))


# grid object
plocha <- as_units(5, "km2") # target cell area 

grid_spacing <- sqrt(2*plocha/sqrt(3)) %>% 
  set_units("m") %>% 
  as.numeric() # sf expects dimensionless spacing in crs units (= meters)

grid <- republika() %>%  
  st_transform(3035) %>% 
  st_make_grid(square = F, cellsize = c(grid_spacing)) %>% # make the grid
  st_intersection(republika() %>% st_transform(3035)) %>% 
  st_sf() 


babis <- st_interpolate_aw(clean_data["babis"],
                                st_geometry(grid),
                                extensive = T) 

pavel <- st_interpolate_aw(clean_data["pavel"],
                                st_geometry(grid),
                                extensive = T) 

nerudova <- st_interpolate_aw(clean_data["nerudova"],
                                st_geometry(grid),
                                extensive = T) 
# matice z vektoru / přes raster
matice_ab <- st_rasterize(babis, 
                          nx = 600, 
                          ny = 400) %>% 
  pull(babis) %>% 
  matrix(nrow = 600,
         ncol = 400)

matice_pp <- st_rasterize(pavel, 
                          nx = 600, 
                          ny = 400) %>% 
  pull(pavel) %>% 
  matrix(nrow = 600,
         ncol = 400)

matice_dn <- st_rasterize(nerudova, 
                          nx = 600, 
                          ny = 400) %>% 
  pull(nerudova) %>% 
  matrix(nrow = 600,
         ncol = 400)

# barvičky pro vykreslení / inspirováno https://www.metmuseum.org/art/collection/search/11145
barvicky_ab <- grDevices::colorRampPalette(RColorBrewer::brewer.pal("Blues", n = 6))(256)
barvicky_pp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal("Reds", n = 6))(256)
barvicky_dn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal("Greens", n = 6))(256)

# vykreslit obrázek Andrejko
height_shade(matice_ab,
             texture = barvicky_ab) %>%
  plot_3d(heightmap = matice_ab,
          zscale = 500,
          solid = FALSE,
          shadow = TRUE,
          shadowdepth = -1,
          shadow_darkness = 4/5,
          theta = 0,
          phi = 35,
          windowsize = c(2200, 1500),
          zoom = 0.5) %>% 
  render_snapshot(filename = "AB.png",
           title_text = "Podpora Andreje Babiše",
           title_font = "Courgette",
           title_size = 50,
           title_position = "north")

# vykreslit obrázek paní profesorky
height_shade(matice_dn,
             texture = barvicky_dn) %>%
  plot_3d(heightmap = matice_dn,
          zscale = 500,
          solid = FALSE,
          shadow = TRUE,
          shadowdepth = -1,
          shadow_darkness = 4/5,
          theta = 0,
          phi = 35,
          windowsize = c(2200, 1500),
          zoom = 0.5) %>% 
  render_snapshot(filename = "DN.png",
                  title_text = "Podpora Danuše Nerudové",
                  title_font = "Courgette",
                  title_size = 50,
                  title_position = "north")

# vykreslit obrázek pana generála
height_shade(matice_pp,
             texture = barvicky_pp) %>%
  plot_3d(heightmap = matice_pp,
          zscale = 500,
          solid = FALSE,
          shadow = TRUE,
          shadowdepth = -1,
          shadow_darkness = 4/5,
          theta = 0,
          phi = 35,
          windowsize = c(2200, 1500),
          zoom = 0.5) %>% 
  render_snapshot(filename = "PP.png",
                  title_text = "Podpora Petra Pavla",
                  title_font = "Courgette",
                  title_size = 50,
                  title_position = "north")


