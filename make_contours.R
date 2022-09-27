
# Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(knitr)
library(tidyr)
library(sf)
library(stars)
library(readr)

#IDW
library(sp)
library(gstat)
library(raster)
library(tmap)
library(rgdal)

# Visualization
library(ggspatial)

### Read/Join data ---------------------------------------------------------
delta <- st_read("shapefiles/Bay_Delta_Poly_New.shp")
zoi_file = list.files("data_zoi", pattern = "NAA_*", full.names = TRUE)
zoi_data <- lapply(zoi_file, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = as.numeric(id)*1000) %>%
  rename(Flow = id)
Month <- "May" #CHANGE THIS TO DESIRED MONTH
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)


# Change all to 4326
delta_4326 <- st_transform(delta, crs = 4326)
nodes_4326 <- st_transform(nodes, crs = st_crs(delta_4326))


### May only ---------------------------------------------------------------
month <- zoi_data %>% dplyr::select(Flow, node, Month) %>%
  rename(DSM2 = May)
month_node <- inner_join(nodes_4326, month) %>%
  mutate(DSM2 = ifelse(DSM2<0, NA, DSM2)) %>%
  na.omit()# may be some missing

### Split up into different files
month_1000 <- month_node %>% filter(Flow == 1000)
month_2000 <- month_node %>% filter(Flow == 2000)
month_3000 <- month_node %>% filter(Flow == 3000)
month_4000 <- month_node %>% filter(Flow == 4000)

### IDW --------------------------------------
#1 https://rpubs.com/Dr_Gurpreet/interpolation_idw_R
#2 https://mgimond.github.io/Spatial/interpolation-in-r.html
     #2 is where the code below comes from (I don't really understand it too well!)

# Separate into different files for each flow
delta_sp <- as(delta_4326, "Spatial")
month_1000_sp <- as(month_1000, "Spatial")
month_2000_sp <- as(month_2000, "Spatial")
month_3000_sp <- as(month_3000, "Spatial")
month_4000_sp <- as(month_4000, "Spatial")

# * 1000 ----------------------------------------
# Create grid based on bounding box of data
grd <- as.data.frame(spsample(month_1000_sp, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_1000_sp) <- proj4string(month_1000_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(month_1000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_1000_sp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r1       <- raster(month.idw)
r.m1     <- mask(r1, delta_sp)

# Plot
tm_shape(r.m1) +
  tm_raster(n=10,palette = "RdBu",
            auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_1000_sp) + tm_dots(size=0.05) +
  tm_legend(legend.outside=TRUE)

# * 2000 ----------------------------------------
# Create grid based on bounding box of data
grd <- as.data.frame(spsample(month_2000_sp, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_2000_sp) <- proj4string(month_2000_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(month_2000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_2000_sp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r2      <- raster(month.idw)
r.m2    <- mask(r2, delta_sp)

# Plot
tm_shape(r.m2) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_2000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# * 3000 ----------------------------------------
# Create grid based on bounding box of data
grd <- as.data.frame(spsample(month_3000_sp, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_3000_sp) <- proj4string(month_3000_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(month_3000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_3000_sp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r3       <- raster(month.idw)
r.m3     <- mask(r3, delta_sp)

# Plot
tm_shape(r.m3) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_3000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

### Functions ------------------------------------
create_df <- function(month, flow) {
  month_data <- zoi_data %>%
    dplyr::select(Flow, node, month) %>%
    rename(DSM2 = month)
  month_node <- inner_join(nodes_4326, month_data) %>%
    mutate(DSM2 = ifelse(DSM2<0, NA, DSM2)) %>%
    na.omit()# may be some missing

  df_filtered <- month_node %>% filter(Flow == flow)
  df_filt_sp <- as(df_filtered, "Spatial")

  return(df_filt_sp)
}

interp_nodes <- function(df, mask=delta_sp) {

  grd <- as.data.frame(spsample(df, "regular", n = 50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object

  # Add projection information to the empty grid
  proj4string(df) <- proj4string(df) # Temp fix until new proj env is adopted
  proj4string(grd) <- proj4string(df)

  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  month.idw <- gstat::idw(DSM2 ~ 1, df, newdata=grd, idp=2.0)

  # Convert to raster object then clip to Texas
  raster       <- raster(month.idw)
  raster_mask     <- mask(raster, mask)

  return(raster_mask)

}

may_3000_sp <- create_df(month = "May", flow = 3000)
r.m3 <- interp_nodes(may_3000_sp)

plot(r.m3)



feb_3000_sp <- create_df(month = "Feb", flow = 3000)
r.m3 <- interp_nodes(feb_3000_sp)

plot(r.m3)


### All Contours ------------------------------------

# Did not figure out how to label this
contour1_95 <- rasterToContour(r.m1, levels = 0.95)
plot(contour1_95)
contour1_75 <- rasterToContour(r.m1, levels = 0.75)
plot(contour1_75)

contour2_95 <- rasterToContour(r.m2, levels = 0.95)
contour2_75 <- rasterToContour(r.m2, levels = 0.75)

contour3_95 <- rasterToContour(r.m3, levels = 0.95)
contour3_75 <- rasterToContour(r.m3, levels = 0.75)

contour4_95 <- rasterToContour(r.m4, levels = 0.95)
contour4_75 <- rasterToContour(r.m4, levels = 0.75)

### Plot -----------------------

#### Make ggmap -----------------
library(ggmap)


coords <- data.frame(st_coordinates(delta_4326))
coords <- data.frame(st_coordinates(delta))

# Define coordinate bounding box. You could also use numbers if you want.
buffer = 0.1
coordDict = list(
  'minLat' = min(coords$Y) - buffer,
  'maxLat' = max(coords$Y) + buffer,
  'minLon' = min(coords$X) - buffer,
  'maxLon' = max(coords$X) + buffer
)
# Create map object using your bounded coordinates
map_obj <- get_stamenmap(
  bbox = c(left = coordDict[['minLon']], bottom = coordDict[['minLat']], right = coordDict[['maxLon']], top = coordDict[['maxLat']]), # the bounding box
  zoom = 8, # zoom lvl; higher number = more detail (but also more processing power)
  maptype = 'terrain-background'# type of basemap; 'terrain' is my default, but check help(get_stamenmap) for a full list
)
# Plot the map
map <- ggmap(map_obj, legend = "right")
map


#### Make larger map ---------------------
library(deltamapr)
# https://stackoverflow.com/questions/34153462/plot-spatiallinesdataframe-with-ggplot2
contour1b <- fortify(contour1)
ggplot(contour1b, aes(x = long, y = lat, group = group)) + geom_path() + theme_classic() + coord_map()

(map2 <- map +
    geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE) +
    geom_sf(data = WW_Delta, fill = "lightblue", color = "lightblue", inherit.aes = FALSE) +
    geom_sf(data = nodes_4326, size = 0.5, inherit.aes = FALSE) +
    geom_path(data = contour1b, aes(x = long, y = lat, group  = group), color = "red", inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    theme_classic())

(map3 <- ggplot() +
    geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE) +
    geom_sf(data = WW_Delta, fill = "lightblue", color = "lightblue", inherit.aes = FALSE) +
    geom_sf(data = nodes_4326, size = 0.5, inherit.aes = FALSE) +
    geom_path(data = contour1b, aes(x = long, y = lat, group  = group), color = "red", inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    theme_classic())

### Export Map ---------------------

map_final
ggsave("StationMap.jpeg", width = 8, height = 5, device = 'jpeg', dpi = 300)

