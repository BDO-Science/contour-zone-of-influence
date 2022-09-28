
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
library(ggmap)
library(ggspatial)
library(deltamapr)
library(viridis)

# Read/Join data ---------------------------------------------------------
delta <- st_read("shapefiles/Bay_Delta_Poly_New.shp")
zoi_file = list.files("data_zoi", pattern = "NAA_*", full.names = TRUE)
zoi_data <- lapply(zoi_file, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = as.numeric(id)*1000) %>%
  rename(Flow = id)
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)

dropNodes <- c(146, 147, 148, 206, 242, 246)
nodes <- nodes[!nodes$node %in% dropNodes, ]

# Change all to 4326
delta_4326 <- st_transform(delta, crs = 4326)
nodes_4326 <- st_transform(nodes, crs = st_crs(delta_4326))

# Month <- "May" #CHANGE THIS TO DESIRED MONTH

## Join nodes with zoi data
# month <- zoi_data %>% dplyr::select(Flow, node, Month) %>%
#   rename(DSM2 = May)
# month_node <- inner_join(nodes_4326, month) %>%
#   mutate(DSM2 = ifelse(DSM2<0, NA, DSM2)) %>%
#   na.omit()# may be some missing

# Functions ------------------------------------

# Select month and flow value
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

# Interpolation based on data frame (from above) and mask shapefile

#1 https://rpubs.com/Dr_Gurpreet/interpolation_idw_R
#2 https://mgimond.github.io/Spatial/interpolation-in-r.html
  #2 is where the code below comes from (I don't really understand it too well!)

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

# Analysis ------------------------------
## Create data frames --------------------
flows = c(1000,2000,3000,4000)

### May --------------
may_1000_sp <- create_df(month = "May", flow = 1000)
may_2000_sp <- create_df(month = "May", flow = 2000)
may_3000_sp <- create_df(month = "May", flow = 3000)
may_4000_sp <- create_df(month = "May", flow = 4000)

### April --------------
apr_1000_sp <- create_df(month = "Apr", flow = 1000)
apr_2000_sp <- create_df(month = "Apr", flow = 2000)
apr_3000_sp <- create_df(month = "Apr", flow = 3000)

### March --------------
mar_1000_sp <- create_df(month = "Mar", flow = 1000)
mar_2000_sp <- create_df(month = "Mar", flow = 2000)
mar_3000_sp <- create_df(month = "Mar", flow = 3000)
mar_4000_sp <- create_df(month = "Mar", flow = 4000)
mar_5000_sp <- create_df(month = "Mar", flow = 5000)

## Create interpolations -------------------

delta_sp <- as(delta_4326, "Spatial")

### May --------------
r.may1 <- interp_nodes(may_1000_sp)
r.may2 <- interp_nodes(may_2000_sp)
r.may3 <- interp_nodes(may_3000_sp)
r.may4 <- interp_nodes(may_4000_sp)

### April ------------
r.apr1 <- interp_nodes(apr_1000_sp)
r.apr2 <- interp_nodes(apr_2000_sp)
r.apr3 <- interp_nodes(apr_3000_sp)

### March --------------
r.mar1 <- interp_nodes(mar_1000_sp)
r.mar2 <- interp_nodes(mar_2000_sp)
r.mar3 <- interp_nodes(mar_3000_sp)
r.mar4 <- interp_nodes(mar_4000_sp)
r.mar5 <- interp_nodes(mar_5000_sp)

## Create contours --------------------------

### May ----------------------
c.may1_95 <- rasterToContour(r.may1, levels = 0.95)
plot(c.may1_95)
c.may1_75 <- rasterToContour(r.may1, levels = 0.75)
plot(c.may1_75)

c.may2_95 <- rasterToContour(r.may2, levels = 0.95)
c.may2_75 <- rasterToContour(r.may2, levels = 0.75)

c.may3_95 <- rasterToContour(r.may3, levels = 0.95)
c.may3_75 <- rasterToContour(r.may3, levels = 0.75)

c.may4_95 <- rasterToContour(r.may4, levels = 0.95)
c.may4_75 <- rasterToContour(r.may4, levels = 0.75)

contours_m <- c(c.may1_75, c.may1_95, c.may2_75, c.may2_95,
                c.may3_75, c.may3_95, c.may4_75, c.may4_95)

contours_may <- lapply(contours_m, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(month = "may") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -1000,
                          id %in% c(3,4) ~ -2000,
                          id %in% c(5,6) ~ -3000,
                          id %in% c(7,8) ~ -4000),
         contour = case_when(id %in% c(1,3, 5,7) ~ 0.75,
                             id %in% c(2,4,6,8) ~ 0.95))

### Apr ----------------------
c.apr1_95 <- rasterToContour(r.apr1, levels = 0.95)
c.apr1_75 <- rasterToContour(r.apr1, levels = 0.75)

c.apr2_95 <- rasterToContour(r.apr2, levels = 0.95)
c.apr2_75 <- rasterToContour(r.apr2, levels = 0.75)

c.apr3_95 <- rasterToContour(r.apr3, levels = 0.95)
c.apr3_75 <- rasterToContour(r.apr3, levels = 0.75)

contours_a <- c(c.apr1_75, c.apr1_95, c.apr2_75, c.apr2_95,c.apr3_75, c.apr3_95)

contours_april <- lapply(contours_a, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(month = "april") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -1000,
                          id %in% c(3,4) ~ -2000,
                          id %in% c(5,6) ~ -3000),
         contour = case_when(id %in% c(1,3, 5) ~ 0.75,
                             id %in% c(2,4,6) ~ 0.95))

### March --------------------------
c.mar1_95 <- rasterToContour(r.mar1, levels = 0.95)
plot(c.mar1_95)
c.mar1_75 <- rasterToContour(r.mar1, levels = 0.75)
plot(c.mar1_75)

c.mar2_95 <- rasterToContour(r.mar2, levels = 0.95)
c.mar2_75 <- rasterToContour(r.mar2, levels = 0.75)

c.mar3_95 <- rasterToContour(r.mar3, levels = 0.95)
c.mar3_75 <- rasterToContour(r.mar3, levels = 0.75)

c.mar4_95 <- rasterToContour(r.mar4, levels = 0.95)
c.mar4_75 <- rasterToContour(r.mar4, levels = 0.75)

c.mar5_95 <- rasterToContour(r.mar5, levels = 0.95)
c.mar5_75 <- rasterToContour(r.mar5, levels = 0.75)

contours_mr <- c(c.mar1_75, c.mar1_95, c.mar2_75, c.mar2_95,
                c.mar3_75, c.mar3_95, c.mar4_75, c.mar4_95, 
                c.mar5_75, c.mar5_95)

contours_mar <- lapply(contours_mr, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(month = "mar") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -1000,
                          id %in% c(3,4) ~ -2000,
                          id %in% c(5,6) ~ -3000,
                          id %in% c(7,8) ~ -4000,
                          id %in% c(9,10) ~ -5000),
         contour = case_when(id %in% c(1,3, 5,7, 9) ~ 0.75,
                             id %in% c(2,4,6,8, 10) ~ 0.95))




### Combine all
contours_all <- rbind(contours_april, contours_may, contours_mar) %>%
  mutate(flow = as.factor(flow))


# Plot -----------------------

## Make basemap (ggmap) -----------------

coords <- data.frame(st_coordinates(delta_4326))



## Make larger map ---------------------

# https://stackoverflow.com/questions/34153462/plot-spatiallinesdataframe-with-ggplot2


contourMay <- contours_all %>%
  filter(month == "may" & contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow))

may_colors <- rev(RColorBrewer::brewer.pal(6, "YlOrBr")[2:5])
# Map not including basemap
(map3 <- ggplot() +
    geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE) +
    geom_sf(data = WW_Delta, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
    geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourMay, aes(x = long, y = lat, group  = grouper, color= flow), size = 0.6, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("Flow", values = may_colors) +
    labs(title = "May Contour 0.75")+
    theme_classic())


## Export Map ---------------------

map3
ggsave("maps/May75_drop.jpeg", width = 7, height = 6, device = 'jpeg', dpi = 300)


##Apr Map
apr_colors <- rev(RColorBrewer::brewer.pal(6, "YlOrBr")[2:4])
# Extract out the data we want
contourApr <- contours_all %>%
  filter(month == "april" & contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow))


# Map not including basemap
(map4 <- ggplot() +
    geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE) +
    geom_sf(data = WW_Delta, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
    geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourApr, aes(x = long, y = lat, group  = grouper, color= flow), size = 0.6, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("Flow", values = apr_colors) +
    labs(title = "Apr Contour 0.75")+
    theme_classic())

map4
ggsave("maps/Apr75_drop.jpeg", width = 7, height = 6, device = 'jpeg', dpi = 300)


###Mar Map
contourMar <- contours_all %>%
  filter(month == "mar" & contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow))

mar_colors <- rev(RColorBrewer::brewer.pal(6, "YlOrBr")[2:6])
# Map not including basemap
(map5 <- ggplot() +
    geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE) +
    geom_sf(data = WW_Delta, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
    geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourMar, aes(x = long, y = lat, group  = grouper, color= flow), size = 0.6, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("Flow", values = mar_colors) +
    labs(title = "Mar Contour 0.75")+
    theme_classic())


## Export Map ---------------------

map5
ggsave("maps/Mar75_drop.jpeg", width = 7, height = 6, device = 'jpeg', dpi = 300)




# Old code
### IDW


# Separate into different files for each flow
delta_sp <- as(delta_4326, "Spatial")
month_1000_sp <- as(month_1000, "Spatial")
month_2000_sp <- as(month_2000, "Spatial")
month_3000_sp <- as(month_3000, "Spatial")
month_4000_sp <- as(month_4000, "Spatial")

# * 1000
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

##
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


# Map including basemap
(map2 <- map +
    geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE) +
    geom_sf(data = WW_Delta, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
    geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contour, aes(x = long, y = lat, group  = grouper, color= flow), size = 0.6, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_brewer("Flow", palette = "YlOrBr", direction = -1)+
    labs(title = "May Contour 0.75")+
    theme_classic())
