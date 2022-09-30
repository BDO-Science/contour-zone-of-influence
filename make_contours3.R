#### make_contours.R ######
#### Catarina Pien and Lisa Elliott

# This code uses zone of influence modeling results (DSM2) to create contours showing
#  how zone of influence changes from operational facilities based on pumping
#  Contour lines of a specific level are then compared between different flow levels
#  to indicate how changing OMR will influence the zone of influence.

# Packages
# general
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

# Change all to 4326 (WGS)
delta_4326 <- st_transform(delta, crs = 4326)
nodes_4326 <- st_transform(nodes, crs = st_crs(delta_4326))
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY")
plot(WW_Delta_crop)

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

# Map making function
# @mon = month, lower case (feb, mar, apr, may)
# @clevel = contour level (0.75, 0.95)
# produces map

mon = "apr"
clevel = 0.75

make_map <- function(mon, clevel){

  # Make a dataset that filters contours for month and contour level of interest. This needs to run
  # after you have already made the contours_all file (run all the contours and combine)
  contourMonth <- contours_all %>%
    filter(month == mon & contour == clevel) %>%
    mutate(grouper = paste0(group, "_", flow))

  # Map not including basemap
  (ggplot() +
      geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
      geom_sf(data = WW_Delta_crop, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
      geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
      geom_path(data = contourMonth, aes(x = long, y = lat, group  = grouper, color= flow), size = 0.6, inherit.aes = FALSE) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
      scale_color_manual("Flow (cfs)", values = cpal[c(1,2,3,4,5)]) +
      labs(title = paste(mon, "contour", clevel))+
      theme_classic())
}


# Analysis ------------------------------
## Create data frames --------------------

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

### February --------------
feb_1000_sp <- create_df(month = "Feb", flow = 1000)
feb_2000_sp <- create_df(month = "Feb", flow = 2000)
feb_3000_sp <- create_df(month = "Feb", flow = 3000)
feb_4000_sp <- create_df(month = "Feb", flow = 4000)
feb_5000_sp <- create_df(month = "Feb", flow = 5000)

### January --------------
jan_1000_sp <- create_df(month = "Jan", flow = 1000)
jan_2000_sp <- create_df(month = "Jan", flow = 2000)
jan_3000_sp <- create_df(month = "Jan", flow = 3000)
jan_4000_sp <- create_df(month = "Jan", flow = 4000)
jan_5000_sp <- create_df(month = "Jan", flow = 5000)

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

### February --------------
r.feb1 <- interp_nodes(feb_1000_sp)
r.feb2 <- interp_nodes(feb_2000_sp)
r.feb3 <- interp_nodes(feb_3000_sp)
r.feb4 <- interp_nodes(feb_4000_sp)
r.feb5 <- interp_nodes(feb_5000_sp)

### February --------------
r.jan1 <- interp_nodes(jan_1000_sp)
r.jan2 <- interp_nodes(jan_2000_sp)
r.jan3 <- interp_nodes(jan_3000_sp)
r.jan4 <- interp_nodes(jan_4000_sp)
r.jan5 <- interp_nodes(jan_5000_sp)

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
  mutate(month = "apr") %>%
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

### February --------------------------
c.feb1_95 <- rasterToContour(r.feb1, levels = 0.95)
plot(c.feb1_95)
c.feb1_75 <- rasterToContour(r.feb1, levels = 0.75)
plot(c.feb1_75)

c.feb2_95 <- rasterToContour(r.feb2, levels = 0.95)
c.feb2_75 <- rasterToContour(r.feb2, levels = 0.75)

c.feb3_95 <- rasterToContour(r.feb3, levels = 0.95)
c.feb3_75 <- rasterToContour(r.feb3, levels = 0.75)

c.feb4_95 <- rasterToContour(r.feb4, levels = 0.95)
c.feb4_75 <- rasterToContour(r.feb4, levels = 0.75)

c.feb5_95 <- rasterToContour(r.feb5, levels = 0.95)
c.feb5_75 <- rasterToContour(r.feb5, levels = 0.75)

contours_f <- c(c.feb1_75, c.feb1_95, c.feb2_75, c.feb2_95,
                 c.feb3_75, c.feb3_95, c.feb4_75, c.feb4_95,
                 c.feb5_75, c.feb5_95)

contours_feb <- lapply(contours_f, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(month = "feb") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -1000,
                          id %in% c(3,4) ~ -2000,
                          id %in% c(5,6) ~ -3000,
                          id %in% c(7,8) ~ -4000,
                          id %in% c(9,10) ~ -5000),
         contour = case_when(id %in% c(1,3, 5,7, 9) ~ 0.75,
                             id %in% c(2,4,6,8, 10) ~ 0.95))

###January --------------------------
c.jan1_95 <- rasterToContour(r.jan1, levels = 0.95)
plot(c.jan1_95)
c.jan1_75 <- rasterToContour(r.jan1, levels = 0.75)
plot(c.jan1_75)

c.jan2_95 <- rasterToContour(r.jan2, levels = 0.95)
c.jan2_75 <- rasterToContour(r.jan2, levels = 0.75)

c.jan3_95 <- rasterToContour(r.jan3, levels = 0.95)
c.jan3_75 <- rasterToContour(r.jan3, levels = 0.75)

c.jan4_95 <- rasterToContour(r.jan4, levels = 0.95)
c.jan4_75 <- rasterToContour(r.jan4, levels = 0.75)

c.jan5_95 <- rasterToContour(r.jan5, levels = 0.95)
c.jan5_75 <- rasterToContour(r.jan5, levels = 0.75)

contours_j <- c(c.jan1_75, c.jan1_95, c.jan2_75, c.jan2_95,
                 c.jan3_75, c.jan3_95, c.jan4_75, c.jan4_95,
                 c.jan5_75, c.jan5_95)

contours_jan <- lapply(contours_j, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(month = "jan") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -1000,
                          id %in% c(3,4) ~ -2000,
                          id %in% c(5,6) ~ -3000,
                          id %in% c(7,8) ~ -4000,
                          id %in% c(9,10) ~ -5000),
         contour = case_when(id %in% c(1,3, 5,7, 9) ~ 0.75,
                             id %in% c(2,4,6,8, 10) ~ 0.95))



### Combine all
# switching factor levels will allow color palette to work right
contours_all <- rbind(contours_april, contours_may, contours_mar, contours_feb, contours_jan) %>%
  mutate(flow = factor(flow, levels = c("-1000", "-2000", "-3000", "-4000", "-5000")))

# Make map -----------------------

# https://stackoverflow.com/questions/34153462/plot-spatiallinesdataframe-with-ggplot2

# Define color palette
cpal <- RColorBrewer::brewer.pal(6, "YlOrBr")[2:6]

# Run make_map function to make maps (see documentation above)
(map_may_75 <- make_map(mon = "may", clevel = 0.75))
(map_apr_75 <- make_map(mon = "apr", clevel = 0.75))
(map_mar_75 <- make_map(mon = "mar", clevel = 0.75))
(map_feb_75 <- make_map(mon = "feb", clevel = 0.75))
(map_jan_75 <- make_map(mon = "jan", clevel = 0.75))

(map_may_95 <- make_map(mon = "may", clevel =  0.95))
(map_apr_95 <- make_map(mon = "apr", clevel =  0.95))
(map_mar_95 <- make_map(mon = "mar", clevel =  0.95))
(map_feb_95 <- make_map(mon = "feb", clevel =  0.95))
(map_jan_95 <- make_map(mon = "jan", clevel =  0.95))


# Export Maps ---------------------

map_jan_75
ggsave("maps/Jan75.png", width = 6, height = 6, device = 'png', dpi = 300)


































# Old code


contourMay <- contours_all %>%
  filter(month == "may" & contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow))



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


## Export Map

map5
ggsave("maps/Mar75_drop.jpeg", width = 7, height = 6, device = 'jpeg', dpi = 300)



# Month <- "May" #CHANGE THIS TO DESIRED MONTH

## Join nodes with zoi data
# month <- zoi_data %>% dplyr::select(Flow, node, Month) %>%
#   rename(DSM2 = May)
# month_node <- inner_join(nodes_4326, month) %>%
#   mutate(DSM2 = ifelse(DSM2<0, NA, DSM2)) %>%
#   na.omit()# may be some missing


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

## Make basemap (ggmap)

coords <- data.frame(st_coordinates(delta_4326))

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

leg.text <- c("Delta Boundary", "Waterways", "DSM2 Nodes")
plot(delta_4326) + 
legend("bottomleft",leg.text, pch = c(NA, 15, 16),
       col=c("gray30", "lightskyblue2", "black"), 
       lty= c("dashed", NA, NA), cex=0.8, xpd = TRUE, horiz = TRUE)
  
  ggplot() +
  geom_sf(data = delta_4326, aes(geometry = geometry, color = "Black"), 
          fill = NA, linetype = "dashed", show.legend = "line") +
  geom_sf(data = WW_Delta_crop, aes(geometry = geometry, fill = "TYPE"), 
          fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, show.legend = "polygon", inherit.aes = FALSE) +
  geom_sf(data = nodes_4326, aes(geometry = geometry), size = 0.4, color = "gray30", show.legend = "point", inherit.aes = FALSE) +
  #geom_path(data = contourMonth, aes(x = long, y = lat, group  = grouper, color= flow), size = 0.6, inherit.aes = FALSE) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
  #scale_color_manual("Legend", values = cpal[c(1,2,3,4,5)]) +
  scale_color_manual("Legend", values = c("black", "lightskyblue2", "gray30"), labels = c("Delta Boundary", "Waterways", "DSM2 Nodes"))
scale_linetype_manual("Legend", values = c("dashed", NA, NA)) +
  labs(title = paste(mon, "contour", clevel)) +
  theme_classic()
