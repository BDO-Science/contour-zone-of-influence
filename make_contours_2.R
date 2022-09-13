
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

### Read/Join data ---------------------------------------------------------
delta <- st_read("shapefiles/Bay_Delta_Poly_New.shp")
zoi_file = list.files("data_zoi", pattern = "NAA_*", full.names = TRUE)
zoi_data <- lapply(zoi_file, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = as.numeric(id)*1000) %>%
  rename(Flow = id) %>%
  rename(DSM2 = Mar) #CHANGE THIS TO DESIRED MONTH ("Jan", "Feb", "Mar", "Apr","May")
Month <- "Mar" #CHANGE THIS TO DESIRED MONTH
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
nodes <- st_transform(nodes, crs = st_crs(delta))

### Single month only ---------------------------------------------------------------
month <- zoi_data %>% 
  dplyr::select(Flow, node, DSM2)

month_node <- inner_join(nodes, month) %>%
  mutate(DSM2 = ifelse(DSM2<0, NA, DSM2)) %>%
  na.omit()# may be some missing

### Split up into different files
month_1000 <- month_node %>% filter(Flow == 1000)
month_2000 <- month_node %>% filter(Flow == 2000)
month_3000 <- month_node %>% filter(Flow == 3000)
month_4000 <- month_node %>% filter(Flow == 4000)
month_5000 <- month_node %>% filter(Flow == 5000)
### IDW --------------------------------------
#1 https://rpubs.com/Dr_Gurpreet/interpolation_idw_R
#2 https://mgimond.github.io/Spatial/interpolation-in-r.html
#2 is where the code below comes from (I don't really understand it too well!)

# Separate into different files for each flow
delta_sp <- as(delta, "Spatial")
month_1000_sp <- as(month_1000, "Spatial")
month_2000_sp <- as(month_2000, "Spatial")
month_3000_sp <- as(month_3000, "Spatial")
month_4000_sp <- as(month_4000, "Spatial") #Note that this is not a valid flow for Apr
month_5000_sp <- as(month_5000, "Spatial") #Note that this is not a valid flow for Apr, May

# * 1000 ----------------------------------------
# Create grid based on bounding box of data
grd1 <- as.data.frame(spsample(month_1000_sp, "regular", n = 50000)) #I tried using a single grid, but it does slightly change things, so I do think we need to have new grids+ .
names(grd1)       <- c("X", "Y")
coordinates(grd1) <- c("X", "Y")
gridded(grd1)     <- TRUE  # Create SpatialPixel object
fullgrid(grd1)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_1000_sp) <- proj4string(month_1000_sp) # Temp fix until new proj env is adopted
proj4string(grd1) <- proj4string(month_1000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_1000_sp, newdata=grd1, idp=2.0)

# Convert to raster object then clip to delta
r1       <- raster(month.idw)
r.m1     <- mask(r1, delta_sp)

# Plot
current.mode <- tmap_mode("plot")
tm_shape(r.m1) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_1000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

tm_shape(r.m1) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_1000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# Plot
# * 2000 ----------------------------------------
grd2 <- as.data.frame(spsample(month_2000_sp, "regular", n = 50000))
names(grd2)       <- c("X", "Y")
coordinates(grd2) <- c("X", "Y")
gridded(grd2)     <- TRUE  # Create SpatialPixel object
fullgrid(grd2)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_2000_sp) <- proj4string(month_2000_sp) # Temp fix until new proj env is adopted
proj4string(grd2) <- proj4string(month_2000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_2000_sp, newdata=grd2, idp=2.0)

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
grd3 <- as.data.frame(spsample(month_3000_sp, "regular", n = 50000))
names(grd3)       <- c("X", "Y")
coordinates(grd3) <- c("X", "Y")
gridded(grd3)     <- TRUE  # Create SpatialPixel object
fullgrid(grd3)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_3000_sp) <- proj4string(month_3000_sp) # Temp fix until new proj env is adopted
proj4string(grd3) <- proj4string(month_3000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_3000_sp, newdata=grd3, idp=2.0)

# Convert to raster object then clip to Texas
r3       <- raster(month.idw)
r.m3     <- mask(r3, delta_sp)

# Plot
tm_shape(r.m3) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_3000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# * 4000 ----------------------------------------
# Create grid based on bounding box of data
grd4 <- as.data.frame(spsample(month_4000_sp, "regular", n = 50000))
names(grd4)       <- c("X", "Y")
coordinates(grd4) <- c("X", "Y")
gridded(grd4)     <- TRUE  # Create SpatialPixel object
fullgrid(grd4)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_4000_sp) <- proj4string(month_4000_sp) # Temp fix until new proj env is adopted
proj4string(grd4) <- proj4string(month_4000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_4000_sp, newdata=grd4, idp=2.0)

# Convert to raster object then clip to Texas
r4       <- raster(month.idw)
r.m4     <- mask(r4, delta_sp)

# Plot
tm_shape(r.m4) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_4000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


# * 5000 ---------------------------------------- 
# Create grid based on bounding box of data
grd5 <- as.data.frame(spsample(month_5000_sp, "regular", n = 50000))
names(grd5)       <- c("X", "Y")
coordinates(grd5) <- c("X", "Y")
gridded(grd5)     <- TRUE  # Create SpatialPixel object
fullgrid(grd5)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(month_5000_sp) <- proj4string(month_5000_sp) # Temp fix until new proj env is adopted
proj4string(grd5) <- proj4string(month_5000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
month.idw <- gstat::idw(DSM2 ~ 1, month_5000_sp, newdata=grd5, idp=2.0)

# Convert to raster object then clip to Texas
r5       <- raster(month.idw)
r.m5     <- mask(r5, delta_sp)

# Plot
tm_shape(r.m5) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(month_5000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

### All Contours ------------------------------------

# Did not figure out how to label this
contour1 <- rasterToContour(r.m1)
plot(contour1)

contour2 <- rasterToContour(r.m2)
plot(contour2)

contour3 <- rasterToContour(r.m3)
plot(contour3)

contour4 <- rasterToContour(r.m4)
plot(contour4)

contour5 <- rasterToContour(r.m5)
plot(contour5)

# Plot
#This map has basemap, A map for zooming in (note that in view mode you can only have fill legends)
current.mode <- tmap_mode("view")
opts <- tmap_options(basemaps = c(Canvas = "Esri.WorldImagery", Imagery = "Esri.WorldImagery"))# Use tmap options to set the basemap and overlay map permanently during the R session:

tm_shape(contour1) + 
  tm_lines(col = "level") +
  tm_shape(month_1000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_1000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour2) + 
  tm_lines(col = "level") +
  tm_shape(month_2000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_2000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour3) + 
  tm_lines(col = "level") +
  tm_shape(month_3000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_3000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour4) + 
  tm_lines(col = "level") +
  tm_shape(month_4000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_4000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour5) + 
  tm_lines(col = "level") +
  tm_shape(month_5000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_5000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

### Best Contour ------------------------------------

#This map shows single best contour (no raster)
contour1_7 <- rasterToContour(r.m1, levels = c(0.7))
contour2_7 <- rasterToContour(r.m2, levels = c(0.7))
contour3_7 <- rasterToContour(r.m3, levels = c(0.7))
contour4_7 <- rasterToContour(r.m4, levels = c(0.7))
contour5_7 <- rasterToContour(r.m5, levels = c(0.7))

current.mode <- tmap_mode("view")

#Maps with basemap
tm_shape(contour2_7) + 
  tm_lines(col = "level") +
  tm_shape(month_2000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_2000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour3_7) + 
  tm_lines(col = "level") +
  tm_shape(month_3000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_3000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour4_7) + 
  tm_lines(col = "level") +
  tm_shape(month_4000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_4000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)

tm_shape(contour5_7) + 
  tm_lines(col = "level") +
  tm_shape(month_5000_sp) + tm_dots(col = "DSM2", size=0.01) + 
  tm_basemap() +
  tm_layout(title = paste0(Month, " -", month_5000_sp$Flow[1], " OMR Flow")) +
  tm_legend(legend.outside=TRUE)


