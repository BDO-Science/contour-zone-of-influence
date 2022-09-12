
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
  rename(Flow = id)
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
nodes <- st_transform(nodes, crs = st_crs(delta))

### May only ---------------------------------------------------------------
may <- zoi_data %>% dplyr::select(Flow, node, May)
may_node <- inner_join(nodes, may) %>%
  mutate(May = ifelse(May<0, NA, May)) %>%
  na.omit()# may be some missing

### Split up into different files
may_1000 <- may_node %>% filter(Flow == 1000)
may_2000 <- may_node %>% filter(Flow == 2000)
may_3000 <- may_node %>% filter(Flow == 3000)
may_4000 <- may_node %>% filter(Flow == 4000)

### IDW --------------------------------------
#1 https://rpubs.com/Dr_Gurpreet/interpolation_idw_R
#2 https://mgimond.github.io/Spatial/interpolation-in-r.html
     #2 is where the code below comes from (I don't really understand it too well!)

# Separate into different files for each flow
delta_sp <- as(delta, "Spatial")
may_1000_sp <- as(may_1000, "Spatial")
may_2000_sp <- as(may_2000, "Spatial")
may_3000_sp <- as(may_3000, "Spatial")
may_4000_sp <- as(may_4000, "Spatial")

# * 1000 ----------------------------------------
# Create grid based on bounding box of data
grd <- as.data.frame(spsample(may_1000_sp, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(may_1000_sp) <- proj4string(may_1000_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(may_1000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
may.idw <- gstat::idw(May ~ 1, may_1000_sp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r1       <- raster(may.idw)
r.m1     <- mask(r1, delta_sp)

# Plot
tm_shape(r.m1) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(may_1000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# * 2000 ----------------------------------------
# Create grid based on bounding box of data
grd <- as.data.frame(spsample(may_2000_sp, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(may_2000_sp) <- proj4string(may_2000_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(may_2000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
may.idw <- gstat::idw(May ~ 1, may_2000_sp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r2      <- raster(may.idw)
r.m2    <- mask(r2, delta_sp)

# Plot
tm_shape(r.m2) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(may_2000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# * 3000 ----------------------------------------
# Create grid based on bounding box of data
grd <- as.data.frame(spsample(may_3000_sp, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add projection information to the empty grid
proj4string(may_3000_sp) <- proj4string(may_3000_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(may_3000_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
may.idw <- gstat::idw(May ~ 1, may_3000_sp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r3       <- raster(may.idw)
r.m3     <- mask(r3, delta_sp)

# Plot
tm_shape(r.m3) +
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Data") +
  tm_shape(may_3000_sp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

### Contours ------------------------------------

# Did not figure out how to label this
contour1 <- rasterToContour(r.m1)
plot(contour1)

contour2 <- rasterToContour(r.m2)
plot(contour2)

contour3 <- rasterToContour(r.m3)
plot(contour3)










### You can ignore this, I was trying to rasterize the data, but I don't think I actually have to
### Still leaving it in for now.

### Rasterize


Rasterize_all <- function(data, var, n=500){
  var<-rlang::enquo(var)
  rlang::as_name(var)
  preds<-purrr::map(unique(data$Flow),
             function(x) st_rasterize(data%>%
                                        filter(Flow==x)%>%
                                        dplyr::select(!!var),
                                      template=st_as_stars(st_bbox(delta),
                                    dx=diff(st_bbox(delta)[c(1, 3)])/n,
                                    dy=diff(st_bbox(delta)[c(2, 4)])/n,
                                    values = NA_real_)))
  return(preds)
  # Then bind all Anomalies together into 1 raster
  out <- exec(c, !!!preds, along=list(Anomaly=unique(data$Anomaly)))
  return(out)
}

raster_data <- Rasterize_all(may_node, May)

# raster_data2 <- st_rasterize(may_node$May,
#                             template = st_as_stars(st_bbox(delta)),
#                             dx=diff(st_bbox(delta)[c(1, 3)])/n,
#                             dy=diff(st_bbox(delta)[c(2, 4)])/n,
#                             values = NA_real_)



(raster_data$May)

summary(raster_data)
min(raster_data)
plot(raster_data[[1]])
raster_data[1]



### Contour

contour_data <- st_contour(raster_data,
                           na.rm = TRUE,
                           contour_lines = TRUE,
                           breaks = 0.10)

plot(contour_data)
