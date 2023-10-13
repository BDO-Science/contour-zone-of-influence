# contour_maps_inflow.R ######
#### Catarina Pien and Lisa Elliott

#  This code uses zone of influence modeling results (DSM2) to create contours showing
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
library(janitor)

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
zoi_file_NAA = list.files("data_raw/zoi/", pattern = "NAA_.*csv$", full.names = TRUE)
#zoi_file_D1641
zoi_data <- lapply(zoi_file_NAA, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_NAA[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow))
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
channels0 <- read_csv("data_raw/Reclamation_2021LTO_DSM2_Version806_ChannelLengths.csv") %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no)

# Drop nodes that are causing issues
dropNodes <- c(146, 147, 148, 206, 242, 246, 432, 433, 434)
# Drop duplicate channels
channels1 <- channels0[!channels0$upnode %in% dropNodes, ]
channels <- channels1[!channels1$downnode %in% dropNodes, ]
total_channel_length <- sum(channels$length_feet)

# Join channel lengths with zoi data
#zoi_channel <- left_join(zoi_data, nodes)
zoi_channel <- left_join(zoi_data, channels)
zoi_channel <- merge(zoi_data, channels, by = "channel_number")

# Change projections to 4326 (WGS)
delta_4326 <- st_transform(delta, crs = 4326) %>%
  mutate(line = "analysis boundary")
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY")
plot(WW_Delta_crop)

# Convert data frame to long
zoi_channel_long <- zoi_channel %>%
  pivot_longer(cols = c(lolo:hihi), names_to = "group", values_to = "overlap")

# Look at data
summary_vals <- zoi_channel_long %>%
  filter(overlap>0) %>%
  group_by(group, OMR_Flow) %>%
  summarize(min = min(overlap),
            max = max(overlap),
            mean = mean(overlap)) %>%
  ungroup()
ggplot(summary_vals) +
  geom_col(aes(x = OMR_Flow, y = mean, fill = OMR_Flow)) + facet_wrap(~group)+
  scale_fill_viridis_d()
ggplot(zoi_channel_long %>% filter(overlap>=0)) +
  geom_jitter(aes(x = OMR_Flow, y = overlap, color = node)) + facet_wrap(~group)
ggplot(zoi_channel_long %>% filter(overlap>=0)) +
  geom_violin(aes(x = OMR_Flow, y = overlap, fill = OMR_Flow)) + facet_wrap(~group) +
  scale_fill_viridis_d()


# Functions ------------------------------------

# Create data frame for each month and flow level
# @group = inflow group
# @flow = OMR flow group
# produces an "sp" object
create_df <- function(groupname, flow) {
  group_data <- zoi_channel_long %>%
    dplyr::select(OMR_Flow, node, channel_number, length_feet, upnode, downnode, group, overlap)
  group_node <- inner_join(nodes_4326, group_data) %>%
    mutate(overlap = ifelse(overlap<0, NA, overlap)) %>%
    na.omit()# may be some missing

  df_filtered <- group_node %>% filter(OMR_Flow == flow,
                                       group == groupname)
  df_filt_sp <- as(df_filtered, "Spatial")

  # return(df_filtered)
  return(df_filt_sp)
}

# create spatial interpolations of proportion overlap from data frame within delta mask
# @df = data frame
# @mask = spatial shapefile mask
# produces a raster
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
  group.idw <- gstat::idw(overlap ~ 1, df, newdata=grd, idp=2.0)

  # Convert to raster object then clip to Delta
  raster       <- raster(group.idw)
  raster_mask     <- mask(raster, mask)

  return(raster_mask)

}

# Contour making function
# @inflow_list = list of inflow rasters
# @inflow_group = inflow group name (e.g. lolo)
# combines contours for one inflow group into one place
# arranges contours in the correct order and groups by contour level
# add OMR names back in
# # produces data frame of contour lines
create_contour <- function(inflow_list, inflow_group) {
  c1 <- mapply(rasterToContour, inflow_list, levels = 0.75)
  c2 <- mapply(rasterToContour, inflow_list, levels = 0.95)
  cont <- rbind(c1, c2)
  contours_lolo <- lapply(cont, fortify) %>%
    bind_rows(.id = "id") %>%
    mutate(group2 = inflow_group) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-2000",
                            id %in% c(3,4) ~ "-3500",
                            id %in% c(5,6) ~ "-5000",
                            id %in% c(7,8) ~ "<-5500"),
           contour = case_when(id %in% c(1,3, 5, 7) ~ 0.75,
                               id %in% c(2,4,6, 8) ~ 0.95))
}

# Map making function
# @group = inflow group
# @clevel = proportion overlap contour level (0.75, 0.25)
# produces map
make_map <- function(grp, clevel){

  # Make a dataset that filters contours for group and contour level of interest. This needs to run
  # after you have already made the contours_all file (run all the contours and combine)
  contourGroup <- contours_all %>%
    filter(group2 == grp & contour == clevel) %>%
    mutate(grouper = paste0(group, "_", flow))
  # Map not including basemap
  (ggplot() +
      geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
      geom_sf(data = WW_Delta_crop, fill = "lightskyblue2", color = "lightskyblue2", alpha = 0.7, inherit.aes = FALSE) +
      geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
      geom_path(data = contourGroup, aes(x = long, y = lat, group  = grouper, color= OMR_flow), linewidth = 1, inherit.aes = FALSE) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
      scale_color_manual("OMR Flow (cfs)", values = cpal[c(1,2,3,4,5)]) +
      labs(title = paste(grp, "contour", clevel))+
      theme_classic())
}

# Analysis ------------------------------

## Create data frames for each inflow-OMR group --------------------

### lolo --------------
lolo_2000_sp <- create_df(groupname = "lolo", flow = "-2000")
lolo_3500_sp <- create_df(groupname = "lolo", flow = "-3500")
lolo_5000_sp <- create_df(groupname = "lolo", flow = "-5000")
lolo_5500_sp <- create_df(groupname = "lolo", flow = "<-5500")

### lomed --------------
lomed_2000_sp <- create_df(groupname = "lomed", flow = "-2000")
lomed_3500_sp <- create_df(groupname = "lomed", flow = "-3500")
lomed_5000_sp <- create_df(groupname = "lomed", flow = "-5000")
lomed_5500_sp <- create_df(groupname = "lomed", flow = "<-5500") # error

### lohi --------------
lohi_2000_sp <- create_df(groupname = "lohi", flow = "-2000")
lohi_3500_sp <- create_df(groupname = "lohi", flow = "-3500")
lohi_5000_sp <- create_df(groupname = "lohi", flow = "-5000")
lohi_5500_sp <- create_df(groupname = "lohi", flow = "<-5500") # error

### medlo --------------
medlo_2000_sp <- create_df(groupname = "medlo", flow = "-2000")
medlo_3500_sp <- create_df(groupname = "medlo", flow = "-3500")
medlo_5000_sp <- create_df(groupname = "medlo", flow = "-5000")
medlo_5500_sp <- create_df(groupname = "medlo", flow = "<-5500")

### medmed --------------
medmed_2000_sp <- create_df(groupname = "medmed", flow = "-2000")
medmed_3500_sp <- create_df(groupname = "medmed", flow = "-3500")
medmed_5000_sp <- create_df(groupname = "medmed", flow = "-5000")
medmed_5500_sp <- create_df(groupname = "medmed", flow = "<-5500")

### medhi --------------
medhi_2000_sp <- create_df(groupname = "medhi", flow = "-2000")
medhi_3500_sp <- create_df(groupname = "medhi", flow = "-3500")
medhi_5000_sp <- create_df(groupname = "medhi", flow = "-5000")
medhi_5500_sp <- create_df(groupname = "medhi", flow = "<-5500")

### hilo --------------
hilo_2000_sp <- create_df(groupname = "hilo", flow = "-2000") # error
hilo_3500_sp <- create_df(groupname = "hilo", flow = "-3500")
hilo_5000_sp <- create_df(groupname = "hilo", flow = "-5000")
hilo_5500_sp <- create_df(groupname = "hilo", flow = "<-5500")

### himed --------------
himed_2000_sp <- create_df(groupname = "himed", flow = "-2000")
himed_3500_sp <- create_df(groupname = "himed", flow = "-3500")
himed_5000_sp <- create_df(groupname = "himed", flow = "-5000")
himed_5500_sp <- create_df(groupname = "himed", flow = "<-5500")

### hihi --------------
hihi_2000_sp <- create_df(groupname = "hihi", flow = "-2000")
hihi_3500_sp <- create_df(groupname = "hihi", flow = "-3500")
hihi_5000_sp <- create_df(groupname = "hihi", flow = "-5000")
hihi_5500_sp <- create_df(groupname = "hihi", flow = "<-5500")

## Create interpolations (takes a little bit) ------------------

# Turn delta shapefile into spatial polygon object
delta_sp <- as(delta_4326, "Spatial")

r.lolo1 <- interp_nodes(lolo_2000_sp)
r.lolo2 <- interp_nodes(lolo_3500_sp)
r.lolo3 <- interp_nodes(lolo_5000_sp)
r.lolo4 <- interp_nodes(lolo_5500_sp)

r.lomed1 <- interp_nodes(lomed_2000_sp)
r.lomed2 <- interp_nodes(lomed_3500_sp)
r.lomed3 <- interp_nodes(lomed_5000_sp)
r.lomed4 <- interp_nodes(lomed_5500_sp) # does not exist

r.lohi1 <- interp_nodes(lohi_2000_sp)
r.lohi2 <- interp_nodes(lohi_3500_sp)
r.lohi3 <- interp_nodes(lohi_5000_sp)
r.lohi4 <- interp_nodes(lohi_5500_sp) # does not exist

r.medlo1 <- interp_nodes(medlo_2000_sp)
r.medlo2 <- interp_nodes(medlo_3500_sp)
r.medlo3 <- interp_nodes(medlo_5000_sp)
r.medlo4 <- interp_nodes(medlo_5500_sp)

r.medmed1 <- interp_nodes(medmed_2000_sp)
r.medmed2 <- interp_nodes(medmed_3500_sp)
r.medmed3 <- interp_nodes(medmed_5000_sp)
r.medmed4 <- interp_nodes(medmed_5500_sp)

r.medhi1 <- interp_nodes(medhi_2000_sp)
r.medhi2 <- interp_nodes(medhi_3500_sp)
r.medhi3 <- interp_nodes(medhi_5000_sp)
r.medhi4 <- interp_nodes(medhi_5500_sp)

r.hilo1 <- interp_nodes(hilo_2000_sp) # does not exist
r.hilo2 <- interp_nodes(hilo_3500_sp)
r.hilo3 <- interp_nodes(hilo_5000_sp)
r.hilo4 <- interp_nodes(hilo_5500_sp)

r.himed1 <- interp_nodes(himed_2000_sp)
r.himed2 <- interp_nodes(himed_3500_sp)
r.himed3 <- interp_nodes(himed_5000_sp)
r.himed4 <- interp_nodes(himed_5500_sp)

r.hihi1 <- interp_nodes(hihi_2000_sp)
r.hihi2 <- interp_nodes(hihi_3500_sp)
r.hihi3 <- interp_nodes(hihi_5000_sp)
r.hihi4 <- interp_nodes(hihi_5500_sp)

## Create contours --------------------------

# 0.25 represents contour at which 25% overlap exists (less similar)
# 0.75 represents contour at which 75% overlap exists (more similar)

### make list of contours for each inflow group ------
lolo_list <- c(r.lolo1, r.lolo2, r.lolo3, r.lolo4)
lomed_list <- c(r.lomed1, r.lomed2, r.lomed3) # missing <-5500
lohi_list <- c(r.lohi1, r.lohi2, r.lohi3) # missing <-5500
medlo_list <- c(r.medlo1, r.medlo2, r.medlo3, r.medlo4)
medmed_list <- c(r.medmed1, r.medmed2, r.medmed3, r.medmed4)
medhi_list <- c(r.medhi1, r.medhi2, r.medhi3, r.medhi4)
hilo_list <- c(r.hilo2, r.hilo3, r.hilo4) # missing -2000
himed_list <- c(r.himed1, r.himed2, r.himed3, r.himed4)
hihi_list <- c(r.hihi1, r.hihi2, r.hihi3, r.hihi4)

### create contours ------
contours_lolo <- create_contour(lolo_list, inflow_group = "lolo")
contours_lomed <- create_contour(lomed_list, inflow_group = "lomed")
contours_lohi <- create_contour(lohi_list, inflow_group = "lohi")
contours_medlo <- create_contour(medlo_list, inflow_group = "medlo")
contours_medmed <- create_contour(medmed_list, inflow_group = "medmed")
contours_medhi <- create_contour(medhi_list, inflow_group = "medhi")
# this one is different because there are no examples at -2000.
contours_hilo <- create_contour(hilo_list, inflow_group = "hilo") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ "-3500",
                          id %in% c(3,4) ~ "-5000",
                          id %in% c(5,6) ~ "<-5500"))
contours_himed <- create_contour(himed_list, inflow_group = "himed")
contours_hihi <- create_contour(hihi_list, inflow_group = "hihi")

### Combine all contours ------------
# switching factor levels will allow color palette to work right
contours_all <- rbind(contours_lolo, contours_lomed, contours_lohi,
                      contours_medlo, contours_medmed, contours_medhi,
                      contours_hilo, contours_himed, contours_hihi) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

# Make maps -----------------------

# https://stackoverflow.com/questions/34153462/plot-spatiallinesdataframe-with-ggplot2
# Define color palette
cpal <- RColorBrewer::brewer.pal(6, "YlOrBr")[2:6]
library(randomcoloR)
n <- 15
palette <- distinctColorPalette(n)

# Run make_map function to make maps (see documentation above)
(map_A_75 <- make_map(grp = "lolo", clevel = 0.75))
(map_B_75 <- make_map(grp = "lomed", clevel = 0.75))
(map_C_75 <- make_map(grp = "lohi", clevel = 0.75))
(map_D_75 <- make_map(grp = "medlo", clevel = 0.75))
(map_E_75 <- make_map(grp = "medmed", clevel = 0.75))
(map_E_75 <- make_map(grp = "medmed", clevel = 0.75))
(map_E_75 <- make_map(grp = "medmed", clevel = 0.75))
(map_E_75 <- make_map(grp = "medmed", clevel = 0.75))
(map_E_75 <- make_map(grp = "medmed", clevel = 0.75))
(map_E_75 <- make_map(grp = "medmed", clevel = 0.75))

(map_A_25 <- make_map(grp = "A", clevel =  0.25))
(map_B_25 <- make_map(grp = "B", clevel =  0.25))
(map_C_25 <- make_map(grp = "C", clevel =  0.25))
(map_D_25 <- make_map(grp = "D", clevel =  0.25))
(map_E_25 <- make_map(grp = "E", clevel =  0.25))

## 0.75 ---------------
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")

# Make one file with all inflow groups
contourGroup <- contours_all %>%
  filter(contour == 0.25) %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow)) %>%
  rename(Inflow = group2) %>%
  mutate(Inflow = factor(Inflow, levels = inflow_order))

### Map of all --------
(map_95 <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup, aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(33)) +
    labs(title = "0.95 Contours")+
    theme_classic())

### Faceted maps ---------
(map_95_f <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    # geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourGroup, aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    # annotation_north_arrow(location = "tr", which_north = "true",
    #                        pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    # annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    facet_wrap(~Inflow) +
    ylim(c(37.6, 38.3)) +
    xlim(c(-122.2, -121.2)) +
    scale_color_viridis_d() +
    labs(title = "0.25 Contours")+
    theme_classic() +
    theme(axis.text = element_blank()))

### Individual facets ------------
(map_95_flolo <- ggplot() +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup %>% filter(Inflow=="lolo"), aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    ylim(c(37.6, 38.3)) +
    xlim(c(-122.2, -121.2)) +
    scale_color_viridis_d("OMR Flow (cfs)") +
    labs(title = paste0("0.95 Contour\n", contourGroup$Inflow))+
    theme_classic())

## 0.75 ---------

contourGroup2 <- contours_all %>%
  filter(contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow))%>%
  rename(Inflow = group2) %>%
  mutate(Inflow = factor(Inflow, levels = inflow_order))

### Map of all ----------
(map_75 <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup2, aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(33)) +
    labs(title = "0.75 Contours")+
    theme_classic())

### Faceted map -----------
(map_75_f <- ggplot() +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup2, aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    facet_wrap(~Inflow) +
    ylim(c(37.7, 38.1)) +
    xlim(c(-121.8, -121.2)) +
    scale_color_viridis_d("OMR Flow (cfs)") +
    labs(title = "0.75 Contours")+
    theme_classic()+
    theme(axis.text = element_blank()))

### Individual facets by inflow group --------------
(map_75_flolo <- ggplot() +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup2 %>% filter(Inflow=="lolo"), aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    ylim(c(37.7, 38.1)) +
    xlim(c(-121.8, -121.2)) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(15)[1:3]) +
    labs(title = paste0("0.75 Contour\n", contourGroup2$Inflow))+
    theme_classic())


# Export Maps ---------------------
map_95_f
ggsave("figures/Contours_95_allgroups_facet_new.png", width = 9, height = 7, device = 'png', dpi = 300)

map_75_f
ggsave("figures/Contours_75_allgroups_facet_new.png", width = 9, height = 7, device = 'png', dpi = 300)










map_75
ggsave("figures/Contours_75_allgroups_.png", width = 7, height = 6, device = 'png', dpi = 300)



map_75_fA
ggsave("figures/Contours_75_allgroups_facetA.png", width = 9, height = 6, device = 'png', dpi = 300)

map_75_fB
ggsave("figures/Contours_75_allgroups_facetB.png", width = 9, height = 6, device = 'png', dpi = 300)

map_75_fC
ggsave("figures/Contours_75_allgroups_facetC.png", width = 9, height = 6, device = 'png', dpi = 300)

map_75_fD
ggsave("figures/Contours_75_allgroups_facetD.png", width = 9, height = 6, device = 'png', dpi = 300)

map_75_fE
ggsave("figures/Contours_75_allgroups_facetE.png", width = 9, height = 6, device = 'png', dpi = 300)


map_25
ggsave("figures/Contours_25_allgroups.png", width = 7, height = 6, device = 'png', dpi = 300)

map_25_f
ggsave("figures/Contours_25_allgroups_facet.png", width = 9, height = 6, device = 'png', dpi = 300)

map_25_fA
ggsave("figures/Contours_25_allgroups_facetA.png", width = 9, height = 6, device = 'png', dpi = 300)

map_25_fB
ggsave("figures/Contours_25_allgroups_facetB.png", width = 9, height = 6, device = 'png', dpi = 300)

map_25_fC
ggsave("figures/Contours_25_allgroups_facetC.png", width = 9, height = 6, device = 'png', dpi = 300)

map_25_fD
ggsave("figures/Contours_25_allgroups_facetD.png", width = 9, height = 6, device = 'png', dpi = 300)

map_25_fE
ggsave("figures/Contours_25_allgroups_facetE.png", width = 9, height = 6, device = 'png', dpi = 300)
