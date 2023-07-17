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
zoi_file_NAA = list.files("data_inflow", pattern = "NAA_.*csv$", full.names = TRUE)
#zoi_file_D1641
zoi_data <- lapply(zoi_file_NAA, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_NAA[as.numeric(id)], 36, 39))) %>%
  rename(OMR_Flow = id)
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
channels0 <- read_csv("data_updatedDSM2/Reclamation_2021LTO_DSM2_Version806_ChannelLengths.csv") %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no)

# Drop nodes that are causing issues
dropNodes <- c(146, 147, 148, 206, 242, 246, 432, 433, 434)
channels1 <- channels0[!channels0$upnode %in% dropNodes, ]
channels <- channels1[!channels1$downnode %in% dropNodes, ]

total_channel_length <- sum(channels$length_feet)

# Join channel lengths with zoi data
#zoi_channel <- left_join(zoi_data, nodes)
zoi_channel <- left_join(zoi_data, channels)
zoi_channel <- merge(zoi_data, channels, by = "channel_number")

# Change all to 4326 (WGS)
delta_4326 <- st_transform(delta, crs = 4326) %>%
  mutate(line = "analysis boundary")
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY")
plot(WW_Delta_crop)

zoi_channel_long <- zoi_channel %>%
  pivot_longer(cols = c(A:E), names_to = "group", values_to = "DSM2")


# Functions ------------------------------------

# Create data frame for each month and flow level
create_df <- function(groupname, flow) {
  group_data <- zoi_channel_long %>%
    dplyr::select(OMR_Flow, node, channel_number, length_feet, upnode, downnode, group, DSM2)
  group_node <- inner_join(nodes_4326, group_data) %>%
    mutate(DSM2 = ifelse(DSM2<0, NA, DSM2)) %>%
    na.omit()# may be some missing

  df_filtered <- group_node %>% filter(OMR_Flow == flow,
                                       group == groupname)
  df_filt_sp <- as(df_filtered, "Spatial")

  # return(df_filtered)
  return(df_filt_sp)
}

# create interpolations
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
  group.idw <- gstat::idw(DSM2 ~ 1, df, newdata=grd, idp=2.0)

  # Convert to raster object then clip to Texas
  raster       <- raster(group.idw)
  raster_mask     <- mask(raster, mask)

  return(raster_mask)

}

# Map making function
# @group = group, lower case (feb, mar, apr, may)
# @clevel = contour level (0.75, 0.25)
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



#
# # Filter to correct similarity level (e.g. 0.7)
# f_filter_similarity_high <- function(df) {
#
#
#   filtered_month <- df %>%
#     filter(DSM2 < 0.25)
#
#   return(filtered_month)
# }
#
# f_filter_similarity_med <- function(df) {
#
#
#   filtered_month <- df %>%
#     filter(DSM2 >= 0.25 & DSM2<=0.75)
#
#   return(filtered_month)
# }

# Analysis ------------------------------

## Create data frames --------------------

### A --------------
A_2000_sp <- create_df(groupname = "A", flow = -2000)
A_3500_sp <- create_df(groupname = "A", flow = -3500)
A_5000_sp <- create_df(groupname = "A", flow = -5000)

### B --------------
B_2000_sp <- create_df(groupname = "B", flow = -2000)
B_3500_sp <- create_df(groupname = "B", flow = -3500)
B_5000_sp <- create_df(groupname = "B", flow = -5000)

### C --------------
C_2000_sp <- create_df(groupname = "C", flow = -2000)
C_3500_sp <- create_df(groupname = "C", flow = -3500)
C_5000_sp <- create_df(groupname = "C", flow = -5000)

### D --------------
D_2000_sp <- create_df(groupname = "D", flow = -2000)
D_3500_sp <- create_df(groupname = "D", flow = -3500)
D_5000_sp <- create_df(groupname = "D", flow = -5000)

### E --------------
E_2000_sp <- create_df(groupname = "E", flow = -2000)
E_3500_sp <- create_df(groupname = "E", flow = -3500)
E_5000_sp <- create_df(groupname = "E", flow = -5000)

# Check spatial distribution-------------------------------------------------

# Create interpolations

delta_sp <- as(delta_4326, "Spatial")

### A --------------
r.A1 <- interp_nodes(A_2000_sp)
r.A2 <- interp_nodes(A_3500_sp)
r.A3 <- interp_nodes(A_5000_sp)

### B ------------
r.B1 <- interp_nodes(B_2000_sp)
r.B2 <- interp_nodes(B_3500_sp)
r.B3 <- interp_nodes(B_5000_sp)

### C --------------
r.C1 <- interp_nodes(C_2000_sp)
r.C2 <- interp_nodes(C_3500_sp)
r.C3 <- interp_nodes(C_5000_sp)

### D --------------
r.D1 <- interp_nodes(D_2000_sp)
r.D2 <- interp_nodes(D_3500_sp)
r.D3 <- interp_nodes(D_5000_sp)

### E --------------
r.E1 <- interp_nodes(E_2000_sp)
r.E2 <- interp_nodes(E_3500_sp)
r.E3 <- interp_nodes(E_5000_sp)

## Create contours --------------------------

### A ----------------------
c.A1_25 <- rasterToContour(r.A1, levels = 0.25)
plot(c.A1_25)
c.A1_75 <- rasterToContour(r.A1, levels = 0.75)

c.A2_25 <- rasterToContour(r.A2, levels = 0.25)
c.A2_75 <- rasterToContour(r.A2, levels = 0.75)

c.A3_25 <- rasterToContour(r.A3, levels = 0.25)
c.A3_75 <- rasterToContour(r.A3, levels = 0.75)

contours_m <- c(c.A1_75, c.A1_25, c.A2_75, c.A2_25,
                c.A3_75, c.A3_25)

contours_A <- lapply(contours_m, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(group2 = "A") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -2000,
                          id %in% c(3,4) ~ -3500,
                          id %in% c(5,6) ~ -5000),
         contour = case_when(id %in% c(1,3, 5) ~ 0.75,
                             id %in% c(2,4,6) ~ 0.25))

### B ----------------------
c.B1_25 <- rasterToContour(r.B1, levels = 0.25)
c.B1_75 <- rasterToContour(r.B1, levels = 0.75)

c.B2_25 <- rasterToContour(r.B2, levels = 0.25)
c.B2_75 <- rasterToContour(r.B2, levels = 0.75)

c.B3_25 <- rasterToContour(r.B3, levels = 0.25)
c.B3_75 <- rasterToContour(r.B3, levels = 0.75)

contours_m <- c(c.B1_75, c.B1_25, c.B2_75, c.B2_25,
                c.B3_75, c.B3_25)

contours_B <- lapply(contours_m, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(group2 = "B") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -2000,
                          id %in% c(3,4) ~ -3500,
                          id %in% c(5,6) ~ -5000),
         contour = case_when(id %in% c(1,3, 5) ~ 0.75,
                             id %in% c(2,4,6) ~ 0.25))
### C ----------------------
c.C1_25 <- rasterToContour(r.C1, levels = 0.25)
c.C1_75 <- rasterToContour(r.C1, levels = 0.75)

c.C2_25 <- rasterToContour(r.C2, levels = 0.25)
c.C2_75 <- rasterToContour(r.C2, levels = 0.75)

c.C3_25 <- rasterToContour(r.C3, levels = 0.25)
c.C3_75 <- rasterToContour(r.C3, levels = 0.75)

contours_m <- c(c.C1_75, c.C1_25, c.C2_75, c.C2_25,
                c.C3_75, c.C3_25)

contours_C <- lapply(contours_m, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(group2 = "C") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -2000,
                          id %in% c(3,4) ~ -3500,
                          id %in% c(5,6) ~ -5000),
         contour = case_when(id %in% c(1,3, 5) ~ 0.75,
                             id %in% c(2,4,6) ~ 0.25))

### D ----------------------
c.D1_25 <- rasterToContour(r.D1, levels = 0.25)
c.D1_75 <- rasterToContour(r.D1, levels = 0.75)

c.D2_25 <- rasterToContour(r.D2, levels = 0.25)
c.D2_75 <- rasterToContour(r.D2, levels = 0.75)

c.D3_25 <- rasterToContour(r.D3, levels = 0.25)
c.D3_75 <- rasterToContour(r.D3, levels = 0.75)

contours_m <- c(c.D1_75, c.D1_25, c.D2_75, c.D2_25,
                c.D3_75, c.D3_25)

contours_D <- lapply(contours_m, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(group2 = "D") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -2000,
                          id %in% c(3,4) ~ -3500,
                          id %in% c(5,6) ~ -5000),
         contour = case_when(id %in% c(1,3, 5) ~ 0.75,
                             id %in% c(2,4,6) ~ 0.25))


### E ----------------------
c.E1_25 <- rasterToContour(r.E1, levels = 0.25)
c.E1_75 <- rasterToContour(r.E1, levels = 0.75)

c.E2_25 <- rasterToContour(r.E2, levels = 0.25)
c.E2_75 <- rasterToContour(r.E2, levels = 0.75)

c.E3_25 <- rasterToContour(r.E3, levels = 0.25)
c.E3_75 <- rasterToContour(r.E3, levels = 0.75)

contours_m <- c(c.E1_75, c.E1_25, c.E2_75, c.E2_25,
                c.E3_75, c.E3_25)

contours_E <- lapply(contours_m, fortify) %>%
  bind_rows(.id = "id") %>%
  mutate(group2 = "E") %>%
  mutate(flow = case_when(id %in% c(1,2) ~ -2000,
                          id %in% c(3,4) ~ -3500,
                          id %in% c(5,6) ~ -5000),
         contour = case_when(id %in% c(1,3,5) ~ 0.75,
                             id %in% c(2,4,6) ~ 0.25))

### Combine all
# switching factor levels will allow color palette to work right
contours_all <- rbind(contours_A, contours_B, contours_C, contours_D, contours_E) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000")))

# Make map -----------------------

# https://stackoverflow.com/questions/34153462/plot-spatiallinesdataframe-with-ggplot2

# Define color palette
cpal <- RColorBrewer::brewer.pal(6, "YlOrBr")[2:6]
library(randomcoloR)
n <- 15
palette <- distinctColorPalette(n)

# Run make_map function to make maps (see documentation above)
(map_A_75 <- make_map(grp = "A", clevel = 0.75))
(map_B_75 <- make_map(grp = "B", clevel = 0.75))
(map_C_75 <- make_map(grp = "C", clevel = 0.75))
(map_D_75 <- make_map(grp = "D", clevel = 0.75))
(map_E_75 <- make_map(grp = "E", clevel = 0.75))

(map_A_25 <- make_map(grp = "A", clevel =  0.25))
(map_B_25 <- make_map(grp = "B", clevel =  0.25))
(map_C_25 <- make_map(grp = "C", clevel =  0.25))
(map_D_25 <- make_map(grp = "D", clevel =  0.25))
(map_E_25 <- make_map(grp = "E", clevel =  0.25))


# Make map for all of them
contourGroup <- contours_all %>%
  filter(contour == 0.75) %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow)) %>%
  mutate(Inflow = case_when(group2 == "A" ~ "low SJR median SAC",
                            group2 == "B" ~ "median SJR median SAC",
                            group2 == "C" ~ "high SJR median SAC",
                            group2 == "D" ~ "median SJR low SAC",
                            group2 == "E" ~ "median SJR high SAC"))
# Map not including basemap
(map_75 <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    # geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourGroup, aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(15)) +
    labs(title = "0.75 Contours")+
    theme_classic())

# Faceted
(map_75_f <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    # geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourGroup, aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    # annotation_north_arrow(location = "tr", which_north = "true",
    #                        pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    # annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    facet_wrap(~Inflow) +
    ylim(c(37.6, 38.3)) +
    xlim(c(-122.2, -121.2)) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(15)) +
    labs(title = "0.75 Contours")+
    theme_classic() +
    theme(axis.text = element_blank()))



contourGroup2 <- contours_all %>%
  filter(contour == 0.25) %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow))%>%
  mutate(Inflow = case_when(group2 == "A" ~ "low SJR median SAC",
                            group2 == "B" ~ "median SJR median SAC",
                            group2 == "C" ~ "high SJR median SAC",
                            group2 == "D" ~ "median SJR low SAC",
                            group2 == "E" ~ "median SJR high SAC"))
# Map not including basemap
(map_25 <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    # geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourGroup2, aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(15)) +
    labs(title = "0.25 Contours")+
    theme_classic())


(map_25_f <- ggplot() +
    # geom_sf(data = delta_4326, fill = NA, inherit.aes = FALSE, linetype = "dashed") +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    # geom_sf(data = nodes_4326, size = 0.4, color = "gray30", inherit.aes = FALSE) +
    geom_path(data = contourGroup2, aes(x = long, y = lat, group  = grouper, color= label, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    # annotation_north_arrow(location = "tr", which_north = "true",
    #                        pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    # annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    facet_wrap(~Inflow) +
    ylim(c(37.7, 38.1)) +
    xlim(c(-121.8, -121.2)) +
    scale_color_manual("OMR Flow (cfs)", values = viridis::viridis(15)) +
    labs(title = "0.25 Contours")+
    theme_classic()+
    theme(axis.text = element_blank()))


# Export Maps ---------------------

map_75
ggsave("figures/Contours_75_allgroups.png", width = 7, height = 6, device = 'png', dpi = 300)

map_75_f
ggsave("figures/Contours_75_allgroups_facet.png", width = 9, height = 6, device = 'png', dpi = 300)

map_25
ggsave("figures/Contours_25_allgroups.png", width = 7, height = 6, device = 'png', dpi = 300)

map_25_f
ggsave("figures/Contours_25_allgroups_facet.png", width = 9, height = 6, device = 'png', dpi = 300)













# Filter to proportion of similarity ----------------------------------

## List all the files --------------------------
df <- list(A_2000_sp, A_3500_sp, A_5000_sp,
           B_2000_sp, B_3500_sp, B_5000_sp,
           C_2000_sp, C_3500_sp, C_5000_sp,
           D_2000_sp, D_3500_sp, D_5000_sp,
           E_2000_sp, E_3500_sp, E_5000_sp)

## Loop it ---------------------------------------

### This one should be filtering correctly
filtered_high <- lapply(df, f_filter_similarity_high) %>%
  bind_rows()

# Calculate total length -------------------------
filtered_high2 <- filtered_high %>%
  group_by(group, OMR_Flow) %>%
  summarize(sumLength = sum(length_feet))%>%
  ungroup() %>%
  mutate(Group = factor(group, levels = c("A", "B", "C", "D", "E")))













# Visualize -------------------------------------

## Check map (needs work) -----------------------------------

map_omr_vals <- ggplot() +
  geom_sf(data = delta_4326, aes(linetype = line), fill = NA, inherit.aes = FALSE)+
  geom_sf(data = filtered2, aes(color = Flow, shape = Flow), alpha = 0.3, inherit.aes = FALSE) +
  facet_wrap(~Month) +
  labs(title = "Nodes with <0.7 similarity under different OMR Levels", color = "OMR",
       shape = "OMR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

png(filename = here::here("figures", "plot_map_omr_vals.png"),
    width = 6, height = 6, units = "in",
    pointsize = 12, family = "sans", res = 300)
map_omr_vals
dev.off()

## Visualize differences -------------------
barplot_omr <- ggplot(filtered2) +
  geom_col(aes(Month, sumLength, fill = Flow), position = "dodge2")  +
  labs(y = "Sum Channel Length") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_bw()

# png(filename = here::here("figures", "plot_barplot_omr_channellength.png"),
#     width = 6, height = 6, units = "in",
#     pointsize = 12, family = "sans", res = 300)
# barplot_omr
# dev.off()


### Look at February
feb <- filtered_try2%>%
  filter(Month == "Feb")

feb_wide <- feb %>%
  select(-upnode, -channel_number) %>%
  pivot_wider( values_from= c("length_feet","DSM2"), names_from = "Flow")

feb235 <- feb_2000_sp %>%
  st_drop_geometry() %>%
  select(-points, -channel_number, -Flow) %>%
  rename(DSM2_2000 =DSM2) %>%
  full_join(feb_3500_sp %>% st_drop_geometry(), by = c("upnode", "downnode", "Month", "node")) %>%
  select(-points, -channel_number, -Flow) %>%
  rename(DSM2_3500 = DSM2) %>%
  full_join(feb_5000_sp %>% st_drop_geometry(), by = c("upnode", "downnode", "Month", "node")) %>%
  st_drop_geometry() %>%
  rename(DSM2_5000 = DSM2) %>%
  select(-points, -channel_number, -Flow) %>%
  mutate(diff = ifelse(DSM2_3500 <0.7 & DSM2_5000 >=0.7, "diff", "0")) %>%
  #mutate(diff = ifelse(DSM2_3500 < DSM2_5000, "diff", "0")) %>%
  mutate(DSM2_3500 = round(DSM2_3500, 3),
         DSM2_5000 = round(DSM2_5000, 3)) %>%
  select(node, upnode, downnode, length_2000 = length_feet.y, length_3500 = length_feet.x,
         length_5000 = length_feet, DSM2_2000, DSM2_3500, DSM2_5000, diff)

