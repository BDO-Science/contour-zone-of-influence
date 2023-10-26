# Functions ------------------------------------

# Create data frame for each month and flow level
# @group = inflow group
# @flow = OMR flow group
# produces an "sp" object
create_df <- function(groupname, flow, alt) {
  group_data <- zoi_channel_long %>%
    dplyr::select(OMR_Flow, Alt, node, channel_number, length_feet, upnode, downnode, group, overlap)
  group_node <- inner_join(nodes_4326, group_data) %>%
    mutate(overlap = ifelse(overlap<0, NA, overlap)) %>%
    na.omit()# may be some missing

  df_filtered <- group_node %>% filter(Alt == alt,
                                       OMR_Flow == flow,
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
  c2 <- mapply(rasterToContour, inflow_list, levels = 0.25)
  cont <- rbind(c1, c2)
  contours_lolo <- lapply(cont, fortify) %>%
    bind_rows(.id = "id") %>%
    mutate(group2 = inflow_group) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-2000",
                            id %in% c(3,4) ~ "-3500",
                            id %in% c(5,6) ~ "-5000",
                            id %in% c(7,8) ~ "<-5500"),
           contour = case_when(id %in% c(1,3, 5, 7) ~ 0.75,
                               id %in% c(2,4,6, 8) ~ 0.25))
}

# This function combines the functions of creating dataframe, interpreting nodes, and creating contour into one.
# See above functions create_df, interp_nodes, create_contour for details.
# @gpname = inflow group
# @altname = alternative name
f_data_interp_contour <- function(gpname, altname) {
  sp_2000 <- create_df(groupname = gpname, flow = "-2000", alt = altname)
  sp_3500 <- create_df(groupname = gpname, flow = "-3500", alt = altname)
  sp_5000 <- create_df(groupname = gpname, flow = "-5000", alt = altname)
  sp_5500 <- create_df(groupname = gpname, flow = "<-5500", alt = altname)
  r.2000 <- interp_nodes(sp_2000)
  r.3500 <- interp_nodes(sp_3500)
  r.5000 <- interp_nodes(sp_5000)
  r.5500 <- interp_nodes(sp_5500)
  group_list <- c(r.2000, r.3500, r.5000, r.5500)
  contours <- create_contour(group_list, inflow_group = gpname) %>%
    mutate(Alt = altname)
  return(contours)
}

# Subsequent functions do the same, but for different combinations of OMR
f_data_interp_contour_no2000 <- function(gpname, altname) {
  sp_3500 <- create_df(groupname = gpname, flow = "-3500", alt = altname)
  sp_5000 <- create_df(groupname = gpname, flow = "-5000", alt = altname)
  sp_5500 <- create_df(groupname = gpname, flow = "<-5500", alt = altname)
  r.3500 <- interp_nodes(sp_3500)
  r.5000 <- interp_nodes(sp_5000)
  r.5500 <- interp_nodes(sp_5500)
  group_list <- c(r.3500, r.5000, r.5500)
  contours <- create_contour(group_list, inflow_group = "hilo") %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-3500",
                            id %in% c(3,4) ~ "-5000",
                            id %in% c(5,6) ~ "<-5500"))%>%
    mutate(Alt = altname)
  return(contours)
}

f_data_interp_contour_no3500 <- function(gpname, altname) {
  sp_2000 <- create_df(groupname = gpname, flow = "-2000", alt = altname)
  sp_5000 <- create_df(groupname = gpname, flow = "-5000", alt = altname)
  sp_5500 <- create_df(groupname = gpname, flow = "<-5500", alt = altname)
  r.2000 <- interp_nodes(sp_2000)
  r.5000 <- interp_nodes(sp_5000)
  r.5500 <- interp_nodes(sp_5500)
  group_list <- c(r.2000, r.5000, r.5500)
  contours <- create_contour(group_list, inflow_group = gpname) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-2000",
                            id %in% c(3,4) ~ "-5000",
                            id %in% c(5,6) ~ "<-5500"))%>%
    mutate(Alt = altname)
  return(contours)
}

f_data_interp_contour_no5500 <- function(gpname, altname) {
  sp_2000 <- create_df(groupname = gpname, flow = "-2000", alt = altname)
  sp_3500 <- create_df(groupname = gpname, flow = "-3500", alt = altname)
  sp_5000 <- create_df(groupname = gpname, flow = "-5000", alt = altname)
  r.2000 <- interp_nodes(sp_2000)
  r.3500 <- interp_nodes(sp_3500)
  r.5000 <- interp_nodes(sp_5000)
  group_list <- c(r.2000, r.3500, r.5000)
  contours <- create_contour(group_list, inflow_group = gpname)%>%
    mutate(Alt = altname)
  return(contours)
}

f_data_interp_contour_no20005000 <- function(gpname, altname) {
  sp_3500 <- create_df(groupname = gpname, flow = "-3500", alt = altname)
  sp_5500 <- create_df(groupname = gpname, flow = "<-5500", alt = altname)
  r.3500 <- interp_nodes(sp_3500)
  r.5500 <- interp_nodes(sp_5500)
  group_list <- c(r.3500, r.5500)
  contours <- create_contour(group_list, inflow_group = gpname) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-3500",
                            id %in% c(3,4) ~ "<-5500"))%>%
    mutate(Alt = altname)
  return(contours)
}

f_data_interp_contour_no20005500 <- function(gpname, altname) {
  sp_3500 <- create_df(groupname = gpname, flow = "-3500", alt = altname)
  sp_5000 <- create_df(groupname = gpname, flow = "-5000", alt = altname)
  r.3500 <- interp_nodes(sp_3500)
  r.5000 <- interp_nodes(sp_5000)
  group_list <- c(r.3500, r.5000)
  contours <- create_contour(group_list, inflow_group = gpname) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-3500",
                            id %in% c(3,4) ~ "-5000"))%>%
    mutate(Alt = altname)
  return(contours)
}


plot_contours <- function(alt, cont) {

  (map_75_f <- ggplot() +
     geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
     geom_path(data = contourGroup %>% filter(Alt == alt) %>% filter(contour =- cont), aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
     # annotation_north_arrow(location = "tr", which_north = "true",
     #                        pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
     #                        style = north_arrow_fancy_orienteering) +
     # annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
     facet_wrap(~Inflow) +
     ylim(c(37.7, 38.1)) +
     xlim(c(-121.8, -121.2)) +
     scale_color_viridis_d(option = "turbo") +
     labs(title = paste(alt, cont, "contour")) +
     theme_classic() +
     theme(axis.text = element_blank()))


  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/attachment_plots/contour_plot_", alt, "_", cont, ".png"), plot=map_75_f, height = 6, width = 7, units = "in")
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
