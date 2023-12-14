# Functions ------------------------------------

# Create data frame for each month and flow level
# @group = inflow group
# @flow = OMR flow group
# produces an "sp" object
create_df <- function(groupname, flow, alt) {
  group_data <- zoi_channel_long %>%
    dplyr::select(OMR_Flow, Alt, node, channel_number, length, upnode, downnode, group, overlap)
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

# Subsequent functions do the same, but for different combinations of OMR.
# Not all of the combinations have all of the OMR bins represented.
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

f_data_interp_contour_no50005500 <- function(gpname, altname) {
  sp_2000 <- create_df(groupname = gpname, flow = "-2000", alt = altname)
  sp_3500 <- create_df(groupname = gpname, flow = "-3500", alt = altname)
  r.2000 <- interp_nodes(sp_2000)
  r.3500 <- interp_nodes(sp_3500)
  group_list <- c(r.2000, r.3500)
  contours <- create_contour(group_list, inflow_group = gpname) %>%
    mutate(flow = case_when(id %in% c(1,2) ~ "-2000",
                            id %in% c(3,4) ~ "-3500"))%>%
    mutate(Alt = altname)
  return(contours)
}

# This function plots a specific contour and then exports it out
# @alt = alternative name
# @cont = contour of interest, should be in a proportion (e.g. 0.75)
plot_contours <- function(alt, cont) {

  map_75_f <- ggplot() +
     geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
     geom_path(data = contourGroup %>% filter(Alt == alt) %>% filter(contour == cont), aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
     facet_wrap(~Inflow) +
     ylim(c(37.7, 38.1)) +
     xlim(c(-121.8, -121.2)) +
     scale_color_viridis_d(option = "turbo") +
     labs(title = paste(alt, cont, "contour"), color = "OMR Bin", linetype = "OMR Bin") +
     theme_classic() +
     theme(axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank())


  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/attachment_plots/contour_plot_", alt, "_", cont, ".png"), plot=map_75_f, height = 6, width = 7, units = "in")
}


# This function plots a specific contour and then exports it out, includes all alternatives
# @grp = inflow group
# @cont = contour of interest, should be in a proportion (e.g. 0.75)

plot_contours_facetalt <- function(grp, cont) {

  map_75_f <- ggplot() +
    geom_sf(data = WW_Delta_crop, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
    geom_path(data = contourGroup %>% filter(Inflow == grp) %>% filter(contour == cont), aes(x = long, y = lat, group  = grouper, color= OMR_flow, linetype = OMR_flow), linewidth = 0.5, inherit.aes = FALSE) +
    facet_wrap(~Alt) +
    ylim(c(37.7, 38.1)) +
    xlim(c(-121.8, -121.2)) +
    scale_color_viridis_d(option = "turbo") +
    labs(title = paste(grp, cont, "contour")) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())


  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/attachment_plots/contour_plot_", grp, "_", cont, ".png"), plot=map_75_f, height = 6, width = 7, units = "in")
}


# This function makes the stacked barplots for channel length and writes them out as figures
# @grp = inflow group
plot_barplot <- function(grp) {

  pal <- c('#9a3324', "#88CCEE","#AA4499",'#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1')

  barplot <-
    filtered_dat %>%
    filter(group == grp) %>%
    ggplot(aes(x = Alt, y = pLength, fill = Alt, pattern = h_influence)) +
    geom_col_pattern(color = "black",
                    pattern_color = "black",
                    pattern_fill = "black",
                    pattern_spacing = 0.05,
                    pattern_size = 0.4,
                    alpha = 0.9)  +
                    labs(y = "Proportional Channel Length", title = grp) +
                    scale_pattern_manual(values = c("none", "circle", "stripe")) +
                    facet_wrap(~OMR_Flow) +
                    scale_fill_manual(values = pal[c(3:10)]) +
                    theme_classic() +
                    theme(legend.position = "top",
                          legend.box = "vertical",
                          axis.text.x = element_text(angle = 90),
                          axis.title.x = element_blank())

  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/attachment_plots/stacked_barplot_", grp, ".png"), plot=barplot, height = 8, width = 7, units = "in")
}


# This function makes the stacked barplots for channel length by inflow group nd writes them out as figures. Dataset specific to BA components.
# @grp = inflow group
plot_barplot_BA <- function(grp) {

  pal <- c('#9a3324', "#88CCEE","#AA4499",'#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1')

  barplot <-
    filtered_dat_BA %>%
    filter(group == grp) %>%
    ggplot(aes(x = Alt, y = pLength, fill = Alt, pattern = h_influence)) +
    geom_col_pattern(color = "black",
                     pattern_color = "black",
                     pattern_fill = "black",
                     pattern_spacing = 0.05,
                     pattern_size = 0.4,
                     alpha = 0.9)  +
    labs(y = "Proportional Channel Length", title = grp) +
    scale_pattern_manual(values = c("none", "circle", "stripe")) +
    facet_wrap(~OMR_Flow) +
    scale_fill_manual(values = pal[c(3:10)]) +
    theme_classic() +
    theme(legend.position = "top",
          legend.box = "vertical",
          axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank())

  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/attachment_plots/BA_stacked_barplot_", grp, ".png"), plot=barplot, height = 8, width = 7, units = "in")
}



# This function makes the stacked barplots for channel length by alternative and writes them out as figures. Dataset specific to BA components.
# @alt = alternative name
plot_barplot_alt <- function(alt) {

  pal <- c('#9a3324', "#88CCEE","#AA4499",'#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1')

  barplot <-
    filtered_dat %>%
    filter(Alt == alt) %>%
    ggplot(aes(x = OMR_Flow, y = pLength, fill = OMR_Flow, pattern = h_influence)) +
    geom_col_pattern(color = "black",
                     pattern_color = "black",
                     pattern_fill = "black",
                     pattern_spacing = 0.05,
                     pattern_size = 0.4,
                     alpha = 0.9)  +
    labs(y = "Proportional Channel Length", title = alt) +
    scale_pattern_manual(values = c("none", "circle", "stripe")) +
    facet_wrap(~group) +
    # scale_fill_manual(values = pal[c(3,4,5,6,7,8,10)]) +
    scale_fill_viridis_d(option = "turbo") +
    theme_classic() +
    theme(legend.position = "top",
          legend.box = "vertical",
          axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank())

  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/attachment_plots/stacked_barplot_", alt, ".png"), plot=barplot, height = 8, width = 7, units = "in")
}
