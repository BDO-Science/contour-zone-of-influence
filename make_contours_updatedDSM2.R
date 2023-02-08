# make_contours.R ######
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
zoi_file_NAA = list.files("data_updatedDSM2", pattern = "NAA_.*csv$", full.names = TRUE)
#zoi_file_D1641
zoi_data <- lapply(zoi_file, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = substr(zoi_file[as.numeric(id)], 41, 44)) %>%
  rename(Flow = id)
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
channels <- read_csv("data_updatedDSM2/Reclamation_2021LTO_DSM2_Version806_ChannelLengths.csv") %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no)

# Drop nodes that are causing issues
dropNodes <- c(146, 147, 148, 206, 242, 246)
nodes <- nodes[!nodes$node %in% dropNodes, ]

# Join channel lengths with zoi data
zoi_channel <- left_join(zoi_data, channels)

# Change all to 4326 (WGS)
delta_4326 <- st_transform(delta, crs = 4326) %>%
  mutate(line = "analysis boundary")
nodes_4326 <- st_transform(nodes, crs = st_crs(delta_4326)) %>%
  mutate(points = "DSM2 nodes")
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY")
plot(WW_Delta_crop)

# Look at availability of data



# Functions ------------------------------------

# Create data frame for each month and flow level
create_df <- function(month, flow) {
  month_data <- zoi_channel %>%
    dplyr::select(Flow, node, channel_number, length_feet, upnode, downnode, month) %>%
    rename(DSM2 = month)
  month_node <- inner_join(nodes_4326, month_data) %>%
    mutate(DSM2 = ifelse(DSM2<0, NA, DSM2),
           Month = month) %>%
    na.omit()# may be some missing

  df_filtered <- month_node %>% filter(Flow == flow)
  df_filt_sp <- as(df_filtered, "Spatial")

  return(df_filtered)
  #return(df_filt_sp)
}

# Filter to correct similarity level (e.g. 0.7)
f_filter_similarity <- function(df) {
  # figure out the downstream nodes that correspond to upstream %sim of 0.7 or greater
  tokeep <- df %>%
  mutate(include1 = ifelse(DSM2 < 0.7, "Y", "N")) %>%
  filter(include1 == "Y") %>%
  pivot_longer(cols = c(upnode, downnode), values_to = "nodestokeep") %>%
  st_drop_geometry()
  nodestokeep <- unique(tokeep$nodestokeep)

  # do the actual filtering of nodes
  filtered_month <- df %>%
    filter(node %in% nodestokeep)

  # filtered_month <- df %>%
  #   filter(DSM2 < 0.7)

  return(filtered_month)
}


# Filter to correct similarity level (e.g. 0.7)
f_filter_similarity2 <- function(df) {


  filtered_month <- df %>%
  filter(DSM2 < 0.7)

  return(filtered_month)
}

# Analysis ------------------------------

## Create data frames --------------------

### June --------------
jun_2000_sp <- create_df(month = "Jun", flow = 2000)
jun_3500_sp <- create_df(month = "Jun", flow = 3500)
jun_5000_sp <- create_df(month = "Jun", flow = 5000)
#jun_6500_sp <- create_df(month = "Jun", flow = 6500)

### May --------------
may_2000_sp <- create_df(month = "May", flow = 2000)
may_3500_sp <- create_df(month = "May", flow = 3500)
#may_5000_sp <- create_df(month = "May", flow = 5000)
#may_6500_sp <- create_df(month = "May", flow = 6500)

### April --------------
apr_2000_sp <- create_df(month = "Apr", flow = 2000)
apr_3500_sp <- create_df(month = "Apr", flow = 3500)
#apr_5000_sp <- create_df(month = "Apr", flow = 5000)
#apr_6500_sp <- create_df(month = "Apr", flow = 6500)

### March --------------
mar_2000_sp <- create_df(month = "Mar", flow = 2000)
mar_3500_sp <- create_df(month = "Mar", flow = 3500)
mar_5000_sp <- create_df(month = "Mar", flow = 5000)
#mar_6500_sp <- create_df(month = "Mar", flow = 6500)

### February --------------
feb_2000_sp <- create_df(month = "Feb", flow = 2000)
feb_3500_sp <- create_df(month = "Feb", flow = 3500)
feb_5000_sp <- create_df(month = "Feb", flow = 5000)
#feb_6500_sp <- create_df(month = "Feb", flow = 6500)

### January --------------
jan_2000_sp <- create_df(month = "Jan", flow = 2000)
jan_3500_sp <- create_df(month = "Jan", flow = 3500)
jan_5000_sp <- create_df(month = "Jan", flow = 5000)
#jan_6500_sp <- create_df(month = "Jan", flow = 6500)

### December --------------
dec_2000_sp <- create_df(month = "Dec", flow = 2000)
dec_3500_sp <- create_df(month = "Dec", flow = 3500)
dec_5000_sp <- create_df(month = "Dec", flow = 5000)
dec_6500_sp <- create_df(month = "Dec", flow = 6500)

# Check spatial distribution-------------------------------------------------

## Looks like we are already inside boundary
ggplot() +
  geom_sf(data = delta_4326, aes(linetype = line), fill = NA, inherit.aes = FALSE)+
  geom_sf(data = dec_2000_sp, aes(color = Flow), inherit.aes = FALSE)

# Filter to proportion of similarity ----------------------------------

## List all the files --------------------------
df <- list(dec_2000_sp, dec_3500_sp, dec_5000_sp, dec_6500_sp,
           jan_2000_sp, jan_3500_sp, jan_5000_sp,
           feb_2000_sp, feb_3500_sp, feb_5000_sp,
           mar_2000_sp, mar_3500_sp, mar_5000_sp,
           apr_2000_sp, apr_3500_sp,
           may_2000_sp, may_3500_sp,
           jun_2000_sp, jun_3500_sp, jun_5000_sp)

## Loop it ---------------------------------------
# filtered <- lapply(df, f_filter_similarity) %>%
#   bind_rows() %>%
#   mutate(Flow = as.factor(as.numeric(Flow) * -1))

### This one should be filtering correctly
filtered_try2 <- lapply(df, f_filter_similarity2) %>%
  bind_rows() %>%
  mutate(Flow = as.factor(as.numeric(Flow) * -1))

# filtered_try2 <- list()
# for(r in 1:nrow(df)){
#   filtered_try2[r] <- filter(as.data.frame(df[r]), DSM2 < 0.7)
# }

# Calculate total length -------------------------
filtered2 <- filtered_try2 %>%
  group_by(Month, Flow) %>%
  summarize(sumLength = sum(length_feet))%>%
  ungroup() %>%
  mutate(Month = factor(Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")))

# Visualize -------------------------------------

## Check map (needs work) -----------------------------------

ggplot() +
  geom_sf(data = delta_4326, aes(linetype = line), fill = NA, inherit.aes = FALSE)+
  geom_sf(data = filtered2, aes(color = Flow, shape = Flow), alpha = 0.3, inherit.aes = FALSE) +
  facet_wrap(~Month) +
  labs(title = "Nodes with <0.7 similarity under different OMR Levels", color = "OMR",
       shape = "OMR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


## Visualize differences -------------------
ggplot(filtered2) +
  geom_col(aes(Month, sumLength, fill = Flow), position = "dodge2")  +
  labs(y = "Sum Channel Length") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_bw()

######### Cat ended here ##############################

# TO DO

# To check we are including the right nodes,
  # map which ones are truly <0.7 and which ones are not and
  # see they are next to each other.
# Think about why 3500 seems to be greater than 5000.
# Do we need to draw lines at all?
# Export table
