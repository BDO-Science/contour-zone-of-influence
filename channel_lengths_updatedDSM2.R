# make_contours.R ######
#### Catarina Pien and Lisa Elliott

# This code uses zone of influence modeling results (DSM2) to create barplots showing
#  how channel length associated with the zone of influence changes from operational facilities based on pumping
#to different OMR levels, at different SJR and SR flow levels

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


# Read/Join data ---------------------------------------------------------
zoi_file_NAA = list.files("data_inflow", pattern = "NAA_.*csv$", full.names = TRUE)
#zoi_file_D1641
zoi_data <- lapply(zoi_file_NAA, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_NAA[as.numeric(id)], 36, 39))) %>%
  rename(OMR_Flow = id)

channels <- read_csv("data_updatedDSM2/Reclamation_2021LTO_DSM2_Version806_ChannelLengths.csv") %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no)



# Drop nodes that are causing issues
dropNodes <- c(146, 147, 148, 206, 242, 246, 432, 433, 434)
nodes <- channels[!channels$upnode %in% dropNodes, ]
nodes <- nodes[!nodes$downnode %in% dropNodes, ]
channels <- nodes

total_channel_length <- sum(channels$length_feet)

# Join channel lengths with zoi data
#zoi_channel <- left_join(zoi_data, nodes)
zoi_channel <- left_join(zoi_data, channels)
zoi_channel <- merge(zoi_data, channels, by = "channel_number")


zoi_channel_group <- zoi_channel %>%
  pivot_longer(cols = c(A, B, C, D, E),
              names_to = "Group",
               values_to = "p_overlap") %>%
  filter(p_overlap>=0)

filtered_dat_high <- zoi_channel_group %>%
  filter(p_overlap < 0.25)

filtered_dat_med <- zoi_channel_group %>%
  filter(p_overlap > 0.25 & p_overlap < 0.75)


# Calculate total length -------------------------
filtered2_high <- filtered_dat_high %>%
  group_by(Group, OMR_Flow) %>%
  summarize(sumLength = sum(length_feet))%>%
  ungroup() %>%
  mutate(Group = factor(Group, levels = c("A", "B", "C", "D", "E")))%>%
  mutate(pLength = sumLength/total_channel_length) %>%
  mutate(h_influence = "High hydrologic influence")

filtered2_med <- filtered_dat_med %>%
  group_by(Group, OMR_Flow) %>%
  summarize(sumLength = sum(length_feet))%>%
  ungroup() %>%
  mutate(Group = factor(Group, levels = c("A", "B", "C", "D", "E"))) %>%
  mutate(pLength = sumLength/total_channel_length) %>%
  mutate(h_influence = "Medium hydrologic influence")

filtered_dat <- rbind(filtered2_high, filtered2_med)


## Visualize differences -------------------
barplot_omr <- ggplot(filtered_dat) +
  geom_col(aes(Group, pLength, fill = OMR_Flow), position = "dodge2")  +
  labs(y = "Proportional Channel Length") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~h_influence, nrow = 2) +
  theme_bw()
barplot_omr

png("figures/proportional_channel_length_dropnodes.png", width = 7, height = 9, units = "in", res = 300, pointsize = 9)
barplot_omr
dev.off()

## Look at map --------------------------
# Visualization
library(ggmap)
library(ggspatial)
library(deltamapr)
library(viridis)
library(sf)
library(purrr)

# Change all to 4326 (WGS)
delta <- st_read("shapefiles/Bay_Delta_Poly_New.shp")
delta_4326 <- st_transform(delta, crs = 4326) %>%
  mutate(line = "analysis boundary")
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY")
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
nodes_4326 <- st_transform(nodes, crs = st_crs(delta_4326)) %>%
  mutate(points = "DSM2 nodes")

dataA <- inner_join(nodes_4326, filtered_dat_high)
data <- dataA %>%
  mutate(long = unlist(map(dataA$geometry,1)),
         lat = unlist(map(dataA$geometry,2)))

weird <- filter(data, long < -121.8) #nodes 432, 433, 434

data2 <- inner_join(nodes_4326, filtered_dat_med)%>%
  mutate(long = unlist(map(data2$geometry,1)),
         lat = unlist(map(data2$geometry,2)))


(map_omr_high <- ggplot() +
     geom_sf(data = WW_Delta_crop, color = "gray94") +
    geom_sf(data = delta_4326, aes(linetype = line), fill = NA, inherit.aes = FALSE)+
  geom_sf(data = data, aes(color = OMR_Flow, shape = OMR_Flow), alpha = 0.3, size = 3,inherit.aes = FALSE) +
    facet_wrap(~Group) +
  labs(title = "Nodes with <0.25 similarity under different OMR Levels", color = "OMR",
       shape = "OMR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)))

png("figures/high_influence_map.png", width = 8, height = 6, units = "in", res = 300)
map_omr_high
dev.off()



(map_omr_med <- ggplot() +
    geom_sf(data = WW_Delta_crop, color = "gray94") +
    geom_sf(data = delta_4326, aes(linetype = line), fill = NA, inherit.aes = FALSE)+
    geom_sf(data = data2, aes(color = OMR_Flow, shape = OMR_Flow), alpha = 0.3, size = 3,inherit.aes = FALSE) +
    facet_wrap(~Group) +

    labs(title = "Nodes with 0.25-0.75 similarity under different OMR Levels", color = "OMR",
         shape = "OMR") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)))
png("figures/med_influence_map.png", width = 8, height = 6, units = "in", res = 300)
map_omr_med
dev.off()
