library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(knitr)
library(tidyr)
library(sf)
library(readr)
library(mapview)
library(deltamapr)
library(ggspatial)


nodes <- st_read("shapefiles/nodes.shp")
nodes <- st_transform(nodes, crs = st_crs(WW_Delta)) %>%
  dplyr::select(nodenum, node, channel_nu, channel_id) %>%
  rename(channel.no = channel_nu)

# Turner Cut, SJR at Jersey Point, Old R at Middle River
nodesofinterest <- c(26, 469, 52)
nodenames <- read.csv("data_raw/inputFile_withnode.csv")%>%
  filter(node.no %in% nodesofinterest)
nodes2 <- left_join(nodes, nodenames) %>%
  mutate(analysis = if_else(channel.no%in%c(172,59,83), "Yes", "No"))
nodes_label <- nodes2 %>%
  filter(channel.no%in%c(172,59,83))
nodes_map <- nodes2%>%
  filter(channel.no%in%c(172,59,83))

# Make map
(map_nodes_attachment <- ggplot() +
  geom_sf(data = WW_Delta, fill = "gray90", color = "gray70", alpha = 0.7, inherit.aes = FALSE) +
  geom_sf(data = nodes_map, color = "gray50", size = 1 ) +
  geom_sf(data = nodes_label, color = "navy", size = 3) +
  geom_sf_label(data = nodes_label, aes(label = description),
                nudge_x = 0.05, nudge_y = 0.05, size = 3.5) +
  ylim(c(37.6, 38.6)) +
  xlim(c(-122.2, -121)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
    # scale_color_manual(values = c("gray90", "navy")) +
    # scale_size_manual(values = c(1, 3)) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()))

png("figures/attachment_plots/map_nodes_select_contourplots.png", units = "in", width = 6, height = 6, res = 300)
map_nodes_attachment
dev.off()




