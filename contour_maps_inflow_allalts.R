# contour_maps_inflow.R ######
# Updated: 10/25/2023
# Catarina Pien and Lisa Elliott (USBR)
# cpien@usbr.gov; lelliott@usbr.gov

#  This code uses zone of influence modeling results (DSM2) to create contours showing
#  how zone of influence changes from operational facilities based on pumping
#  Contour lines of a specific level are then compared between different flow levels
#  to indicate how changing OMR will influence the zone of influence.

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
library(here)

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

# This file contains many of the functions used to create contour maps
source(here("functions_zoi.R"))

# Read/Join data ---------------------------------------------------------
delta <- st_read(here("shapefiles/Bay_Delta_Poly_New.shp"))
zoi_file_NAA = list.files("data_raw/zoi/", pattern = "NAA_.*csv$", full.names = TRUE)
zoi_file_Alt1 = list.files("data_raw/zoi/", pattern = "ALT1_.*csv$", full.names = TRUE)
zoi_file_Alt2d = list.files("data_raw/zoi/", pattern = "ALT2v1_wTUCP_.*csv$", full.names = TRUE)
zoi_file_Alt2a = list.files("data_raw/zoi/", pattern = "ALT2v1_woTUCP_.*csv$", full.names = TRUE)
zoi_file_Alt2b = list.files("data_raw/zoi/", pattern = "ALT2v2_.*csv$", full.names = TRUE)
zoi_file_Alt2c = list.files("data_raw/zoi/", pattern = "ALT2v3_.*csv$", full.names = TRUE)
zoi_file_Alt4 = list.files("data_raw/zoi/", pattern = "ALT4_.*csv$", full.names = TRUE)

zoi_data_NAA <- lapply(zoi_file_NAA, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_NAA[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow))%>%
  mutate(Alt = "NAA")
zoi_data_Alt1 <- lapply(zoi_file_Alt1, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_Alt1[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow)) %>%
  mutate(Alt = "Alt1")
zoi_data_Alt2a <- lapply(zoi_file_Alt2a, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_Alt2a[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow)) %>%
  mutate(Alt = "Alt2a")
zoi_data_Alt2b <- lapply(zoi_file_Alt2b, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_Alt2b[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow)) %>%
  mutate(Alt = "Alt2b")
zoi_data_Alt2c <- lapply(zoi_file_Alt2c, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_Alt2c[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow)) %>%
  mutate(Alt = "Alt2c")
zoi_data_Alt2d <- lapply(zoi_file_Alt2d, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_Alt2d[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow)) %>%
  mutate(Alt = "Alt2d")
zoi_data_Alt4 <- lapply(zoi_file_Alt4, read_csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = paste0("-", substr(zoi_file_Alt4[as.numeric(id)],25, 28))) %>%
  rename(OMR_Flow = id) %>%
  mutate(OMR_Flow = if_else(OMR_Flow == "-sTha", "<-5500", OMR_Flow)) %>%
  mutate(Alt = "Alt4")

# combine each individual file
zoi_data <- rbind(zoi_data_NAA, zoi_data_Alt1, zoi_data_Alt2a, zoi_data_Alt2b, zoi_data_Alt2c, zoi_data_Alt2d, zoi_data_Alt4)

# read in nodes, channels data
nodes <- st_read("shapefiles/nodes.shp") %>%
  dplyr::select(node)
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
channels0 <- read_csv("data_raw/DSM2_Version822_Grid_20231102.csv") %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no) %>%
  dplyr::select(-manning, -dispersion)

# Drop nodes that are causing issues
dropNodes <- c(146, 147, 148, 206, 242, 246, 432, 433, 434)
# Drop duplicate channels
channels1 <- channels0[!channels0$upnode %in% dropNodes, ]
channels <- channels1[!channels1$downnode %in% dropNodes, ]
total_channel_length <- sum(channels$length)

# Join channel lengths with zoi data
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

# Convert data frame to long
zoi_channel_long <- zoi_channel %>%
  pivot_longer(cols = c(lolo:hihi), names_to = "group", values_to = "overlap")

# Write data for channel length script
# write_csv(zoi_channel_long, "data_export/prop_overlap_data_long.csv")

# Look at raw data -----------------------------------
summary_vals <- zoi_channel_long %>%
  filter(overlap>=0) %>%
  group_by(group, OMR_Flow, Alt) %>%
  summarize(min = min(overlap),
            max = max(overlap),
            mean = mean(overlap)) %>%
  ungroup()

## plots ----------------------
ggplot(summary_vals) +
  geom_col(aes(x = Alt, y = mean, fill = Alt)) + facet_grid(OMR_Flow~group)+
  scale_fill_viridis_d()
ggplot(zoi_channel_long %>% filter(overlap>=0)) +
  geom_jitter(aes(x = Alt, y = overlap, color = node)) + facet_wrap(~group)
ggplot(zoi_channel_long %>% filter(overlap>=0)) +
  geom_violin(aes(x = Alt, y = overlap, fill = Alt)) + facet_grid(OMR_Flow~group) +
  scale_fill_viridis_d()

# Create data frames for each inflow-OMR group --------------------
# Look at which combinations are missing for the next exercise
png("figures/allalts_missingcombos.png", units = "in", width = 7, height = 7, res = 300)
zoi_channel_long %>% filter(overlap>=0) %>%
  group_by(Alt, group, OMR_Flow) %>%
  summarize(n = n()) %>%
  mutate(group = factor(group, levels = inflow_order)) %>%
  ggplot()  + geom_tile(aes(x = OMR_Flow, y = group, fill = n), color = "black") + facet_wrap(~Alt) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()

# filtered to medium hydrologic overlap sample sizes
png("figures/allalts_samplesizes_medhydro.png", units = "in", width = 7, height = 7, res = 300)
zoi_channel_long %>% filter(overlap>=0 & overlap <=0.75) %>%
  group_by(Alt, group, OMR_Flow) %>%
  summarize(n = n()) %>%
  mutate(group = factor(group, levels = inflow_order)) %>%
  ggplot()  +
  geom_tile(aes(x = OMR_Flow, y = group, fill = n), color = "black") +
  geom_text(aes(x = OMR_Flow, y = group, label = n), color = "gray65", size = 2.7) +
  facet_wrap(~Alt) +
  viridis::scale_fill_viridis(option = "plasma") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()


# Run contour functions -------------------------------------
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")
alt_order = c("NAA","ALT1","ALT2a", "ALT2b", "ALT2c", "ALT2d","ALT4")
delta_sp <- as(delta_4326, "Spatial")

# NAA
lolo_contour_NAA <- f_data_interp_contour(gpname = "lolo", altname = "NAA")
lomed_contour_NAA <- f_data_interp_contour_no5500(gpname = "lomed", altname = "NAA")
lohi_contour_NAA <- f_data_interp_contour_no5500(gpname = "lohi", altname = "NAA")
medlo_contour_NAA <- f_data_interp_contour(gpname = "medlo", altname = "NAA")
medmed_contour_NAA <- f_data_interp_contour(gpname = "medmed", altname = "NAA")
medhi_contour_NAA <- f_data_interp_contour(gpname = "medhi", altname = "NAA")
hilo_contour_NAA <- f_data_interp_contour_no2000(gpname = "hilo", altname = "NAA")
himed_contour_NAA <- f_data_interp_contour(gpname = "himed", altname = "NAA")
hihi_contour_NAA <- f_data_interp_contour(gpname = "hihi", altname = "NAA")

# Alt1
lolo_contour_Alt1 <- f_data_interp_contour(gpname = "lolo", altname = "Alt1")
lomed_contour_Alt1 <- f_data_interp_contour(gpname = "lomed", altname = "Alt1")
lohi_contour_Alt1 <- f_data_interp_contour_no5500(gpname = "lohi", altname = "Alt1")
medlo_contour_Alt1 <- f_data_interp_contour_no3500(gpname = "medlo", altname = "Alt1")
medmed_contour_Alt1 <- f_data_interp_contour(gpname = "medmed", altname = "Alt1")
medhi_contour_Alt1 <- f_data_interp_contour(gpname = "medhi", altname = "Alt1")
hilo_contour_Alt1 <- f_data_interp_contour_no20005000(gpname = "hilo", altname = "Alt1")
himed_contour_Alt1 <- f_data_interp_contour_no20005000(gpname = "himed", altname = "Alt1")
hihi_contour_Alt1 <- f_data_interp_contour(gpname = "hihi", altname = "Alt1")

# Alt2a
lolo_contour_Alt2a <- f_data_interp_contour(gpname = "lolo", altname = "Alt2a")
lomed_contour_Alt2a <- f_data_interp_contour_no5500(gpname = "lomed", altname = "Alt2a")
lohi_contour_Alt2a <- f_data_interp_contour_no5500(gpname = "lohi", altname = "Alt2a")
medlo_contour_Alt2a <- f_data_interp_contour(gpname = "medlo", altname = "Alt2a")
medmed_contour_Alt2a <- f_data_interp_contour(gpname = "medmed", altname = "Alt2a")
medhi_contour_Alt2a <- f_data_interp_contour_no5500(gpname = "medhi", altname = "Alt2a")
hilo_contour_Alt2a <- f_data_interp_contour_no2000(gpname = "hilo", altname = "Alt2a")
himed_contour_Alt2a <- f_data_interp_contour(gpname = "himed", altname = "Alt2a")
hihi_contour_Alt2a <- f_data_interp_contour(gpname = "hihi", altname = "Alt2a")

# Alt2b
lolo_contour_Alt2b <- f_data_interp_contour(gpname = "lolo", altname = "Alt2b")
lomed_contour_Alt2b <- f_data_interp_contour_no5500(gpname = "lomed", altname = "Alt2b")
lohi_contour_Alt2b <- f_data_interp_contour_no5500(gpname = "lohi", altname = "Alt2b")
medlo_contour_Alt2b <- f_data_interp_contour(gpname = "medlo", altname = "Alt2b")
medmed_contour_Alt2b <- f_data_interp_contour(gpname = "medmed", altname = "Alt2b")
medhi_contour_Alt2b <- f_data_interp_contour_no5500(gpname = "medhi", altname = "Alt2b")
hilo_contour_Alt2b <- f_data_interp_contour_no2000(gpname = "hilo", altname = "Alt2b")
himed_contour_Alt2b <- f_data_interp_contour(gpname = "himed", altname = "Alt2b")
hihi_contour_Alt2b <- f_data_interp_contour(gpname = "hihi", altname = "Alt2b")

# Alt2c
lolo_contour_Alt2c <- f_data_interp_contour(gpname = "lolo", altname = "Alt2c")
lomed_contour_Alt2c <- f_data_interp_contour_no5500(gpname = "lomed", altname = "Alt2c")
lohi_contour_Alt2c <- f_data_interp_contour_no5500(gpname = "lohi", altname = "Alt2c")
medlo_contour_Alt2c <- f_data_interp_contour(gpname = "medlo", altname = "Alt2c")
medmed_contour_Alt2c <- f_data_interp_contour(gpname = "medmed", altname = "Alt2c")
medhi_contour_Alt2c <- f_data_interp_contour_no5500(gpname = "medhi", altname = "Alt2c")
hilo_contour_Alt2c <- f_data_interp_contour_no2000(gpname = "hilo", altname = "Alt2c")
himed_contour_Alt2c <- f_data_interp_contour(gpname = "himed", altname = "Alt2c")
hihi_contour_Alt2c <- f_data_interp_contour(gpname = "hihi", altname = "Alt2c")

# Alt2d
lolo_contour_Alt2d <- f_data_interp_contour(gpname = "lolo", altname = "Alt2d")
lomed_contour_Alt2d <- f_data_interp_contour_no5500(gpname = "lomed", altname = "Alt2d")
lohi_contour_Alt2d <- f_data_interp_contour_no5500(gpname = "lohi", altname = "Alt2d")
medlo_contour_Alt2d <- f_data_interp_contour(gpname = "medlo", altname = "Alt2d")
medmed_contour_Alt2d <- f_data_interp_contour(gpname = "medmed", altname = "Alt2d")
medhi_contour_Alt2d <- f_data_interp_contour_no5500(gpname = "medhi", altname = "Alt2d")
hilo_contour_Alt2d <- f_data_interp_contour_no2000(gpname = "hilo", altname = "Alt2d")
himed_contour_Alt2d <- f_data_interp_contour(gpname = "himed", altname = "Alt2d")
hihi_contour_Alt2d <- f_data_interp_contour(gpname = "hihi", altname = "Alt2d")

# Alt4
lolo_contour_Alt4 <- f_data_interp_contour(gpname = "lolo", altname = "Alt4")
lomed_contour_Alt4 <- f_data_interp_contour(gpname = "lomed", altname = "Alt4")
lohi_contour_Alt4 <- f_data_interp_contour_no5500(gpname = "lohi", altname = "Alt4")
medlo_contour_Alt4 <- f_data_interp_contour(gpname = "medlo", altname = "Alt4")
medmed_contour_Alt4 <- f_data_interp_contour(gpname = "medmed", altname = "Alt4")
medhi_contour_Alt4 <- f_data_interp_contour(gpname = "medhi", altname = "Alt4")
hilo_contour_Alt4 <- f_data_interp_contour_no20005500(gpname = "hilo", altname = "Alt4")
himed_contour_Alt4 <- f_data_interp_contour(gpname = "himed", altname = "Alt4")
hihi_contour_Alt4 <- f_data_interp_contour(gpname = "hihi", altname = "Alt4")

## Combine contours --------------------------

# 0.75 represents contour at which 75% overlap exists
# combine first by alternative
contours_all_NAA <- rbind(lolo_contour_NAA, lomed_contour_NAA, lohi_contour_NAA,
                      medlo_contour_NAA, medmed_contour_NAA, medhi_contour_NAA,
                      hilo_contour_NAA, himed_contour_NAA, hihi_contour_NAA) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

contours_all_Alt1 <- rbind(lolo_contour_Alt1, lomed_contour_Alt1, lohi_contour_Alt1,
                          medlo_contour_Alt1, medmed_contour_Alt1, medhi_contour_Alt1,
                          hilo_contour_Alt1, himed_contour_Alt1, hihi_contour_Alt1) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

contours_all_Alt2a <- rbind(lolo_contour_Alt2a, lomed_contour_Alt2a, lohi_contour_Alt2a,
                          medlo_contour_Alt2a, medmed_contour_Alt2a, medhi_contour_Alt2a,
                          hilo_contour_Alt2a, himed_contour_Alt2a, hihi_contour_Alt2a) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

contours_all_Alt2b <- rbind(lolo_contour_Alt2b, lomed_contour_Alt2b, lohi_contour_Alt2b,
                          medlo_contour_Alt2b, medmed_contour_Alt2b, medhi_contour_Alt2b,
                          hilo_contour_Alt2b, himed_contour_Alt2b, hihi_contour_Alt2b) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

contours_all_Alt2c <- rbind(lolo_contour_Alt2c, lomed_contour_Alt2c, lohi_contour_Alt2c,
                          medlo_contour_Alt2c, medmed_contour_Alt2c, medhi_contour_Alt2c,
                          hilo_contour_Alt2c, himed_contour_Alt2c, hihi_contour_Alt2c) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

contours_all_Alt2d <- rbind(lolo_contour_Alt2d, lomed_contour_Alt2d, lohi_contour_Alt2d,
                          medlo_contour_Alt2d, medmed_contour_Alt2d, medhi_contour_Alt2d,
                          hilo_contour_Alt2d, himed_contour_Alt2d, hihi_contour_Alt2d) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

contours_all_Alt4 <- rbind(lolo_contour_Alt4, lomed_contour_Alt4, lohi_contour_Alt4,
                            medlo_contour_Alt4, medmed_contour_Alt4, medhi_contour_Alt4,
                            hilo_contour_Alt4, himed_contour_Alt4, hihi_contour_Alt4) %>%
  mutate(OMR_flow = factor(flow, levels = c("-2000", "-3500", "-5000", "<-5500")))

save(contours_all_NAA, contours_all_Alt1, contours_all_Alt2a, contours_all_Alt2b, contours_all_Alt2c, contours_all_Alt2d,
     contours_all_Alt4, file = "contours_allalts.Rdata")

# Can start here if desired
# load("contours_allalts.Rdata")

# Make one contour file for all
alt_order2 = c("NAA","Alt1","Alt2woTUCPwoVA","Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA", "Alt2wTUCPwoVA", "Alt4")

contourGroup <- rbind(contours_all_NAA, contours_all_Alt1, contours_all_Alt2a, contours_all_Alt2b,
                      contours_all_Alt2c, contours_all_Alt2d, contours_all_Alt4)%>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow)) %>%
  rename(Inflow = group2) %>%
  mutate(Inflow = factor(Inflow, levels = inflow_order)) %>%
  mutate(Alt = case_when(Alt == "Alt2b" ~ "Alt2woTUCPwoVA",
                         Alt == "Alt2c" ~ "Alt2woTUCPDeltaVA",
                         Alt == "Alt2d" ~ "Alt2woTUCPAllVA",
                         Alt == "Alt2a" ~ "Alt2wTUCPwoVA",
                         TRUE ~ Alt),
         Alt = factor(Alt, levels = alt_order2))

# Make contour plots -------------------------------------------
plot_contours(alt = "NAA", cont = 0.75)
plot_contours(alt = "Alt1", cont = 0.75)
plot_contours(alt = "Alt2wTUCPwoVA", cont = 0.75)
plot_contours(alt = "Alt2woTUCPDeltaVA", cont = 0.75)
plot_contours(alt = "Alt2woTUCPAllVA", cont = 0.75)
plot_contours(alt = "Alt2woTUCPwoVA", cont = 0.75)
plot_contours(alt = "Alt4", cont = 0.75)

