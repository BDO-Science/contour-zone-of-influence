##### channel_lengths_barplots.R ######
# contour_maps_inflow.R ######
# Updated: 11/16/2023
# Catarina Pien and Lisa Elliott (USBR)
# cpien@usbr.gov; lelliott@usbr.gov
# This code uses zone of influence modeling results (DSM2) to create barplots showing
# how channel length associated with the zone of influence changes from operational facilities based on pumping
# to different OMR levels, at different SJR and SR flow levels and alternatives

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
library(ggpattern)
source("functions_zoi.R")

# Ordering ----------------------------------
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")
# alt_order = c("EXP1", "EXP3", "NAA","ALT1","Alt2woTUCPwoVA","Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA", "Alt2wTUCPwoVA", "ALT3", "ALT4")
alt_order = c("NAA","Alt1","Alt2woTUCPwoVA","Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA", "Alt2wTUCPwoVA", "Alt4")

# Read data ---------------------------------------------------------

zoi_channel_group <- read_csv("data_export/prop_overlap_data_long.csv") %>%
  filter(overlap>=0)# from contour_maps_inflow_allalts.R

n_flow_OMR_alt <- read_csv("data_export/samplesizes_flow_OMR_alt.csv")# from calculate_frequency_bins_alts.R

# Filter to contours of interest -------------------------------------
# Based on BA
filtered_dat_high <- zoi_channel_group %>%
  filter(overlap < 0.25)

filtered_dat_med <- zoi_channel_group %>%
  filter(overlap >= 0.25 & overlap <= 0.75)

filtered_dat_low <- zoi_channel_group %>%
  filter(overlap > 0.75)


# Calculate total length for each group -------------------------
filtered2_high <- filtered_dat_high %>%
  group_by(group, OMR_Flow, Alt) %>%
  summarize(sumLength = sum(length))%>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")))%>%
  mutate(Alt = case_when(Alt == "Alt2b" ~ "Alt2woTUCPwoVA",
                         Alt == "Alt2c" ~ "Alt2woTUCPDeltaVA",
                         Alt == "Alt2d" ~ "Alt2woTUCPAllVA",
                         Alt == "Alt2a" ~ "Alt2wTUCPwoVA",
                         TRUE ~ Alt),
         Alt = factor(Alt, levels = alt_order)) %>%
  mutate(pLength = sumLength/total_channel_length) %>%
  mutate(h_influence = "High hydrologic influence")

filtered2_med <- filtered_dat_med %>%
  group_by(group, OMR_Flow, Alt) %>%
  summarize(sumLength = sum(length))%>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi"))) %>%
  mutate(Alt = case_when(Alt == "Alt2b" ~ "Alt2woTUCPwoVA",
                         Alt == "Alt2c" ~ "Alt2woTUCPDeltaVA",
                         Alt == "Alt2d" ~ "Alt2woTUCPAllVA",
                         Alt == "Alt2a" ~ "Alt2wTUCPwoVA",
                         TRUE ~ Alt),
         Alt = factor(Alt, levels = alt_order)) %>%
  mutate(pLength = sumLength/total_channel_length) %>%
  mutate(h_influence = "Medium hydrologic influence") %>%
  mutate(Alt = factor(Alt, levels = alt_order))

filtered2_low <- filtered_dat_low %>%
  group_by(group, OMR_Flow, Alt) %>%
  summarize(sumLength = sum(length))%>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi"))) %>%
  mutate(Alt = case_when(Alt == "Alt2b" ~ "Alt2woTUCPwoVA",
                         Alt == "Alt2c" ~ "Alt2woTUCPDeltaVA",
                         Alt == "Alt2d" ~ "Alt2woTUCPAllVA",
                         Alt == "Alt2a" ~ "Alt2wTUCPwoVA",
                         TRUE ~ Alt),
         Alt = factor(Alt, levels = alt_order)) %>%
  mutate(pLength = sumLength/total_channel_length) %>%
  mutate(h_influence = "Low hydrologic influence")

#* Combine files and rename to  official names ---------------------------
filtered_dat <- rbind(filtered2_high, filtered2_med, filtered2_low) %>%
  mutate(h_influence = factor(h_influence, levels = c("Low hydrologic influence", "Medium hydrologic influence", "High hydrologic influence")),
        Alt = case_when(Alt == "Alt2b" ~ "Alt2woTUCPwoVA",
                        Alt == "Alt2c" ~ "Alt2woTUCPDeltaVA",
                        Alt == "Alt2d" ~ "Alt2woTUCPAllVA",
                        Alt == "Alt2a" ~ "Alt2wTUCPwoVA",
                        TRUE ~ Alt),
        Alt = factor(Alt, levels = alt_order))

filtered_dat_BA <- filtered_dat %>%
  filter(!(Alt %in% c("Alt1", "Alt4", "Alt2wTUCPwoVA")))

# Visualize differences -------------------
pal <- c('#9a3324', "#88CCEE","#AA4499",'#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1')

# unstacked
barplot_omr <- ggplot(filtered_dat) +
  geom_col(aes(Alt, pLength, fill = OMR_Flow), position = "dodge2")  +
  labs(y = "Proportional Channel Length") +
  viridis::scale_fill_viridis(discrete = TRUE, option = "turbo") +
  facet_grid(group~h_influence) +
  theme_bw()
barplot_omr

#* look at just medium -------------------

# Add in sample size
med_barplot_data <- left_join(filtered2_med, n_flow_OMR_Alt %>% dplyr::select(OMR_Flow = OMR, Alt, group = Sub.group, n)) %>%
  mutate(low_n = if_else(n<5, "*", "")) %>%
  complete(OMR_Flow, group, nesting(Alt)) %>%
  filter(group!="NA")

#** EIS figures ---------------
(med_barplot_eis <- med_barplot_data %>%
  ggplot() +
  geom_col(aes(OMR_Flow, pLength, fill = Alt), position= position_dodge(0.75, preserve = "single"), width = 0.6) +
  geom_text(aes(OMR_Flow, pLength, group = Alt, label = low_n), vjust = -0.01, position= position_dodge(0.75))+
  facet_wrap(~group) +
  labs(y = "Proportional Channel Length", x = "OMR Bin", fill = "Alternative") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "top") +
  scale_fill_manual(values = pal[c(3,4,5,6,7,8,10)]))
ggsave(filename="figures/attachment_plots/med_influence_omr_barplots_eis.png", plot=med_barplot_eis, height = 5, width = 6.5, units = "in")

(med_barplot_inflow_eis <- med_barplot_data %>%
  ggplot() +
  geom_col(aes(group, pLength, fill = Alt), position= position_dodge(0.75, preserve = "single"), width = 0.6) +
  geom_text(aes(group, pLength, group = Alt, label = low_n), vjust = -0.1, position= position_dodge(0.75))+
  facet_wrap(~OMR_Flow, nrow = 4) +
  labs(y = "Proportional Channel Length", x = "Inflow Group", fill = "Alternative") +
  theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "top") +
  scale_fill_manual(values = pal[c(3,4,5,6,7,8,10)]))
ggsave(filename="figures/attachment_plots/med_influence_inflow_barplots_eis.png", plot=med_barplot_inflow_eis, height =8, width = 6, units = "in")

(med_barplot_inflow_alt <- med_barplot_data %>%
    ggplot() +
    geom_col(aes(Alt, pLength, fill = OMR_Flow), position= position_dodge(0.75, preserve = "single"), width = 0.6) +
    geom_text(aes(Alt, pLength, group = OMR_Flow, label = low_n), vjust = -0.1, position= position_dodge(0.75))+
    facet_wrap(~group, nrow = 4) +
    labs(y = "Proportional Channel Length", x = "Inflow Group", fill = "Alternative") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "top") +
    scale_fill_manual(values = pal[c(3,4,5,6,7,8,10)]))

#** BA figures -----------------------------
med_barplot_data_ba <- med_barplot_data %>%
  filter(!(Alt %in% c("Alt1", "Alt4", "Alt2wTUCPwoVA")))

(med_barplot_ba <- med_barplot_data_ba %>%
   ggplot() +
   geom_col(aes(OMR_Flow, pLength, fill = Alt), position= position_dodge(0.75, preserve = "single"), width = 0.6) +
   geom_text(aes(OMR_Flow, pLength, group = Alt, label = low_n), vjust = -0.01, position= position_dodge(0.75))+
   facet_wrap(~group) +
   labs(y = "Proportional Channel Length", x = "OMR Bin", fill = "PA Components") +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90),
         legend.position = "top") +
    guides(fill = guide_legend(nrow = 2)) +
   scale_fill_manual(values = pal[c(3,5,6,7)]))
ggsave(filename="figures/attachment_plots/med_influence_omr_barplots_ba.png", plot=med_barplot_ba, height = 5, width = 6.5, units = "in")

(med_barplot_inflow_ba <- med_barplot_data_ba %>%
    ggplot() +
    geom_col(aes(group, pLength, fill = Alt), position= position_dodge(0.75, preserve = "single"), width = 0.6) +
    geom_text(aes(group, pLength, group = Alt, label = low_n), vjust = -0.1, position= position_dodge(0.75))+
    facet_wrap(~OMR_Flow, nrow = 4) +
    labs(y = "Proportional Channel Length", x = "Inflow Group", fill = "PA Components") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "top") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = pal[c(3,5,6,7)]))
ggsave(filename="figures/attachment_plots/med_influence_inflow_barplots_ba.png", plot=med_barplot_inflow_ba, height =8, width = 6, units = "in")

# make channel length table --------------------------------------
med_table <- filtered2_med %>%
  dplyr::select(-pLength,-h_influence) %>%
  mutate(Alt = factor(Alt, levels = alt_order)) %>%
  pivot_wider(names_from = "Alt", values_from = "sumLength", values_fill = list(n = 0)) %>%
  arrange(group) %>%
  mutate(group = factor(group, levels = inflow_order))%>%
  dplyr::select(group, OMR_Flow, NAA, Alt1, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA, everything())

med_table_long <- med_table %>%
  pivot_longer(cols = NAA:Alt4, values_to = "sumLength", names_to = "Alt")

med_prop <- med_table %>%
  dplyr::select(group, OMR_Flow, NAA, everything()) %>%
  mutate(across(NAA:Alt4, ~ round((.x-NAA)/NAA * 100))) %>%
  pivot_longer(cols = NAA:Alt4, values_to = "changeLength", names_to = "Alt")

write_csv(med_table_EIS, "data_export/tab9_medium_hydro_channel_length_EIS.csv")
#** EIS --------------------------------
med_table_EIS <- left_join(med_table_long, med_prop) %>%
  # mutate_if(is.numeric, ~as.character(.),
            # is.character, ~replace_na(.,""))
  mutate(length_change = paste0(sumLength, " (", changeLength, "%)" )) %>%
  mutate(length_change = replace(length_change, length_change == "NA (NA%)", "NA")) %>%
  dplyr::select(-sumLength, -changeLength) %>%
  pivot_wider(names_from = "Alt", values_from = "length_change", values_fill = list(n = 0)) %>%
  arrange(group) %>%
  mutate(group = factor(group, levels = inflow_order))%>%
  dplyr::select(`Inflow group` = group, `OMR bin` = OMR_Flow,
                NAA, Alt1, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA, everything())
#** BA ---------------------
med_table_BA <- med_table %>%
  dplyr::select(`Inflow group` = group, `OMR bin` = OMR_Flow, NAA, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA)

write_csv(med_table_BA, "data_export/tab8_medium_hydro_channel_length_BA.csv")


# Calculate lows and highs for narrative -------------------------
ordered_medium <- med_table_long %>% arrange(sumLength) %>%
  mutate(propLength = round(sumLength/3849200 *100,2))

ordered_medium_prop <- med_prop %>% arrange(changeLength)

ordered_medium_BA <- med_table_long %>% arrange(sumLength) %>%
  filter(!(Alt %in% c("Alt1", "Alt4", "Alt2wTUCPwoVA", "NAA"))) %>%
  mutate(propLength = round(sumLength/3849200 *100,2))

ordered_medium_Alt2 <- med_table_long %>% arrange(sumLength) %>%
  filter(!(Alt %in% c("Alt1", "Alt4",  "NAA"))) %>%
  mutate(propLength = round(sumLength/3849200 *100,2))

# write out all the stacked barplots -----------------------------
lapply(inflow_order, plot_barplot)
lapply(alt_order, plot_barplot_alt)

# individual plot
plot_barplot("lohi")
plot_barplot_BA("NAA")
plot_barplot_alt(alt = "NAA")

# faceted barplot for all alts
barplot_f <-
  filtered_dat %>%
  ggplot(aes(x = Alt, y = pLength, fill = Alt, pattern = h_influence)) +
  geom_col_pattern(color = "black",
                   pattern_color = "black",
                   pattern_fill = "black",
                   pattern_spacing = 0.05,
                   pattern_size = 0.4,
                   alpha = 0.9)  +
  labs(y = "Proportional Channel Length") +
  scale_pattern_manual(values = c("none", "circle", "stripe")) +
  facet_grid(group~OMR_Flow) +
  scale_fill_manual(values = pal[c(3,4,5,6,7,8,10)]) +
  theme_classic() +
  theme(legend.position = "top",
        legend.box = "vertical",
        axis.text.x = element_text(angle = 90),
        axis.title = element_blank())
ggsave(filename="figures/attachment_plots/stacked_barplot_all.png", plot=barplot_f, height = 10, width = 7, units = "in")
