##############################################
# Last updated 10/19/2023 by Catarina Pien (USBR)
# Calculate sample sizes for each alt in flow and OMR bins
# There is a version for the BA (Frequency) that needs to be edited
# and a version for ZOI which also applies to other analyses
#################################################

library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(viridis)
library(tidyr)

# This part applies to both sections.
# Read data -------------------------------

# order
alt_order = c("EXP1", "EXP3", "NAA","ALT1",
              "ALT2d", "ALT2b", "ALT2c", "ALT2a", "ALT3", "ALT4")
col_order = c("Flow", "OMR", "OMR_range",  "EXP1", "EXP3", "NAA",
              "ALT1",
              "ALT2d", "ALT2b", "ALT2c", "ALT2a",
              "ALT3", "ALT4")
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi", "NA")
omr_order = c("< -3500", ">= -3500")
pal <- c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1','#9a3324', "#88CCEE","#AA4499")

# read in data
bins <- read_excel("data_raw/calsim/Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result.xlsx", skip = 5)
# rename column names
colnames(bins) <- c("Date", "Flow_EXP1", "OMR_EXP1", "Flow_EXP3", "OMR_EXP3",
                    "Flow_NAA", "OMR_NAA", "Flow_ALT1", "OMR_ALT1",
                    "Flow_ALT2a", "OMR_ALT2a", "Flow_ALT2b", "OMR_ALT2b",
                    "Flow_ALT2c", "OMR_ALT2c", "Flow_ALT2d", "OMR_ALT2d",
                    "Flow_ALT3", "OMR_ALT3", "Flow_ALT4", "OMR_ALT4")
bins2 <- bins[-1,]

# Start here for ZOI analysis. Skip to BA section for BA analysis.
# ZOI -------------------------------
## reformat ----------------------
###  make long ---------

# use bins2 for looking at sample sizes for zone of influence
bins_long <-  bins2 %>%
  pivot_longer(
    -Date,
    cols_vary = "slowest",
    names_to = c(".value", "Alt"),
    names_sep = "_"
  ) %>%
  mutate(Month = month(Date))

## filter Dec-June --------
bins_months <- bins_long %>% filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

## calculate sample sizes -----------------

bins_summary_months <- bins_months %>%
  group_by(Flow, OMR, Alt) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Alt = factor(Alt, levels = alt_order ),
         Flow = factor(Flow, levels = inflow_order)) %>%
  arrange(Flow, OMR, Alt)

# fill in groups that have no samples; replace with zero
bins_summary_complete <- bins_summary_months %>%
  complete(Flow, OMR,
           nesting(Alt),
           fill = list(n = 0))

bins_summary_months_flowgroup <- bins_months %>%
  group_by(Flow, Alt) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Alt = factor(Alt, levels = alt_order ),
         Flow = factor(Flow, levels = inflow_order)) %>%
  arrange(Flow,Alt)

bins_summary_complete_flowgroup <- bins_summary_months_flowgroup %>%
  complete(Flow, nesting(Alt), fill = list(n=0))

# make tables ------------------------------------

# flowgroup only
bins_months_table_flowgroup <- bins_summary_complete_flowgroup %>%
  pivot_wider(names_from = "Alt", values_from = "n", values_fill = list(n = 0)) %>%
  arrange(Flow)%>%
  mutate(Flow = factor(Flow, levels = inflow_order))

# with OMR
bins_months_table <- bins_summary_complete %>%
  pivot_wider(names_from = "Alt", values_from = "n", values_fill = list(n = 0)) %>%
  arrange(Flow, OMR)%>%
  mutate(Flow = factor(Flow, levels = inflow_order),
         OMR_range = case_when(OMR == "-2000" ~ "-2500 to -1500",
                               OMR == "-3500" ~ "-4000 to -3000",
                               OMR == "-5000" ~ "-5500 to -4500",
                               OMR == "<-5500" ~ "less than -5500",
                               TRUE~"NA"))
bins_months_table_v2 <- bins_months_table[, col_order]

# calculate proportion in each group by alt
bins_months_prop_table_v2 <- bins_months_table_v2 %>%
  mutate(across(EXP1:ALT4, ~.x/sum(.x))) %>%
  mutate(across(EXP1:ALT4, ~round(.x,3)))

# calculate proportion NA
not_included <- bins_summary_complete %>%
  filter((Flow == "NA" | OMR == "NA") | (Flow == "NA" & OMR == "NA")) %>%
  group_by(Alt) %>%
  summarize(num =sum(n),
            prop = num/700)

# plot results --------------------------
bins_plotting <- bins_summary_complete %>%
  mutate(n = replace(n, n == 0, NA))

ggplot(bins_plotting) + geom_tile(aes(OMR, Flow, fill = n), color = "gray50") +
  facet_wrap(~Alt) +
  # scale_fill_viridis(option = "plasma") +
  scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) )

ggplot(bins_plotting%>% filter(!Alt %in% c("EXP1", "EXP3"))) +
  geom_tile(aes(OMR, Flow, fill = n), color = "gray50") +
  facet_wrap(~Alt) +
  scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) )

(plot_n_alts_omr_inflow_v2 <- ggplot(bins_plotting ) +
    geom_tile(aes(OMR, Alt, fill = n), color = "gray50") +
    facet_wrap(~Flow) +
    scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.position = "top"))

(plot_n_alts_omr_inflow_noexp_v2 <-ggplot(bins_plotting %>% filter(!Alt %in% c("EXP1", "EXP3"))) +
    geom_tile(aes(OMR, Alt, fill = n), color = "gray50") +
    facet_wrap(~Flow) +
    scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 14),
          legend.position = "top"))

ggplot(bins_months) + geom_boxplot(aes(x = Sac.cfs, y = ))

# export------------

## write plots------------
png("figures/plot_n_alts_omr_inflow_zoi.png", width = 8, height = 8, units = "in", res = 300, pointsize = 10)
plot_n_alts_omr_inflow_v2
dev.off()

png("figures/plot_n_alts_omr_inflow_no_exp_zoi.png", width = 8, height = 8, units = "in", res = 300, pointsize = 10)
plot_n_alts_omr_inflow_noexp_v2
dev.off()

## Write tables --------------------------
write_csv(bins_months_table_flowgroup, "data_export/flowbin_samplesizes_acrossalts_zoi.csv")
write_csv(bins_months_table, "data_export/bin_samplesizes_acrossalts_zoi.csv")
write_csv(bins_months_prop_table, "data_export/bin_prop_samplesizes_acrossalts_zoi.csv")












# This part applies to BA.
# BA bins include grouping all the "NA" values into separate bins.
# BA ------------------------
## reformat ----------------------
omr_bins <- read_excel("data_raw/calsim/Reclamation_2021LTO_OMR_rev01_20231010.xlsx", skip = 11)
bins2_flow <- dplyr::select(bins2, contains("Flow"))
bins_upd <- cbind(omr_bins, bins2_flow)

colnames(bins_upd) <- c("Date", "OMR_EXP1",  "OMR_EXP3","OMR_NAA",
                     "OMR_ALT1","OMR_ALT2a",
                    "OMR_ALT2b","OMR_ALT2c", "OMR_ALT2d","OMR_ALT3","OMR_ALT4",
                    "Flow_EXP1",  "Flow_EXP3", "Flow_NAA", "Flow_ALT1",
                    "Flow_ALT2a", "Flow_ALT2b", "Flow_ALT2c", "Flow_ALT2d",
                    "Flow_ALT3", "Flow_ALT4" )

###  make long ---------

# use bins_upd for looking at frequency in the BA
bins_long <-  bins_upd %>%
  pivot_longer(
    -Date,
    cols_vary = "slowest",
    names_to = c(".value", "Alt"),
    names_sep = "_"
  ) %>%
  mutate(Month = month(Date))

### filter Dec-June --------
bins_months <- bins_long %>% filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))


### regroup OMR (freq analysis only) ---------
bins_freq <- bins_months %>%
  mutate(OMR_group = case_when(OMR >= -3500 ~ ">= -3500",
                               OMR < -3500 ~ "< -3500",
                               TRUE ~ "Other")) %>%
  rename(OMR_val = OMR,
         OMR = OMR_group)

# bins_freq <- bins_months %>%
#   mutate(OMR_group = case_when(OMR > -1500 ~ "positive",
#                                 OMR >= -2500 & OMR <=-1500 ~ "-2000",
#                                 OMR >= -4000 & OMR <=-3000 ~ "-3500",
#                                 OMR >= -5500 & OMR <=-4500 ~ "-5000",
#                                 OMR <=-5500 ~ "less than -5500",
#                                 TRUE ~ "Other")) %>%
#   rename(OMR_val = OMR,
#          OMR = OMR_group)

### calculate sample sizes -----------------
bins_summary_months <- bins_freq %>%
  group_by(Flow, OMR, Alt) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Alt = factor(Alt, levels = alt_order ),
         Flow = factor(Flow, levels = inflow_order),
         OMR = factor(OMR, levels = omr_order)) %>%
         # OMR = factor(OMR, levels = c("positive", "-2000", "-3500", "-5000", "less than -5500", "Other"))) %>%
  arrange(Flow, OMR, Alt)

# fill in groups that have no samples; replace with zero
bins_summary_complete <- bins_summary_months %>%
  complete(Flow, OMR,
           nesting(Alt),
           fill = list(n = 0))

## make tables ------------------------------------

# sample sizes
bins_months_table <- bins_summary_complete %>%
  pivot_wider(names_from = "Alt", values_from = "n", values_fill = list(n = 0)) %>%
  arrange(Flow, OMR)%>%
  mutate(Flow = factor(Flow, levels = inflow_order),
         OMR_range = case_when(OMR == "< -3500" ~ "less than -3500",
                               OMR == ">= -3500" ~ "greater than or equal to -3500"))
bins_months_table <- bins_months_table[, col_order]

# calculate proportion in each group by alt
bins_months_prop_table <- bins_months_table %>%
  mutate(across(EXP1:ALT4, ~.x/sum(.x))) %>%
  mutate(across(EXP1:ALT4, ~round(.x,3)))

# calculate proportion NA
not_included <- bins_summary_complete %>%
  # filter(OMR %in% c("Other", "positive")) %>%
  filter((Flow == "NA" | OMR == "Other") | (Flow == "NA" & OMR == "Other")) %>%
  group_by(Alt, OMR) %>%
  summarize(num =sum(n),
            prop = round(num/700,2))

# not_included_wide <- not_included %>%
#   select(-num) %>%
#   pivot_wider(names_from = "Alt", values_from = "prop")
# write_csv(not_included_wide, "data_export/data_not_included_allalts.csv")

## plot results --------------------------
bins_plotting <- bins_summary_complete %>%
  mutate(n = replace(n, n == 0, NA))

ggplot(bins_plotting) + geom_tile(aes(OMR, Flow, fill = n), color = "gray50") +
  facet_wrap(~Alt) +
  # scale_fill_viridis(option = "plasma") +
  scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) )

ggplot(bins_plotting%>% filter(!Alt %in% c("EXP1", "EXP3"))) +
  geom_tile(aes(OMR, Flow, fill = n), color = "gray50") +
  facet_wrap(~Alt) +
  scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) )

(plot_n_alts_omr_inflow <- ggplot(bins_plotting) +
    geom_tile(aes(OMR, Alt, fill = n), color = "gray50") +
    facet_wrap(~Flow) +
    scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.position = "top"))

(plot_n_alts_omr_inflow_noexp <-ggplot(bins_plotting %>% filter(!Alt %in% c("EXP1", "EXP3"))) +
    geom_tile(aes(OMR, Alt, fill = n), color = "gray50") +
    facet_wrap(~Flow) +
    scale_fill_viridis_c(na.value = "gray90", option = "plasma") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 14),
          legend.position = "top"))

## export------------

## write plots------------
png("figures/plot_n_alts_omr_inflow_freq.png", width = 8, height = 8, units = "in", res = 300, pointsize = 10)
plot_n_alts_omr_inflow
dev.off()

png("figures/plot_n_alts_omr_inflow_noexp_freq.png", width = 8, height = 8, units = "in", res = 300, pointsize = 10)
plot_n_alts_omr_inflow_noexp
dev.off()


## Write tables --------------------------
write_csv(bins_months_table, "data_export/bin_samplesizes_acrossalts_freq.csv")
write_csv(bins_months_prop_table, "data_export/bin_prop_samplesizes_acrossalts_freq.csv")


