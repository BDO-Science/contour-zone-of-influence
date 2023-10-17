##############################################
# Last updated 10/17/2023 by Catarina Pien (USBR)
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

# Read data -------------------------------

# order
alt_order = c("EXP1", "EXP3", "NAA",
              "ALT1",
              "ALT2v1", "ALT2v2", "ALT2v3", "ALT2v4",
              "ALT3", "ALT4")
col_order = c("Flow", "OMR", "OMR_range",  "EXP1", "EXP3", "NAA",
              "ALT1",
              "ALT2v1", "ALT2v2", "ALT2v3", "ALT2v4",
              "ALT3", "ALT4")
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi", "NA")
omr_order = c("greater than -1000", "-2000", "-3500", "-5000", "less than -5500")

# read in data
omr_bins <- read_excel("data_raw/calsim/Reclamation_2021LTO_OMR_rev01_20231010.xlsx", skip = 11)
bins <- read_excel("data_raw/calsim/Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result.xlsx", skip = 5)

# rename column names
colnames(bins) <- c("Date", "Flow_EXP1", "OMR_EXP1", "Flow_EXP3", "OMR_EXP3",
                    "Flow_NAA", "OMR_NAA", "Flow_ALT1", "OMR_ALT1",
                    "Flow_ALT2v1", "OMR_ALT2v1", "Flow_ALT2v2", "OMR_ALT2v2",
                    "Flow_ALT2v3", "OMR_ALT2v3", "Flow_ALT2v4", "OMR_ALT2v4",
                    "Flow_ALT3", "OMR_ALT3", "Flow_ALT4", "OMR_ALT4")
bins2 <- bins[-1,]



bins2_flow <- select(bins2, contains("Flow"))
bins_upd <- cbind(omr_bins, bins2_flow)

colnames(bins_upd) <- c("Date", "OMR_EXP1",  "OMR_EXP3","OMR_NAA",
                     "OMR_ALT1","OMR_ALT2v1",
                    "OMR_ALT2v2","OMR_ALT2v3", "OMR_ALT2v4","OMR_ALT3","OMR_ALT4",
                    "Flow_EXP1",  "Flow_EXP3", "Flow_NAA", "Flow_ALT1",
                    "Flow_ALT2v1", "Flow_ALT2v2", "Flow_ALT2v3", "Flow_ALT2v4",
                    "Flow_ALT3", "Flow_ALT4" )



# There is a BA and ZOI section. Go to # ZOI section for the analysis bins.
# BA bins include grouping all the "NA" values into separate bins.

# BA ------------------------
## reformat ----------------------
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
  mutate(OMR_group = case_when(OMR > -1000 ~ "greater than -1000",
                               OMR > -2500 & OMR <=-1000 ~ "-2000",
                               OMR > -4000 & OMR <=-2500 ~ "-3500",
                               OMR > -5500 & OMR <=-4000 ~ "-5000",
                               OMR <=-5500 ~ "less than -5500",
                               TRUE ~ "Other")) %>%
  rename(OMR_val = OMR,
         OMR = OMR_group)

### calculate sample sizes -----------------
bins_summary_months <- bins_freq %>%
  group_by(Flow, OMR, Alt) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Alt = factor(Alt, levels = alt_order ),
         Flow = factor(Flow, levels = inflow_order),
         OMR = factor(OMR, levels = omr_order)) %>%
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
         OMR_range = case_when(OMR == "greater than -1000" ~ "greater than -1000",
                               OMR == "-2000" ~ "-2500 to -1000",
                               OMR == "-3500" ~ "-4000 to -2500",
                               OMR == "-5000" ~ "-5500 to -4000",
                               OMR == "less than -5500" ~ "less than -5500"))
bins_months_table <- bins_months_table[, col_order]

# calculate proportion in each group by alt
bins_months_prop_table <- bins_months_table %>%
  mutate(across(EXP1:ALT4, ~.x/sum(.x))) %>%
  mutate(across(EXP1:ALT4, ~round(.x,3)))

# calculate proportion NA
not_included <- bins_summary_complete %>%
  filter((Flow == "NA" | OMR == "NA") | (Flow == "NA" & OMR == "NA")) %>%
  group_by(Alt) %>%
  summarize(num =sum(n),
            prop = num/700)

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


# There are different calculations for analyses vs BA frequency analysis
# Start here for ZOI analysis

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

# filter to months of interest (12-6)
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

# make tables ------------------------------------

# sample sizes
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

# export------------

## write plots------------
png("figures/plot_n_alts_omr_inflow_zoi.png", width = 8, height = 8, units = "in", res = 300, pointsize = 10)
plot_n_alts_omr_inflow_v2
dev.off()

png("figures/plot_n_alts_omr_inflow_no_exp_zoi.png", width = 8, height = 8, units = "in", res = 300, pointsize = 10)
plot_n_alts_omr_inflow_noexp_v2
dev.off()

## Write tables --------------------------
write_csv(bins_months_table, "data_export/bin_samplesizes_acrossalts_zoi.csv")
write_csv(bins_months_prop_table, "data_export/bin_prop_samplesizes_acrossalts_zoi.csv")
