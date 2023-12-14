##############################################
# Last updated 12/8/2023 by Catarina Pien (USBR)
# Contact: cpien@usbr.gov
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
library(here)
library(purrr)

# DSM2 data input ---------------------------
# This version of frequency is currently the most up to date.
## Read data -------------------------------

# order
alt_order = c("EXP1", "EXP3", "NAA","ALT1",
              "ALT2d", "ALT2b", "ALT2c", "ALT2a", "ALT3", "ALT4")
alt_order2 =c("NAA","Alt1","Alt2woTUCPwoVA","Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA", "Alt2wTUCPwoVA","Alt3", "Alt4")
col_order = c("Flow", "OMR", "OMR_range",  "EXP1", "EXP3", "NAA",
              "ALT1",
              "ALT2d", "ALT2b", "ALT2c", "ALT2a",
              "ALT3", "ALT4")
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi", "NA")
omr_order = c("-2000","-3500","-5000", "<-5500",  "NA")



# Read in data (DSM2 processing)

fp <- dir(here::here("data_raw/DSM2_samples"), full.names = TRUE)
dsm2bins <- map(fp, read_csv, col_types = cols(.default = "c"))
walk(dsm2bins, glimpse)

bins <- bind_rows(dsm2bins, .id = "id") %>%
  mutate(Alt = case_when(id == 1 ~ "Alt1",
                         id ==2 ~ "Alt2b",
                         id == 3 ~ "Alt2a",
                         id == 4 ~ "Alt2c",
                         id ==5 ~ "Alt2d",
                         id ==6~ "Alt3",
                         id == 7 ~ "Alt4",
                         id == 8~ "NAA")) %>%
  mutate(Sub.group = replace(Sub.group, is.na(Sub.group), "NA")) %>%
  rename(Date = month_year) %>%
  dplyr::select(-Date_x) %>%
  distinct() %>%
  mutate(OMR = case_when(OMR_bin == -13000 ~ "<-5500",
                         OMR_bin == 99999 ~ "NA",
                         TRUE~ OMR_bin)) %>%
  mutate(Alt = case_when(Alt == "Alt2b" ~ "Alt2woTUCPwoVA",
                         Alt == "Alt2c" ~ "Alt2woTUCPDeltaVA",
                         Alt == "Alt2d" ~ "Alt2woTUCPAllVA",
                         Alt == "Alt2a" ~ "Alt2wTUCPwoVA",
                         TRUE ~ Alt)) %>%
  mutate(OMR = factor(OMR, levels = omr_order),
         Alt = factor(Alt, levels = alt_order2),
         Sub.group = factor(Sub.group, levels = inflow_order))


## calculate sample sizes --------------------------
# this is used later for channel length code

# OMR and inflow bins
n_flow_OMR_Alt <- bins %>%
  group_by(Alt, Sub.group, OMR) %>%
  summarize(n = n(),
            percent = round(n/700*100))%>%
  ungroup() %>%
  complete(Sub.group, OMR, nesting(Alt),
           fill = list(n = 0))  %>%
  ungroup()


# inflow bins only
n_flow_Alt <- bins %>%
  group_by(Alt, Sub.group) %>%
  summarize(n = n(),
            percent = round(n/700*100))%>%
  ungroup() %>%
  complete(Sub.group, nesting(Alt),
           fill = list(n = 0))

### write out ------------------------
 # write_csv(n_flow_OMR_Alt, "data_export/samplesizes_flow_OMR_alt.csv")
 # write_csv(n_flow_Alt, "data_export/samplesizes_flow_alt.csv")

### plot sample size data --------------------------------
pal <- c('#9a3324', "#88CCEE","#AA4499",'#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1')

(dsm_sample_plot_flow_omr <- ggplot(n_flow_OMR_Alt) + geom_tile(aes(x = Alt, y = OMR, fill = n), color = "gray70") +
  geom_text(aes(x = Alt, y = OMR, label = n), color = "white") +
  facet_wrap(~Sub.group) +
  scale_fill_viridis(option = "plasma") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)))
ggsave("figures/plot_dsm2_samples_omr_inflow.png",
       plot = dsm_sample_plot_flow_omr, units = "in", width = 8, height = 8)

(dsm_sample_plot_flow_omr <- ggplot(n_flow_OMR_Alt) + geom_tile(aes(x = Alt, y = OMR, fill = n), color = "gray70") +
  geom_text(aes(x = Alt, y = OMR, label = n), color = "white") +
  facet_wrap(~Sub.group) +
    labs(y = "OMR bin (cfs)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top",
        axis.title.x = element_blank(),
        strip.text = element_text(size= 10)))
ggsave("figures/plot_dsm2_samples_omr_inflow_blue.png",
       plot = dsm_sample_plot_flow_omr, units = "in", width = 8, height = 9)

(dsm_sample_barplot_flow <- ggplot(data = n_flow_OMR_Alt, aes(x = Alt, y = n, fill = OMR), position = dodge2) +
    geom_col() +
    facet_wrap(~Sub.group) +
    geom_text(aes(label = n), position = position_stack(vjust = .5), color = "gray50") +
    scale_fill_viridis_d(option = "plasma") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)))
ggsave("figures/plot_dsm2_samples_inflow.png",
       plot = dsm_sample_barplot_flow, units = "in", width = 8, height = 8)

(dsm_sample_plot_flow <- ggplot(data = n_flow_Alt, aes(x = Alt, y = percent, fill = Sub.group)) +
  geom_col() +
  geom_text(aes(label = percent), position = position_stack(vjust = .5), color = "gray50") +
  scale_fill_viridis_d(option = "plasma") +
  theme(axis.text.x = element_text(angle = 90)))
ggsave("figures/plot_dsm2_samples_inflow.png",
       plot = dsm_sample_plot_flow, units = "in", width = 8, height = 8)

### summary stats tables ---------------------------------
dsm2_samples_inflow_table <- n_flow_Alt %>%
  rename(`Inflow Group` = Sub.group) %>%
  dplyr::select(-percent) %>%
  pivot_wider(names_from = "Alt", values_from = "n", values_fill = 0)

dsm2_samples_inflow_omr_table <- n_flow_OMR_Alt %>%
  rename(`Inflow Group` = Sub.group, `OMR Bin` = "OMR") %>%
  dplyr::select(-percent) %>%
  pivot_wider(names_from = "Alt", values_from = "n", values_fill = 0) %>%
  arrange(`Inflow Group`, `OMR Bin`)
write_csv(dsm2_samples_inflow_omr_table, "data_export/tab4_samplesizes_inflowgroups_omr_alts_dsm2.csv")

samples_excluded <- n_flow_OMR_Alt %>%
  dplyr::select(-percent, -Sub.group) %>%
  filter(OMR == "NA") %>%
  group_by(Alt, OMR) %>%
  summarize(prop = sum(n)/700)%>%
  pivot_wider(names_from = "Alt",
              values_from = prop)
write_csv(dsm2_samples_inflow_table, "data_export/tab2_samplesizes_inflowgroups_alts_dsm2.csv")


# Calsim 3 input data-----------------------------
# These data are used for other analyses and were used to determine bins. However, these do not
# reflect actual sample sizes for zoi analysis (similar but slight differences)

# read in data (Calsim inputs from Steve's binning document) -----------------------
bins <- read_excel("data_raw/calsim/Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result.xlsx", skip = 5)
# rename column names
colnames(bins) <- c("Date", "Flow_EXP1", "OMR_EXP1", "Flow_EXP3", "OMR_EXP3",
                    "Flow_NAA", "OMR_NAA", "Flow_ALT1", "OMR_ALT1",
                    "Flow_ALT2a", "OMR_ALT2a", "Flow_ALT2b", "OMR_ALT2b",
                    "Flow_ALT2c", "OMR_ALT2c", "Flow_ALT2d", "OMR_ALT2d",
                    "Flow_ALT3", "OMR_ALT3", "Flow_ALT4", "OMR_ALT4")
bins2 <- bins[-1,]

# Start here for ZOI analysis. Skip to BA section for BA analysis.
## ZOI -------------------------------
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


