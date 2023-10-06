##############################################
# Last updated 10/6/2023 by Catarina Pien (USBR)
# Calculate sample sizes for each alt in flow and OMR bins
#################################################

library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(viridis)
library(tidyr)

# Read dat -------------------------------

# read data
bins <- read_excel("data_raw/calsim/Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result.xlsx", skip = 5)
alt_order = c("EXP1", "EXP3", "NAA",
              "ALT1",
              "ALT2v1", "ALT2v2", "ALT2v3", "ALT2v4",
              "ALT3", "ALT4")
col_order = c("Flow", "OMR", "EXP1", "EXP3", "NAA",
              "ALT1",
              "ALT2v1", "ALT2v2", "ALT2v3", "ALT2v4",
              "ALT3", "ALT4")
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi", "NA")
# rename column names
colnames(bins) <- c("Date", "Flow_EXP1", "OMR_EXP1", "Flow_EXP3", "OMR_EXP3",
                    "Flow_NAA", "OMR_NAA", "Flow_ALT1", "OMR_ALT1",
                    "Flow_ALT2v1", "OMR_ALT2v1", "Flow_ALT2v2", "OMR_ALT2v2",
                    "Flow_ALT2v3", "OMR_ALT2v3", "Flow_ALT2v4", "OMR_ALT2v4",
                    "Flow_ALT3", "OMR_ALT3", "Flow_ALT4", "OMR_ALT4")
bins2 <- bins[-1,]
# make long ----------------------
bins_long <-  bins2 %>%
  pivot_longer(
    -Date,
    cols_vary = "slowest",
    names_to = c(".value", "Alt"),
    names_sep = "_"
  ) %>%
  mutate(Month = month(Date))

# Dec-June
bins_months <- bins_long %>% filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

# calculate sample sizes
bins_summary_all <- bins_long %>%
  group_by(Flow, OMR, Alt) %>%
  summarize(n = n())

# filter to months of interest (12-6)
bins_summary_months <- bins_months %>%
  group_by(Flow, OMR, Alt) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Alt = factor(Alt, levels = alt_order ),
         Flow = factor(Flow, levels = inflow_order))

# fill in groups that have no samples; replace with zero
bins_summary_complete <- bins_summary_months %>%
  complete(Flow,
           nesting(OMR, Alt),
           fill = list(n = 0))

# table by alternative
# sample sizes
bins_months_table <- bins_summary_complete %>%
  pivot_wider(names_from = "Alt", values_from = "n", values_fill = list(n = 0)) %>%
  arrange(Flow, OMR)%>%
  mutate( Flow = factor(Flow, levels = inflow_order))
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

# look at results
ggplot(bins_summary_months) + geom_tile(aes(OMR, Flow, fill = n), color = "gray50") +
  facet_wrap(~Alt) +
  scale_fill_viridis(option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) )

ggplot(bins_summary_months%>% filter(!Alt %in% c("EXP1", "EXP3"))) +
  geom_tile(aes(OMR, Flow, fill = n), color = "gray50") +
  facet_wrap(~Alt) +
  scale_fill_viridis(option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) )

(plot_n_alts_omr_inflow <- ggplot(bins_summary_months ) +
  geom_tile(aes(OMR, Alt, fill = n), color = "gray50") +
  facet_wrap(~Flow) +
  scale_fill_viridis(option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) ))

(plot_n_alts_omr_inflow_noexp <-ggplot(bins_summary_months %>% filter(!Alt %in% c("EXP1", "EXP3"))) +
  geom_tile(aes(OMR, Alt, fill = n), color = "gray50") +
  facet_wrap(~Flow) +
  scale_fill_viridis(option = "plasma") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90) ))

# Write plot
png("figures/plot_n_alts_omr_inflow.png", width = 9, height = 8, units = "in", res = 300, pointsize = 12)
plot_n_alts_omr_inflow
dev.off()

png("figures/plot_n_alts_omr_inflow_noexp.png", width = 9, height = 8, units = "in", res = 300, pointsize = 12)
plot_n_alts_omr_inflow_noexp
dev.off()

# Write tables
write_csv(bins_months_table, "data_export/bin_samplesizes_acrossalts.csv")
write_csv(bins_months_prop_table, "data_export/bin_prop_samplesizes_acrossalts.csv")
