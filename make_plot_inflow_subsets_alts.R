##############################################
# Last updated 9/15/2023 by Catarina Pien (USBR)
# Most up to date subsets
# Creates a plot and two versions of the table to describe range of inflow values
#################################################

library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(viridis)
library(tidyr)

# Read dat -------------------------------
# flow.dat <- read.csv("data_inflow/Inflow.csv") # This file will change based on new Calsim results
flow.dat0 <- read_excel("data_inflow/Reclamation_2021LTO_CalSim3_SacFreeport_SanJoaquinVernalis_rev01_20230913__NAA_090723.xlsx", skip = 6)
colnames(flow.dat0) = c("Row", "Date", "Sac.cfs", "SJR.cfs")
flow.dat1 <- flow.dat0 %>%
  select(-Row) %>%
  mutate(Month = month(Date),
         Year = year(Date))
# read omr data
omr.dat <- read_excel("data_inflow/Reclamation_2021LTO_CalSim3_OMR_rev01_20230913__NAA_090723.xlsx", skip = 6)
colnames(omr.dat) = c("Row", "Date", "OMR")
# read export data
export.dat <- read_excel("data_inflow/Reclamation_2021LTO_CalSim3_DeltaExports_rev01_20230926__NAA_090723.xlsx", skip = 6)
colnames(export.dat) = c("Row", "Date", "Exports")
# join all together and define OMR groupings
flow.dat <- left_join(flow.dat1, omr.dat) %>%
  left_join(export.dat) %>%
  select(-Row) %>%
  mutate(OMR_group = case_when(OMR >= -2500 & OMR <=-1500 ~ "-2000",
                               OMR >= -4000 & OMR <=-3000 ~ "-3500",
                               OMR >= -5500 & OMR <=-4500 ~ "-5000",
                               OMR <=-5500 ~ "less than -5500",
                               TRUE ~ "Other"))

# Sample sizes and summaries ----------------------
(flow.dat.summary <- flow.dat %>%
  group_by(OMR_group) %>%
  summarize(n =n()))

# Filter down to Dec-June
flow.months <- flow.dat %>%
  dplyr::filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

# Add inflow bins -----------------------------------------------

Sac.quant <- quantile(flow.months$Sac.cfs, probs = c(0, 0.333, 0.666, 1))
Sjr.quant <- quantile(flow.months$SJR.cfs, probs = c(0, 0.333, 0.666, 1))

# Rename values to make it easier to track
Sac0 = Sac.quant[[1]]
SacL = Sac.quant[[2]]
SacM = Sac.quant[[3]]
SacH = Sac.quant[[4]]
SacMed = median(flow.months$Sac.cfs)
SjrMed = median(flow.months$SJR.cfs)
Sjr0 = Sjr.quant[[1]]
SjrL = Sjr.quant[[2]]
SjrM = Sjr.quant[[3]]
SjrH = Sjr.quant[[4]]

# Create groupings based on values above
Flow.subsetall  <- flow.months %>%
  mutate(lolo = if_else(Sac.cfs < SacL & Sac.cfs >=Sac0 & SJR.cfs <  SjrL & SJR.cfs >=  Sjr0, "lolo", ""),
         medmed = if_else(Sac.cfs < SacM & Sac.cfs >=SacL & SJR.cfs <  SjrM & SJR.cfs >=  SjrL, "medmed", ""),
         hihi = if_else(Sac.cfs <= SacH & Sac.cfs >=SacM & SJR.cfs <=  SjrH & SJR.cfs >=  SjrM, "hihi", ""),
         himed = if_else(Sac.cfs <= SacH & Sac.cfs >=SacM & SJR.cfs <  SjrM & SJR.cfs >=  SjrL, "himed", ""),
         medhi = if_else(Sac.cfs < SacM & Sac.cfs >=SacL & SJR.cfs <=  SjrH & SJR.cfs >=  SjrM, "medhi", ""),
         lohi = if_else(Sac.cfs < SacL & Sac.cfs >=Sac0 & SJR.cfs <=  SjrH & SJR.cfs >=  SjrM, "lohi", ""),
         hilo = if_else(Sac.cfs <= SacH & Sac.cfs >=SacM & SJR.cfs <  SjrL & SJR.cfs >=  Sjr0, "hilo", ""),
         lomed = if_else(Sac.cfs < SacL & Sac.cfs >=Sac0 & SJR.cfs <  SjrM & SJR.cfs >=  SjrL, "lomed", ""),
         medlo = if_else(Sac.cfs < SacM& Sac.cfs >=SacL & SJR.cfs <  SjrL & SJR.cfs >=  Sjr0, "medlo", "")) %>%
  mutate(Sub.group = paste0(lolo, medmed, hihi, himed, medhi, lohi, hilo,lomed, medlo))

# Remove extra variables
Flow.subset.clean <- Flow.subsetall %>%
  filter(Sub.group != "") %>%
  filter(OMR_group!="Other") %>%
  select(-lolo, -medmed, -hihi, -himed, -medhi, -lohi, -hilo, -lomed, -medlo)

# Summarize sample sizes -----------------------------------------------------
Flow.subset.sumstats <- Flow.subsetall %>%
  group_by(Sub.group) %>%
  summarize(meanOMR = round(mean(OMR),0),
            meanExports = round(mean(Exports),0),
            n = n()) %>%
  rename(Group = Sub.group)

# sample sizes for inflow groups
Flow.subset.summary <- Flow.subset.clean %>%
  group_by(Sub.group) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(n) %>%
  rename(`Inflow Group` = Sub.group)

# sample sizes for inflow-omr groups
Flow.OMR.subset.summary <- Flow.subset.clean %>%
  group_by(Sub.group, OMR_group) %>%
  summarize(n = n(),
            meanOMR = round(mean(OMR), 0),
            meanExports = round(mean(Exports),0)) %>%
  ungroup() %>%
  mutate(Include = if_else(n<5, "N", "Y"),
         OMR_Range = if_else(OMR_group == "-2000", "-2500 to -1500 cfs",
                             if_else(OMR_group == "-3500", "-4000 to -3000 cfs",
                             if_else(OMR_group == "-5000", "-5500 to -4500 cfs",
                            if_else(OMR_group == "less than -5500", "less than -5500", ""))))) %>%
  arrange(Sub.group, OMR_group) %>%
  rename(`Inflow Group` = Sub.group) %>%
  select(`Inflow Group`,
         `OMR Group` = OMR_group,
         OMR_Range,
         `Mean OMR (cfs)` = meanOMR,
         ` Mean Exports (cfs)` = meanExports,
         n)

Flow.OMR.complete <- Flow.OMR.subset.summary %>%
  complete(`Inflow Group`,
           nesting(`OMR Group`, `OMR_Range`),
           fill = list(n = 0))

# Write file
# write_csv(Flow.subset.clean, "data_export/flow_subsets_month_year_NAA.csv")
# write_csv(Flow.subset.summary, "data_export/flow_subsets_samplesize_NAA_equalspaced.csv")
write_csv(Flow.OMR.complete, "data_export/flow_omr_samplesizes.csv")


# Plot of OMR and Exports
ggplot(Flow.subset.clean) +
  geom_point(aes(OMR, Exports, color = Sub.group), size = 3)

# Look at stormflex
Stormflex.data <- Flow.subsetall %>%
  filter(OMR_group == "less than -5500") %>%
  select(-lolo, -medmed, -hihi, -himed, -medhi, -lohi, -hilo, -lomed, -medlo)

ggplot(Stormflex.data) + geom_point(aes(Sac.cfs, SJR.cfs, color = Sub.group), size = 5) + theme_bw()



# Plot of inflow and OMR groupings ---------------------------------------------
(plot_allvals <- ggplot() +
    geom_point(data = flow.months, aes(x=Sac.cfs, y=SJR.cfs), shape = 15, size = 2, color = "gray80") +
    geom_point(data = Flow.subset.clean , aes(x = Sac.cfs, y = SJR.cfs, color = Sub.group, shape = OMR_group), size = 4) +
    geom_vline(xintercept = SacMed, linetype = "dashed")+
    geom_hline(yintercept = SjrMed, linetype = "dashed")+
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    scale_color_viridis_d(option = "turbo")+
    labs(x = "Sacramento Inflow (cfs)", y = "San Joaquin Inflow (cfs)", color = "Inflow Group", shape = "OMR") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = "top",
          legend.box = "vertical"))

(plot_zoom <- ggplot() +
    geom_point(data = flow.months, aes(x=Sac.cfs, y=SJR.cfs), shape = 15, size = 2, color = "gray80") +
    geom_point(data = Flow.subset.clean , aes(x = Sac.cfs, y = SJR.cfs, color = Sub.group, shape = OMR_group), size = 4) +
    geom_vline(xintercept = SacMed, linetype = "dashed")+
    geom_hline(yintercept = SjrMed, linetype = "dashed")+
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    scale_color_viridis_d(option = "turbo")+
    ylim(800,5000) +
    xlim(5500, 26000) +
    labs(x = "Sacramento Inflow (cfs)", y = "San Joaquin Inflow (cfs)", color = "Inflow Group", shape = "OMR") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = "none",
          legend.box = "vertical"))

# Combine zoom and main plot
library(patchwork)
(plot <- plot_allvals + inset_element(plot_zoom, left = 0.02, bottom = 0.55, right = 0.6, top = 0.97))

# Write plot
png("figures/allflowvals_groups_NAA_OMR_plot.png", width = 9, height = 7, units = "in", res = 300, pointsize = 12)
plot
dev.off()


# Clean table for summary statistics --------------------------------------------------------
# This one is currently in the appendix
Flowvals_unfiltered = data.frame(Group = c("lolo", "medmed", "hihi", "himed", "medhi", "lomed", "medlo", "lohi", "hilo"),
                           Description = c("Low SR Low SJR", "Med SR Med SJR", "High SR High SJR",
                                           "High SR Med SJR", "Med SR High SJR",
                                           "Low SR Med SJR", "Med SR Low SJR", "Low SR High SJR", "High SR Low SJR"),
                           minSAC = c(Sac0, SacL, SacM, SacM, SacL, Sac0, SacL, Sac0, SacM),
                           maxSAC = c(SacL, SacM, SacH, SacH, SacM, SacL, SacM, SacL, SacH),
                           minSJR = c(Sjr0, SjrL, SjrM, SjrL, SjrM, SjrL, Sjr0, SjrM, Sjr0),
                           maxSJR = c(SjrL, SjrM, SjrH, SjrM, SjrH, SjrM, SjrL, SjrH, SjrL)) %>%
  mutate(across(c(minSAC:maxSJR), ~round(., 0))) %>%
  mutate("SR Flow Range (cfs)" = paste0(minSAC, " - ", maxSAC),
         "SJR Flow Range (cfs)" = paste0(minSJR, " - ", maxSJR)) %>%
  dplyr::select(Group, Description, `SR Flow Range (cfs)`, `SJR Flow Range (cfs)`) %>%
  left_join(Flow.subset.sumstats %>% select(Group, meanOMR, meanExports, n)) %>%
  select(`Inflow Group` = Group,
         Description,
         `SR Flow Range (cfs)`,
         `SJR Flow Range (cfs)`,
         `Mean OMR (cfs)` = meanOMR,
         `Mean Exports (cfs)` = meanExports,
         n)

# Clean table describing ranges for filtering data in code-------------------------------------
Flowvals_program = data.frame(Group = c("lolo", "medmed", "hihi", "himed", "medhi", "lomed", "medlo", "lohi", "hilo"),
                              Description = c("Low SR Low SJR", "Med SR Med SJR", "High SR High SJR",
                                              "High SR Med SJR", "Med SR High SJR",
                                              "Low SR Med SJR", "Med SR Low SJR", "Low SR High SJR", "High SR Low SJR"),
                              minSAC = c(Sac0, SacL, SacM, SacM, SacL, Sac0, SacL, Sac0, SacM),
                              maxSAC = c(SacL, SacM, SacH, SacH, SacM, SacL, SacM, SacL, SacH),
                              minSJR = c(Sjr0, SjrL, SjrM, SjrL, SjrM, SjrL, Sjr0, SjrM, Sjr0),
                              maxSJR = c(SjrL, SjrM, SjrH, SjrM, SjrH, SjrM, SjrL, SjrH, SjrL)) %>%
  mutate(across(c(minSAC:maxSJR), ~round(., 1))) %>%
  dplyr::select(Group, Description,minSAC, maxSAC, minSJR, maxSJR)

# Write tables
write_csv(Flowvals_unfiltered, "data_export/flow_subset_table_sumstats.csv")
