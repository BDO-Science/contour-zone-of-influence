##############################################
# Last updated 8/30/2023
# Most up to date subsets
# Creates a plot and two versions of the table to describe range of inflow values
#################################################

library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)

# Read dat -------------------------------
# flow.dat <- read.csv("data_inflow/Inflow.csv") # This file will change based on new Calsim results
flow.dat0 <- read_excel("data_inflow/Reclamation_2021LTO_CalSim3_SacFreeport_SanJoaquinVernalis_rev01_20230913__NAA_090723.xlsx", skip = 6)
colnames(flow.dat0) = c("Row", "Date", "Sac.cfs", "SJR.cfs")
flow.dat <- flow.dat0 %>%
  select(-Row) %>%
  mutate(Month = month(Date),
         Year = year(Date))


flow.months <- flow.dat %>%
  dplyr::filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

# New bins -----------------------------------------------

# consider if we want to change this to bounds at 0 and 1?
Sac.quant <- quantile(flow.months$Sac.cfs, probs = c(0, 0.333, 0.666, 1))
Sjr.quant <- quantile(flow.months$SJR.cfs, probs = c(0, 0.333, 0.666, 1))
# Sac.quant <- quantile(flow.months$Sac.cfs, probs = c(0, 0.25, 0.75, 1))
# Sjr.quant <- quantile(flow.months$SJR.cfs, probs = c(0, 0.25, 0.75, 1))

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

# Create groupings based on values above -------------------------------------
Flow.subset <- flow.months %>%
  mutate(lolo = if_else(Sac.cfs < SacL & Sac.cfs >=Sac0 & SJR.cfs <  SjrL & SJR.cfs >=  Sjr0, "lolo", ""),
         medmed = if_else(Sac.cfs < SacM & Sac.cfs >=SacL & SJR.cfs <  SjrM & SJR.cfs >=  SjrL, "medmed", ""),
         hihi = if_else(Sac.cfs <= SacH & Sac.cfs >=SacM & SJR.cfs <=  SjrH & SJR.cfs >=  SjrM, "hihi", ""),
         himed = if_else(Sac.cfs <= SacH & Sac.cfs >=SacM & SJR.cfs <  SjrM & SJR.cfs >=  SjrL, "himed", ""),
         medhi = if_else(Sac.cfs < SacM & Sac.cfs >=SacL & SJR.cfs <=  SjrH & SJR.cfs >=  SjrM, "medhi", ""),
         lohi = if_else(Sac.cfs < SacL & Sac.cfs >=Sac0 & SJR.cfs <=  SjrH & SJR.cfs >=  SjrM, "lohi", ""),
         hilo = if_else(Sac.cfs <= SacH & Sac.cfs >=SacM & SJR.cfs <  SjrL & SJR.cfs >=  Sjr0, "hilo", ""),
         lomed = if_else(Sac.cfs < SacL & Sac.cfs >=Sac0 & SJR.cfs <  SjrM & SJR.cfs >=  SjrL, "lomed", ""),
         medlo = if_else(Sac.cfs < SacM& Sac.cfs >=SacL & SJR.cfs <  SjrL & SJR.cfs >=  Sjr0, "medlo", ""))%>%
  mutate(Sub.group = paste0(lolo, medmed, hihi, himed, medhi, lohi, hilo,lomed, medlo)) %>%
  filter(Sub.group != "")

# Remove extra variables
Flow.subset.clean <- Flow.subset %>%
  select(-lolo, -medmed, -hihi, -himed, -medhi, -lohi, -hilo, -lomed, -medlo)

# Summarize sample size
Flow.subset.summary <- Flow.subset %>%
  group_by(Sub.group) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Include = if_else(Sub.group %in% c("hilo", "lohi"), "N", "Y")) %>%
  arrange(n) %>%
  rename(Group = Sub.group)

Flow.OMR.subset.summary <- Flow.subset %>%
  group_by(Sub.group, OMR) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Include = if_else(Sub.group %in% c("hilo", "lohi"), "N", "Y")) %>%
  arrange(n) %>%
  rename(Group = Sub.group)

# Write file
write_csv(Flow.subset.clean, "data_export/flow_subsets_month_year_NAA.csv")
write_csv(Flow.subset.summary, "data_export/flow_subsets_samplesize_NAA_equalspaced.csv")
write_csv(Flow.OMR.subset.summary, "data_export/flow_omr_subsets_samplesize_for_alts_equalspaced.csv")

# Remake the median plot ---------------------------------------------
(plot_allvals <- ggplot() +
    geom_point(data = flow.months, aes(x=Sac.cfs, y=SJR.cfs), shape = 15, size = 2, color = "gray80") +
    geom_point(data = Flow.subset.clean , aes(x = Sac.cfs, y = SJR.cfs, color = Sub.group), size = 4) +
    geom_vline(xintercept = SacMed, linetype = "dashed")+
    geom_hline(yintercept = SjrMed, linetype = "dashed")+
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_color_viridis_d(option = "turbo")+
    labs(x = "Sac Flow (cfs)", y = "SJR Flow (cfs)", color = "Inflow Group") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = "top",
          legend.box = "vertical"))

# Write plot
png("figures/allflowvals_groups_NAA_equalspaced_plot.png", width = 8, height = 5, units = "in", res = 300, pointsize = 12)
plot_allvals
dev.off()

# Clean table describing ranges for a word document ----------------------------------------
Flowvals_word = data.frame(Group = c("lolo", "medmed", "hihi", "himed", "medhi", "lomed", "medlo", "lohi", "hilo"),
                      Description = c("Low SAC Low SJR", "Med SAC Med SJR", "High SAC High SJR",
                                      "High SAC Med SJR", "Med SAC High SJR",
                                      "Low SAC Med SJR", "Med SAC Low SJR", "Low SAC High SJR", "High SAC Low SJR"),
                      minSAC = c(Sac0, SacL, SacM, SacM, SacL, Sac0, SacL, Sac0, SacM),
                      maxSAC = c(SacL, SacM, SacH, SacH, SacM, SacL, SacM, SacL, SacH),
                      minSJR = c(Sjr0, SjrL, SjrM, SjrL, SjrM, SjrL, Sjr0, SjrM, Sjr0),
                      maxSJR = c(SjrL, SjrM, SjrH, SjrM, SjrH, SjrM, SjrL, SjrH, SjrL)) %>%
  mutate(across(c(minSAC:maxSJR), ~round(., 1))) %>%
  mutate("Sac Flow Range (cfs)" = paste0(minSAC, "-", maxSAC),
         "SJR Flow Range (cfs)" = paste0(minSJR, "-", maxSJR)) %>%
  dplyr::select(Group, Description, `Sac Flow Range (cfs)`, `SJR Flow Range (cfs)`) %>%
  left_join(Flow.subset.summary %>% select(Group, n))

# Clean table describing ranges for filtering data in code-------------------------------------
Flowvals_program = data.frame(Group = c("lolo", "medmed", "hihi", "himed", "medhi", "lomed", "medlo", "lohi", "hilo"),
                              Description = c("Low SAC Low SJR", "Med SAC Med SJR", "High SAC High SJR",
                                              "High SAC Med SJR", "Med SAC High SJR",
                                              "Low SAC Med SJR", "Med SAC Low SJR", "Low SAC High SJR", "High SAC Low SJR"),
                              minSAC = c(Sac0, SacL, SacM, SacM, SacL, Sac0, SacL, Sac0, SacM),
                              maxSAC = c(SacL, SacM, SacH, SacH, SacM, SacL, SacM, SacL, SacH),
                              minSJR = c(Sjr0, SjrL, SjrM, SjrL, SjrM, SjrL, Sjr0, SjrM, Sjr0),
                              maxSJR = c(SjrL, SjrM, SjrH, SjrM, SjrH, SjrM, SjrL, SjrH, SjrL)) %>%
  mutate(across(c(minSAC:maxSJR), ~round(., 1))) %>%
  # mutate("Sac Flow Range (cfs)" = paste0(minSAC, "-", maxSAC),
         # "SJR Flow Range (cfs)" = paste0(minSJR, "-", maxSJR)) %>%
  dplyr::select(Group, Description,minSAC, maxSAC, minSJR, maxSJR)

# Write tables
write_csv(Flowvals_word, "data_export/flow_subset_table_NAA_for_alts.csv")
# write_csv(Flowvals_program, "data_export/flow_subset_table_forsteve_for_alts.csv")
