library(ggplot2)
library(dplyr)
library(readr)

# Read data --------------------------------------------
flow_table <- read_csv("data_export/flow_omr_subsets_month_year_dectojun.csv")%>%
  mutate(Month = factor(Month, levels = c("12", "1", "2", "3", "4", "5", "6")),
         Sub.group = factor(Sub.group, levels = inflow_order))

feb <- flow_table %>% filter(Month == "2")
mar <- flow_table %>% filter(Month == "3")

# Count data --------------------------------------------
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")
flow_table_summary <- flow_table %>%
  group_by(Sub.group, Month) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Month = factor(Month, levels = c("12", "1", "2", "3", "4", "5", "6")),
         Sub.group = factor(Sub.group, levels = inflow_order))

flow_OMR_table_summary <- flow_table %>%
  group_by(Sub.group, Month, OMR) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Month = factor(Month, levels = c("12", "1", "2", "3", "4", "5", "6")),
         Sub.group = factor(Sub.group, levels = inflow_order))

# Plot data ------------------------------------------------
# Make plots of month

png(filename = "figures/month_inflowbin_samplesizes_NAA.png")
ggplot(flow_table_summary) + geom_tile(aes(x = Sub.group, y = Month, fill = n), color = "black") +
  theme_classic()
dev.off()

ggplot(feb) + geom_point(aes(x = Sac.cfs, y = OMR, color = OMR_group))

png(filename = "figures/month_missingOMR_Sacflow_NAA.png", units = "in", width= 8, height = 7, res = 300)
ggplot(flow_table) + geom_point(aes(x = Sac.cfs, y = OMR, color = OMR_group), size = 2) + facet_wrap(~Month) +
  scale_color_viridis_d() + theme_bw()
dev.off()

png(filename = "figures/month_missingOMR_Sjrflow_NAA.png", units = "in", width= 8, height = 7, res = 300)
ggplot(flow_table %>% filter(SJR.cfs < 20000)) + geom_point(aes(x = SJR.cfs, y = OMR, color = OMR_group), size = 2) + facet_wrap(~Month) +
  scale_color_viridis_d() + theme_bw()
dev.off()


ggplot(flow_table) +
  geom_point(aes(x = Sac.cfs, y = SJR.cfs, color = OMR_group, shape =Sub.group), size = 3) +
  facet_wrap(~Month) +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 12, 13, 14, 15, 16)) +
  scale_color_viridis_d() +
  theme_bw()


ggplot(flow_table) + geom_boxplot(aes(x = Month, y = Sac.cfs, fill = Month))
ggplot(flow_table) + geom_boxplot(aes(x = Month, y = SJR.cfs, fill = Month))















# flow.dat <- read.csv("data_inflow/Inflow.csv")
# flow.months <- flow.dat %>%
  # dplyr::filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

###############

Sac.quant <- quantile(flow.months$Sac.cfs, probs = c(0.25, 0.5, 0.75))
Sjr.quant <- quantile(flow.months$SJR.cfs, probs = c(0.25, 0.5, 0.75))

SacL = Sac.quant[[1]]
SacM = Sac.quant[[2]]
SacH = Sac.quant[[3]]
SjrL = Sjr.quant[[1]]
SjrM = Sjr.quant[[2]]
SjrH = Sjr.quant[[3]]


# Make flow values table for Suzanne ---------------------------------------------------
Flowvals = data.frame(Group = c("A", "B", "C", "D", "E", "F", "G"),
                      Description = c("Med SAC Low SJR", "Med SAC Med SJR",
                                      "Med SAC High SJR", "Low SAC Med SJR",
                                      "High SAC Med SJR", "Low SAC Low SJR", "High SAC High SJR"),
                      minSAC = c(0.75*SacM,0.75*SacM,
                                 0.75*SacM,0.75*SacL,
                                 0.75*SacH, 0.75*SacL, 0.75*SacH),
                      maxSAC = c(1.25*SacM,1.25*SacM,
                                1.25*SacM,1.25*SacL,
                                1.25*SacH, 1.25*SacL, 1.25*SacH),
                      minSJR = c(0.75*SjrL,0.75*SjrM,
                                 0.75*SjrH,0.75*SjrM,
                                 0.75*SjrM, 0.75*SjrL, 0.75*SjrH),
                      maxSJR = c(1.25*SjrL,1.25*SjrM,
                                 1.25*SjrH, 1.25*SjrM,
                                 1.25*SjrM, 1.25*SjrL, 1.25*SjrH)) %>%
  mutate(across(c(minSAC:maxSJR), ~round(., 1))) %>%
  mutate("Sac Flow Range (cfs)" = paste0(minSAC, "-", maxSAC),
         "SJR Flow Range (cfs)" = paste0(minSJR, "-", maxSJR)) %>%
  dplyr::select(Group, Description, `Sac Flow Range (cfs)`, `SJR Flow Range (cfs)`)

# write_csv(Flowvals, "data_export/flow_subset_values.csv")

# Remake the median plot
(plot_allvals <- ggplot() +
  geom_vline(xintercept = SacM, linetype = "dashed")+
  geom_hline(yintercept = SjrM, linetype = "dashed")+
  geom_point(data = flow.months, aes(x=Sac.cfs, y=SJR.cfs), shape = 15, size = 2, color = "gray80") +
  geom_point(data = flow_table %>%filter(OMR>-6500), aes(x = Sac.cfs, y = SJR.cfs, color = Sub.group, shape = factor(OMR)), size = 4) +
    scale_shape_manual(values = c(21, 22, 23)) +
  scale_color_viridis_d(option = "turbo")+
  labs(x = "Sac Flow (cfs)", y = "SJR Flow (cfs)", color = "Inflow Group", shape = "OMR Flow (cfs)") +
  theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = "top"))

png("figures/allflowvals_groups_plot.png", width = 8, height = 5, units = "in", res = 300, pointsize = 12)
plot_allvals
dev.off()



# These are plots I sent to Suzanne ---------------------------------------

flow_table1 <- flow_table %>%
  mutate(month_group = if_else(Month %in% c(4, 5, 6), "apr-jun", "dec-mar"),
         month_group = factor(month_group, levels = c("dec-mar", "apr-jun")))

(plot1 <- ggplot2::ggplot(flow_table1)+
    geom_point(aes(Sac.cfs, SJR.cfs, color = Sub.group, shape = factor(Month)), size = 3) +
    labs(title = "OMR all")  +
    scale_color_viridis_d(option = "turbo")+
    facet_wrap(~month_group, nrow = 2) +
    theme_bw())


png("figures/months_groups_overlap_plot.png", width = 7, height = 9, units = "in", res = 300, pointsize = 9)
plot1
dev.off()



# only -5000
flow_table2 <- flow_table %>%
  filter(OMR == -5000) %>%
  mutate(month_group = if_else(Month %in% c(4, 5, 6), "apr-jun", "dec-mar"),
         month_group = factor(month_group, levels = c("dec-mar", "apr-jun")))

(plot2 <- ggplot2::ggplot(flow_table2)+
  geom_point(aes(Sac.cfs, SJR.cfs, color = Sub.group, shape = factor(Month)), size = 3) +
  labs(title = "OMR -5000")  +
  scale_color_viridis_d(option = "turbo")+
  facet_wrap(~month_group, nrow = 2) +
  theme_bw())


png("figures/months_groups_overlap_plot_5000only.png", width = 7, height = 9, units = "in", res = 300, pointsize = 9)
plot2
dev.off()

