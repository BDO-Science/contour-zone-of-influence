library(ggplot2)
library(dplyr)

############### Quantile Subset ##############
flow_table <- read.csv("flow.subset.table.csv")
flow.dat <- read.csv("data_inflow/Inflow.csv")
flow.months <- flow.dat %>%
  dplyr::filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

###############

Sac.quant <- quantile(flow.months$Sac.cfs, probs = c(0.25, 0.5, 0.75))
Sjr.quant <- quantile(flow.months$SJR.cfs, probs = c(0.25, 0.5, 0.75))

# Subset to multiple groups
Flow.subset <- flow.months %>%
  mutate(A = ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
                      Sac.cfs >=  0.75*Sac.quant[[2]] &
                      SJR.cfs <= 1.25*Sjr.quant[[1]] &
                      SJR.cfs >= 0.75*Sjr.quant[[1]], "A", ""),
         B =  ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
                       Sac.cfs >=  0.75*Sac.quant[[2]] &
                       SJR.cfs <= 1.25*Sjr.quant[[2]] &
                       SJR.cfs >= 0.75*Sjr.quant[[2]], "B", ""),
         C = ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
                      Sac.cfs >=  0.75*Sac.quant[[2]] &
                      SJR.cfs <= 1.25*Sjr.quant[[3]] &
                      SJR.cfs >= 0.75*Sjr.quant[[3]], "C", ""),
         D = ifelse(SJR.cfs <=  1.25*Sjr.quant[[2]] &
                      SJR.cfs >=  0.75*Sjr.quant[[2]] &
                      Sac.cfs <= 1.25*Sac.quant[[1]] &
                      Sac.cfs >= 0.75*Sac.quant[[1]], "D", ""),
         E = ifelse(SJR.cfs <=  1.25*Sjr.quant[[2]] &
                      SJR.cfs >=  0.75*Sjr.quant[[2]] &
                      Sac.cfs <= 1.25*Sac.quant[[3]] &
                      Sac.cfs >= 0.75*Sac.quant[[3]], "E", "")
  )

Flow_vals <- flow.months %>%
  mutate(A = Sac.cfs )

# Make flow values table for Suzanne ---------------------------------------------------
Flowvals = data.frame(Group = c("A", "B", "C", "D", "E"),
                      Description = c("Med SAC Low SJR", "Med SAC Med SJR",
                                      "Med SAC High SJR", "Low SAC Med SJR",
                                      "High SAC Med SJR"),
                      minSAC = c(0.75*Sac.quant[[2]],0.75*Sac.quant[[2]],
                                 0.75*Sac.quant[[2]],0.75*Sac.quant[[1]],
                                 0.75*Sac.quant[[3]]),
                      maxSAC = c(1.25*Sac.quant[[2]],1.25*Sac.quant[[2]],
                                1.25*Sac.quant[[2]],1.25*Sac.quant[[1]],
                                1.25*Sac.quant[[3]]),
                      minSJR = c(0.75*Sjr.quant[[1]],0.75*Sjr.quant[[2]],
                                 0.75*Sjr.quant[[3]],0.75*Sjr.quant[[2]],
                                 0.75*Sjr.quant[[2]]),
                      maxSJR = c(1.25*Sjr.quant[[1]],1.25*Sjr.quant[[2]],
                                 1.25*Sjr.quant[[3]], 1.25*Sjr.quant[[2]],
                                 1.25*Sjr.quant[[2]])) %>%
  mutate(across(c(minSAC:maxSJR), ~round(., 1))) %>%
  mutate("Sac Flow Range (cfs)" = paste0(minSAC, "-", maxSAC),
         "SJR Flow Range (cfs)" = paste0(minSJR, "-", maxSJR)) %>%
  dplyr::select(Group, Description, `Sac Flow Range (cfs)`, `SJR Flow Range (cfs)`)

# write_csv(Flowvals, "data_export/flow_subset_values.csv")

# Remake the median plot
(plot_allvals <- ggplot() +
  geom_vline(xintercept = Sac.quant[[2]], linetype = "dashed")+
  geom_hline(yintercept = Sjr.quant[[2]], linetype = "dashed")+
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

