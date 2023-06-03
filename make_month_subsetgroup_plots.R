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




ggplot2::ggplot(flow_table)+ geom_point(aes(Sac.cfs, SJR.cfs, color = factor(Month)), size = 3) +
  theme_bw()

A <- filter(Flow.subset, A == "A")
ggplot2::ggplot(A)+ geom_point(aes(Sac.cfs, SJR.cfs, color = factor(Month)), size = 3) +
  theme_bw()

B <- filter(Flow.subset, B == "B")
ggplot2::ggplot(B)+ geom_point(aes(Sac.cfs, SJR.cfs, color = factor(Month)), size = 3) +
  theme_bw()

C <- filter(Flow.subset, C == "C")
ggplot2::ggplot(C)+ geom_point(aes(Sac.cfs, SJR.cfs, color = factor(Month)), size = 3) +
  theme_bw()

D <- filter(Flow.subset, D == "D")
ggplot2::ggplot(D)+ geom_point(aes(Sac.cfs, SJR.cfs, color = factor(Month)), size = 3) +
  theme_bw()

E <- filter(Flow.subset, E == "E")
ggplot2::ggplot(E)+ geom_point(aes(Sac.cfs, SJR.cfs, color = factor(Month)), size = 3) +
  theme_bw()


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

