rm(list=ls())

#library("epanetReader")
library("car")
library("dplyr")
##################################################
#ZOI.dat <- read.inp("hydro_echo_NAA_20220624.inp")
#str(ZOI.dat)



##################################################
library("RColorBrewer")
display.brewer.pal(4, "Spectral")
brewer.pal(4, "Spectral")

flow.dat <- read.csv("data_inflow/Inflow.csv")
flow.dat <- transform(flow.dat, bg.col = ifelse(OMR < -6000, "#D7191C95", ifelse(OMR < -4900 & OMR >-6000, "#FDAE6195", ifelse(OMR > -2200, "#2B83BA95", "#ABDDA495"))))
flow.dat <- transform(flow.dat, bg.col = as.character(bg.col))

flow.dat[1:50,]
flow.dat[400:500,]

head(flow.dat)
unique(flow.dat$OMR)

############### Quantile Subset ##############

flow.dat <- read.csv("data_inflow/Inflow.csv")
flow.months <- flow.dat %>%
  dplyr::filter(Month %in% c(12, 1, 2, 3, 4, 5, 6))

###############

Sac.quant <- quantile(flow.months$Sac.cfs, probs = c(0.25, 0.5, 0.75))
Sjr.quant <- quantile(flow.months$SJR.cfs, probs = c(0.25, 0.5, 0.75))

SacL = Sac.quant[[1]]
SacM = Sac.quant[[2]]
SacH = Sac.quant[[3]]
SjrL = Sjr.quant[[1]]
SjrM = Sjr.quant[[2]]
SjrH = Sjr.quant[[3]]
# Sac.quant <- quantile(flow.dat$Sac.cfs, probs = c(0.2, 0.5, 0.8))
# Sjr.quant <- quantile(flow.dat$SJR.cfs, probs = c(0.2, 0.5, 0.8))

Flow.subset <- transform(flow.months, Sub.group = ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
                                                        Sac.cfs >=  0.75*Sac.quant[[2]] &
                                                        SJR.cfs <= 1.25*Sjr.quant[[1]] &
                                                        SJR.cfs >= 0.75*Sjr.quant[[1]], "A",
                                                      ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
                                                               Sac.cfs >=  0.75*Sac.quant[[2]] &
                                                               SJR.cfs <= 1.25*Sjr.quant[[2]] &
                                                               SJR.cfs >= 0.75*Sjr.quant[[2]], "B",
                                                             ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
                                                                      Sac.cfs >=  0.75*Sac.quant[[2]] &
                                                                      SJR.cfs <= 1.25*Sjr.quant[[3]] &
                                                                      SJR.cfs >= 0.75*Sjr.quant[[3]], "C",
                                                                    ifelse(SJR.cfs <=  1.25*Sjr.quant[[2]] &
                                                                             SJR.cfs >=  0.75*Sjr.quant[[2]] &
                                                                             Sac.cfs <= 1.25*Sac.quant[[1]] &
                                                                             Sac.cfs >= 0.75*Sac.quant[[1]], "D",
                                                                           ifelse(SJR.cfs <=  1.25*Sjr.quant[[2]] &
                                                                                    SJR.cfs >=  0.75*Sjr.quant[[2]] &
                                                                                    Sac.cfs <= 1.25*Sac.quant[[3]] &
                                                                                    Sac.cfs >= 0.75*Sac.quant[[3]], "E", "F"))))))

Flow.subset <- subset(Flow.subset, Sub.group != "F")
nrow(Flow.subset)



# Subset to multiple groups
Flow.subset <- flow.months %>%
  mutate(A = if_else(Sac.cfs <= 1.25*SacM & Sac.cfs >=0.75*SacM & SJR.cfs <= 1.25* SjrL & SJR.cfs >= 0.75* SjrL, "A", ""),
         B = if_else(Sac.cfs <= 1.25*SacM & Sac.cfs >=0.75*SacM & SJR.cfs <= 1.25* SjrM & SJR.cfs >= 0.75* SjrM, "B", ""),
         C = if_else(Sac.cfs <= 1.25*SacM & Sac.cfs >=0.75*SacM & SJR.cfs <= 1.25* SjrH & SJR.cfs >= 0.75* SjrH, "C", ""),
         D = if_else(Sac.cfs <= 1.25*SacL & Sac.cfs >=0.75*SacL & SJR.cfs <= 1.25* SjrM & SJR.cfs >= 0.75* SjrM, "D", ""),
         E = if_else(Sac.cfs <= 1.25*SacH & Sac.cfs >=0.75*SacH & SJR.cfs <= 1.25* SjrM & SJR.cfs >= 0.75* SjrM, "E", ""),
         F = if_else(Sac.cfs <= 1.25*SacL & Sac.cfs >=0.75*SacL & SJR.cfs <= 1.25* SjrL & SJR.cfs >= 0.75* SjrL, "F", ""),
         G = if_else(Sac.cfs <= 1.25*SacH & Sac.cfs >=0.75*SacH & SJR.cfs <= 1.25* SjrH & SJR.cfs >= 0.75* SjrH, "G", ""))%>%
  mutate(Sub.group = paste0(A, B, C, D, E, F, G)) %>%
  filter(Sub.group != "") %>%
  select(-A, -B, -C, -D, -E, -F, -G) %>%
  arrange(OMR)

# Flow.subset <- flow.months %>%
#   mutate(A = ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
#                       Sac.cfs >=  0.75*Sac.quant[[2]] &
#                       SJR.cfs <= 1.25*Sjr.quant[[1]] &
#                       SJR.cfs >= 0.75*Sjr.quant[[1]], "A", ""),
#           B =  ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
#                         Sac.cfs >=  0.75*Sac.quant[[2]] &
#                         SJR.cfs <= 1.25*Sjr.quant[[2]] &
#                         SJR.cfs >= 0.75*Sjr.quant[[2]], "B", ""),
#           C = ifelse(Sac.cfs <=  1.25*Sac.quant[[2]] &
#                     Sac.cfs >=  0.75*Sac.quant[[2]] &
#                     SJR.cfs <= 1.25*Sjr.quant[[3]] &
#                     SJR.cfs >= 0.75*Sjr.quant[[3]], "C", ""),
#           D = ifelse(SJR.cfs <=  1.25*Sjr.quant[[2]] &
#                            SJR.cfs >=  0.75*Sjr.quant[[2]] &
#                            Sac.cfs <= 1.25*Sac.quant[[1]] &
#                            Sac.cfs >= 0.75*Sac.quant[[1]], "D", ""),
#            E = ifelse(SJR.cfs <=  1.25*Sjr.quant[[2]] &
#                                   SJR.cfs >=  0.75*Sjr.quant[[2]] &
#                                   Sac.cfs <= 1.25*Sac.quant[[3]] &
#                                   Sac.cfs >= 0.75*Sac.quant[[3]], "E", "")
#          ) %>%
#   mutate(Sub.group = paste0(A, B, C, D, E)) %>%
#   filter(Sub.group != "") %>%
#   select(-A, -B, -C, -D, -E) %>%
#   arrange(OMR)

### Summarize counts of OMR across groups ###

Flow.sum <- Flow.subset %>%
  group_by(OMR, Sub.group) %>%
  summarise(Count = length(OMR)) %>%
  as.data.frame(Flow.sum)

Flow.sum
 readr::write_csv(Flow.subset, "flow.subset.table.csv")
# readr::write_csv(Flow.sum, "flow.summary.table.csv")

## Added: plot the subsetted data to check -----------------------------------
library(ggplot2)
library(viridis)

(plot.subsetted <- ggplot(Flow.subset) +
  geom_point(aes(x = Sac.cfs, y = SJR.cfs, color = Sub.group, shape = factor(OMR)), size = 3) +
  geom_vline(xintercept = Sac.quant[[2]], linetype = "dashed") +
  geom_hline(yintercept = Sjr.quant[[2]], linetype = "dashed") +
  labs(x = "Sacramento Inflow (cfs)", y = "SJR Inflow (cfs)") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_bw())

# png(filename = here::here("figures", "plot_subsetteddata_quantiles.png"),
#     width = 7, height = 5, units = "in",
#     pointsize = 12, family = "sans", res = 300)
# plot.subsetted
# dev.off()


# -----------------------------------------------------------------




### Alternatively subset Directly ##
Flow.subset <- subset(flow.dat, Sac.cfs <=  1.2*Sac.quant[[2]] &
                        Sac.cfs >=  0.8*Sac.quant[[2]] &
                        SJR.cfs <= 1.2*Sjr.quant[[1]] &
                        SJR.cfs >= 0.8*Sjr.quant[[1]] |

                        Sac.cfs <=  1.2*Sac.quant[[2]] &
                        Sac.cfs >=  0.8*Sac.quant[[2]] &
                        SJR.cfs <= 1.2*Sjr.quant[[2]] &
                        SJR.cfs >= 0.8*Sjr.quant[[2]] |

                        Sac.cfs <=  1.2*Sac.quant[[2]] &
                        Sac.cfs >=  0.8*Sac.quant[[2]] &
                        SJR.cfs <= 1.2*Sjr.quant[[3]] &
                        SJR.cfs >= 0.8*Sjr.quant[[3]] |

                        SJR.cfs <=  1.2*Sjr.quant[[2]] &
                        SJR.cfs >=  0.8*Sjr.quant[[2]] &
                        Sac.cfs <= 1.2*Sac.quant[[1]] &
                        Sac.cfs >= 0.8*Sac.quant[[1]] |

                        SJR.cfs <=  1.2*Sjr.quant[[2]] &
                        SJR.cfs >=  0.8*Sjr.quant[[2]] &
                        Sac.cfs <= 1.2*Sac.quant[[3]] &
                        Sac.cfs >= 0.8*Sac.quant[[3]])

nrow(Flow.subset)


########################################################
################## Quantile Plot #######################
########################################################

jpeg(filename = "figures/Quantile_subset.jpg", height=7.5, width = 5.5, units="in", bg="white", res=500)

par(mfrow=c(2,1), oma=c(3.25, 3.75, 0.5, 1), mar=c(0.25, 0.75, 0.25, 0.25), bty="l")

plot(flow.dat$Sac.cfs, flow.dat$SJR.cfs, pch=21, lwd=0.25, bg=flow.dat$bg.col, cex=1.2, xlim=c(5000, 80000), ylim=c(0, 20000), las=1, xlab="", ylab="", axes=FALSE, frame=TRUE)
axis(1, at = c(20000, 40000, 60000, 80000), labels=FALSE)
axis(2, at = c(0, 5000, 10000, 15000, 20000), las=1)

polygon(c(0.8*Sac.quant[[2]], 1.2*Sac.quant[[2]], 1.2*Sac.quant[[2]], 0.8*Sac.quant[[2]]),
        c(0.8*Sjr.quant[[2]], 0.8*Sjr.quant[[2]], 1.2*Sjr.quant[[2]], 1.2*Sjr.quant[[2]]), lty=2, border="navy")

polygon(c(0.8*Sac.quant[[2]], 1.2*Sac.quant[[2]], 1.2*Sac.quant[[2]], 0.8*Sac.quant[[2]]),
        c(0.8*Sjr.quant[[1]], 0.8*Sjr.quant[[1]], 1.2*Sjr.quant[[1]], 1.2*Sjr.quant[[1]]), lty=2, border="navy")

polygon(c(0.8*Sac.quant[[2]], 1.2*Sac.quant[[2]], 1.2*Sac.quant[[2]], 0.8*Sac.quant[[2]]),
        c(0.8*Sjr.quant[[3]], 0.8*Sjr.quant[[3]], 1.2*Sjr.quant[[3]], 1.2*Sjr.quant[[3]]), lty=2, border="navy")


#Sac quantiles
lines(c(Sac.quant[[2]], Sac.quant[[2]]), c(0, 20000), lty=2, lwd=0.8, col="black")
#SJR quantiles
lines(c(0, 90000), c(Sjr.quant[[2]], Sjr.quant[[2]]), lty=2, lwd=0.8, col="black")

points(c(70000, 70000, 70000, 70000), c(20000, 19250, 18500, 17750), pch=21, lwd=0.5, bg=c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA"))
text(c(70000, 70000, 70000, 70000), c(20000, 19250, 18500, 17750), labels=c("-6,500", "-5,000", "-3,500", "-2,000"), pos=4, cex=0.75)

mtext(2, text="San Joaquin Inflow (cfs)", line=3.5)

plot(flow.dat$Sac.cfs, flow.dat$SJR.cfs, pch=21, lwd=0.25, bg=flow.dat$bg.col, cex=1.2, xlim=c(5000, 80000), ylim=c(0, 20000), las=1, xlab="", ylab="", axes=FALSE, frame=TRUE)
axis(1, at = c(20000, 40000, 60000, 80000))
axis(2, at = c(0, 5000, 10000, 15000, 20000), las=1)

polygon(c(0.8*Sac.quant[[2]], 1.2*Sac.quant[[2]], 1.2*Sac.quant[[2]], 0.8*Sac.quant[[2]]),
        c(0.8*Sjr.quant[[2]], 0.8*Sjr.quant[[2]], 1.2*Sjr.quant[[2]], 1.2*Sjr.quant[[2]]), lty=2, border="navy")

polygon(c(0.8*Sac.quant[[1]], 1.2*Sac.quant[[1]], 1.2*Sac.quant[[1]], 0.8*Sac.quant[[1]]),
        c(0.8*Sjr.quant[[2]], 0.8*Sjr.quant[[2]], 1.2*Sjr.quant[[2]], 1.2*Sjr.quant[[2]]), lty=2, border="navy")

polygon(c(0.8*Sac.quant[[3]], 1.2*Sac.quant[[3]], 1.2*Sac.quant[[3]], 0.8*Sac.quant[[3]]),
        c(0.8*Sjr.quant[[2]], 0.8*Sjr.quant[[2]], 1.2*Sjr.quant[[2]], 1.2*Sjr.quant[[2]]), lty=2, border="navy")


#Sac quantiles
lines(c(Sac.quant[[2]], Sac.quant[[2]]), c(0, 20000), lty=2, lwd=0.8, col="black")
#SJR quantiles
lines(c(0, 90000), c(Sjr.quant[[2]], Sjr.quant[[2]]), lty=2, lwd=0.8, col="black")


mtext(1, text="Sacramento Inflow (cfs)", line=2.25)
mtext(2, text="San Joaquin Inflow (cfs)", line=3.5)

dev.off()



############# OMR Frequency ################

flow.sum <- flow.dat %>%
  group_by(OMR) %>%
  summarise(tot.count = length(OMR)) %>%
  as.data.frame(flow.sum)

flow.sum

################# Quantiles and plot ##################

Sac.quant <- quantile(flow.dat$Sac.cfs, probs = c(0.25, 0.5, 0.75))
Sjr.quant <- quantile(flow.dat$SJR.cfs, probs = c(0.25, 0.5, 0.75))


jpeg(filename = "Sac_SJR_Inflow.jpg", heigh=5, width = 7, units="in", bg="white", res=500)

par(mfrow=c(1,1), oma=c(3.25, 3.75, 0.5, 0.5), mar=c(0.25, 0.75, 0.5, 0.25), bty="l")

plot(flow.dat$Sac.cfs, flow.dat$SJR.cfs, pch=21, lwd=0.25, bg=flow.dat$bg.col, cex=1.2, xlim=c(5000, 80000), ylim=c(0, 20000), las=1, xlab="", ylab="")
par(new=TRUE)
dataEllipse(flow.dat$Sac.cfs, flow.dat$SJR.cfs, levels=c(0.25, 0.5, 0.75), center.cex = 0.5, ellipse.label = c(0.25, 0.5, 0.75), plot.points=FALSE, xlim=c(5000, 80000), ylim=c(0, 20000), las=1, xlab="", ylab="", col="black", lwd=0.8)

#Sac quantiles
lines(c(Sac.quant[[1]], Sac.quant[[1]]), c(0, 20000), lty=2, lwd=0.8, col="black")
lines(c(Sac.quant[[2]], Sac.quant[[2]]), c(0, 20000), lty=2, lwd=0.8, col="black")
lines(c(Sac.quant[[3]], Sac.quant[[3]]), c(0, 20000), lty=2, lwd=0.8, col="black")

#SJR quantiles
lines(c(0, 90000), c(Sjr.quant[[1]], Sjr.quant[[1]]), lty=2, lwd=0.8, col="black")
lines(c(0, 90000), c(Sjr.quant[[2]], Sjr.quant[[2]]), lty=2, lwd=0.8, col="black")
lines(c(0, 90000), c(Sjr.quant[[3]], Sjr.quant[[3]]), lty=2, lwd=0.8, col="black")

text(Sac.quant[[1]], Sjr.quant[[1]], labels=expression("25"^"th"), cex=0.75)
text(Sac.quant[[2]], Sjr.quant[[2]], labels=expression("50"^"th"), cex=0.75)
text(Sac.quant[[3]], Sjr.quant[[3]], labels=expression("75"^"th"), cex=0.75)

points(c(75000, 75000, 75000, 75000), c(20000, 19250, 18500, 17750), pch=21, lwd=0.5, bg=c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA"))
text(c(75000, 75000, 75000, 75000), c(20000, 19250, 18500, 17750), labels=c("-6,500", "-5,000", "-3,500", "-2,000"), pos=4, cex=0.75)


mtext(1, text="Sacramento Inflow (cfs)", line=2.25)
mtext(2, text="San Joaquin Inflow (cfs)", line=3.5)

dev.off()

###########################################



################# 50th percentile ###################
flow.sub50 <- subset(flow.dat, Sac.cfs <= 1.1*Sac.quant[[2]] &
                       Sac.cfs >= 0.9*Sac.quant[[2]] &
                       SJR.cfs <= 1.1*Sjr.quant[[2]] &
                       SJR.cfs >= 0.9*Sjr.quant[[2]])

flow.sum50 <- flow.sub50 %>%
  group_by(OMR) %>%
  summarise(count = length(OMR)) %>%
  as.data.frame(flow.sum50)

flow.sum50

########## 25th percentile #############
flow.sub25 <- subset(flow.dat, Sac.cfs <= 1.1*Sac.quant[[1]] &
                       Sac.cfs >= 0.9*Sac.quant[[1]] &
                       SJR.cfs <= 1.1*Sjr.quant[[1]] &
                       SJR.cfs >= 0.9*Sjr.quant[[1]])

flow.sum25 <- flow.sub25 %>%
  group_by(OMR) %>%
  summarise(count = length(OMR)) %>%
  as.data.frame(flow.sum25)

flow.sum25


########## 75th percentile #############
flow.sub75 <- subset(flow.dat, Sac.cfs <= 1.1*Sac.quant[[3]] &
                       Sac.cfs >= 0.9*Sac.quant[[3]] &
                       SJR.cfs <= 1.1*Sjr.quant[[3]] &
                       SJR.cfs >= 0.9*Sjr.quant[[3]])

flow.sum75 <- flow.sub75 %>%
  group_by(OMR) %>%
  summarise(count = length(OMR)) %>%
  as.data.frame(flow.sum75)

flow.sum75


#################################################################
############ iterating across different quantiles ###############
#################################################################



quants <- seq(0.15, 0.85, 0.05)

dat.goods <- data.frame(OMR=NA, count = NA, quant = NA)

for(i in 1:length(quants)){
  SJR.Quant <- quantile(flow.dat$SJR.cfs, probs = quants[[i]])
  Sac.Quant <- quantile(flow.dat$Sac.cfs, probs = quants[[i]])

  flow.sub <- subset(flow.dat, Sac.cfs <= 1.15*Sac.Quant &
                       Sac.cfs >= 0.85*Sac.Quant &
                       SJR.cfs <= 1.15*SJR.Quant &
                       SJR.cfs >= 0.85*SJR.Quant)

  sum.flow <- flow.sub %>%
    group_by(OMR) %>%
    summarise(count = length(OMR)) %>%
    as.data.frame(sum.flow)

  sum.flow$quant <- quants[[i]]
  dat.goods <- rbind(dat.goods, sum.flow)
}

dat.goods <- dat.goods[-1,]
dat.goods

dat.goods <- merge(dat.goods, flow.sum, by="OMR", all.x=TRUE)
dat.goods <- transform(dat.goods, perc.rep = (count/tot.count))
dat.goods <- dat.goods[order(dat.goods$quant),]

dat.goods.csv <- write.csv(dat.goods, "quantile_counts.csv", row.names=FALSE)


####################### Quantile Box #######################


jpeg(filename = "Inflow_subset.jpg", heigh=5, width = 7, units="in", bg="white", res=500)

par(mfrow=c(1,1), oma=c(3.25, 3.75, 0.5, 0.5), mar=c(0.25, 0.75, 0.5, 0.25), bty="l")

plot(flow.dat$Sac.cfs, flow.dat$SJR.cfs, pch=21, lwd=0.25, bg=flow.dat$bg.col, cex=1.2, xlim=c(5000, 80000), ylim=c(0, 20000), las=1, xlab="", ylab="")

polygon(c(0.85*Sac.quant[[2]], 1.15*Sac.quant[[2]], 1.15*Sac.quant[[2]], 0.85*Sac.quant[[2]]),
        c(0.85*Sjr.quant[[2]], 0.85*Sjr.quant[[2]], 1.15*Sjr.quant[[2]], 1.15*Sjr.quant[[2]]), lty=2, border="white")


#Sac quantiles
lines(c(Sac.quant[[2]], Sac.quant[[2]]), c(0, 20000), lty=2, lwd=0.8, col="black")
#SJR quantiles
lines(c(0, 90000), c(Sjr.quant[[2]], Sjr.quant[[2]]), lty=2, lwd=0.8, col="black")
#text(Sac.quant[[2]], Sjr.quant[[2]], labels=expression("50"^"th"), cex=0.75)

points(c(75000, 75000, 75000, 75000), c(20000, 19250, 18500, 17750), pch=21, lwd=0.5, bg=c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA"))
text(c(75000, 75000, 75000, 75000), c(20000, 19250, 18500, 17750), labels=c("-6,500", "-5,000", "-3,500", "-2,000"), pos=4, cex=0.75)


mtext(1, text="Sacramento Inflow (cfs)", line=2.25)
mtext(2, text="San Joaquin Inflow (cfs)", line=3.5)

dev.off()








