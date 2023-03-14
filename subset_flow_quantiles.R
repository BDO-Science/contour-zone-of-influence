############### Quantile Subset ##############

flow.dat <- read.csv("data_inflow/Inflow.csv")


Sac.quant <- quantile(flow.dat$Sac.cfs, probs = c(0.25, 0.5, 0.75))
Sjr.quant <- quantile(flow.dat$SJR.cfs, probs = c(0.25, 0.5, 0.75))

Flow.subset
