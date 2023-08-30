calc_overlap <- function(df, nodeNum, chanNum) {

  # Filter by monthly OMR, node and channel number-----------------
  node <- df %>%
    filter(node == nodeNum & channel_number == chanNum) %>%
    mutate(OMR2 = case_when(monthlyOMR >= -5500 & monthlyOMR <= -4500 ~ -5000,
                            monthlyOMR >= -3500 & monthlyOMR <= -2500 ~ -3000,
                            monthlyOMR >= -2500 & monthlyOMR <= -1500 ~ -2000)) %>%
    dplyr::filter(OMR2 %in% c(-5000, -3000, -2000))

  # Create unique datasets for each monthly OMR bin--------------
  d5000 <- dplyr::filter(node, OMR2 == -5000)
  d3000 <- dplyr::filter(node, OMR2 == -3000)
  d2000 <- dplyr::filter(node, OMR2 == -2000)

  # Calculate overlap for each OMR-----------------
  vel5 <- list(d5000$Velocity_Pumping, d5000$Velocity_No_Pumping)
  out5 <- overlapping::overlap(vel5, nbins = 100, boundaries=c(-3,3), type = "2")

  vel3 <- list(d3000$Velocity_Pumping, d3000$Velocity_No_Pumping)
  out3 <- overlapping::overlap(vel3, nbins = 100, boundaries=c(-3,3),  type = "2")

  vel2 <- list(d2000$Velocity_Pumping, d2000$Velocity_No_Pumping)
  out2 <- overlapping::overlap(vel2, nbins = 100, boundaries=c(-3,3),  type = "2")

  # Plot density distributions------------------------------------
  long <- node %>%
    rename(Vel_NP = Velocity_No_Pumping,
           Vel_P = Velocity_Pumping) %>%
    tidyr::pivot_longer(cols = c("Vel_NP", "Vel_P"), names_to = "Pumping", values_to = "Velocity")

  plot1 <- ggplot(long%>%filter(OMR2==-5000)) +
    geom_density(aes(Velocity, color = Pumping, fill = Pumping), alpha = 0.5) +
    scale_color_manual(values = c("navy", "red"))+
    scale_fill_manual(values = c("navy", "red")) +
    labs(title = paste0("Hourly \nNode:", nodeNum, " OMR:-5000  Overlap:", round(out5$OV,3))) +
    theme_bw()+
    theme(legend.position = c(0.5, 0.8),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))+
    xlim(min(long$Velocity)-0.5, max(long$Velocity)+0.5)

  plot2 <- ggplot(long %>% filter(OMR2==-3000)) +
    geom_density(aes(Velocity, color = Pumping, fill = Pumping), alpha = 0.5) +
    scale_color_manual(values = c("navy", "red"))+
    scale_fill_manual(values = c("navy", "red"))+
    labs(title = paste0("Hourly \nNode:", nodeNum, " OMR:-3000  Overlap:", round(out3$OV,3))) +
    theme_bw()+
    theme(legend.position = c(0.5, 0.8),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))+
    xlim(min(long$Velocity)-0.5, max(long$Velocity)+0.5)

  plot3 <- ggplot(long %>% filter(OMR2==-2000)) +
    geom_density(aes(Velocity, color = Pumping, fill = Pumping), alpha = 0.5) +
    scale_color_manual(values = c("navy", "red"))+
    scale_fill_manual(values = c("navy", "red"))+
    labs(title = paste0("Hourly \nNode:", nodeNum, " OMR:-2000  Overlap:", round(out2$OV,3))) +
    theme_bw()+
    theme(legend.position = c(0.5, 0.8),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))+
    xlim(min(long$Velocity)-0.5, max(long$Velocity)+0.5)

  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/kde_plots/kde_", nodeNum, "_5000_hourly.png"), plot=plot1, height = 6, width = 7, units = "in")
  ggsave(filename=paste0("figures/kde_plots/kde_", nodeNum, "_3000_hourly.png"), plot=plot2, height = 6, width = 7, units = "in")
  ggsave(filename=paste0("figures/kde_plots/kde_", nodeNum, "_2000_hourly.png"), plot=plot3, height = 6, width = 7, units = "in")

  # Create df of overlap values ----------------------
  overlap_df <- data.frame(node = nodeNum, OMR = c(-5000, -3000, -2000), overlap = c(out5$OV, out3$OV, out2$OV))
  print(overlap_df)
  return(overlap_df)

}



calc_overlap_daily <- function(df, nodeNum, chanNum) {

  # Filter by monthly OMR, node and channel number-----------------
  node<- df %>%
    dplyr::filter(node == nodeNum & channel_number == chanNum) %>%
    mutate(OMR2 = case_when(monthlyOMR >= -5500 & monthlyOMR <= -4500 ~ -5000,
                            monthlyOMR >= -3500 & monthlyOMR <= -2500 ~ -3000,
                            monthlyOMR >= -2500 & monthlyOMR <= -1500 ~ -2000))%>%
    dplyr::filter(OMR2 %in% c(-5000, -3000, -2000))

  # Dailify data -----------------------------------------
  daily <- node %>% group_by(date, channel_id, channel_number, node, monthlyOMR, OMR2) %>%
    summarize(Velocity_Pumping = mean(Velocity_Pumping, na.rm = TRUE),
              Velocity_No_Pumping = mean(Velocity_No_Pumping, na.rm = TRUE),
              OMR = mean(OMR, na.rm = TRUE),
              SAC = mean(SAC, na.rm = TRUE),
              SJR = mean(SJR, na.rm = TRUE)) %>%
    ungroup()

  # Create unique datasets for each monthly OMR bin--------------
  d5000 <- dplyr::filter(daily, OMR2 == -5000)
  d3000 <- dplyr::filter(daily, OMR2 == -3000)
  d2000 <- dplyr::filter(daily, OMR2 == -2000)

  # Calculate overlap ---------------------------------------------
  vel5 <- list(d5000$Velocity_Pumping, d5000$Velocity_No_Pumping)
  out5 <- overlapping::overlap(vel5, nbins = 100, boundaries=c(-3,3), type = "2")

  vel3 <- list(d3000$Velocity_Pumping, d3000$Velocity_No_Pumping)
  out3 <- overlapping::overlap(vel3, nbins = 100,boundaries=c(-3,3),  type = "2")

  vel2 <- list(d2000$Velocity_Pumping, d2000$Velocity_No_Pumping)
  out2 <- overlapping::overlap(vel2, nbins = 100, boundaries=c(-3,3),  type = "2")

  # Plot density distributions------------------------------------
  daily_long <- daily %>%
    rename(Vel_NP = Velocity_No_Pumping,
           Vel_P = Velocity_Pumping) %>%
    tidyr::pivot_longer(cols = c("Vel_NP", "Vel_P"), names_to = "Pumping", values_to = "Velocity")

  plot1 <- ggplot(daily_long %>%
                    dplyr::filter(OMR2==-5000)) +
    geom_density(aes(Velocity, color = Pumping, fill = Pumping), alpha = 0.5) +
    scale_color_manual(values = c("navy", "red"))+
    scale_fill_manual(values = c("navy", "red")) +
    labs(title = paste0("Daily \nNode:", nodeNum, " OMR:-5000  Overlap:", round(out5$OV,3))) +
    theme_bw()+
    theme(legend.position = c(0.8, 0.8),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))+
    xlim(min(d5000$Velocity_Pumping)-0.2, max(d5000$Velocity_No_Pumping)+0.2)

  plot2 <- ggplot(daily_long %>% filter(OMR2==-3000)) +
    geom_density(aes(Velocity, color = Pumping, fill = Pumping), alpha = 0.5) +
    scale_color_manual(values = c("navy", "red"))+
    scale_fill_manual(values = c("navy", "red")) +
    labs(title = paste0("Daily \nNode:", nodeNum, " OMR:-3000  Overlap:", round(out3$OV,3))) +
    theme_bw()+
    theme(legend.position = c(0.8, 0.8),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))+
    xlim(min(d5000$Velocity_Pumping)-0.2, max(d5000$Velocity_No_Pumping)+0.2)

  plot3 <- ggplot(daily_long %>% filter(OMR2==-2000)) +
    geom_density(aes(Velocity, color = Pumping, fill = Pumping), alpha = 0.5) +
    scale_color_manual(values = c("navy", "red"))+
    scale_fill_manual(values = c("navy", "red")) +
    labs(title = paste0("Daily \nNode:", nodeNum, " OMR:-2000  Overlap:", round(out2$OV,3))) +
    theme_bw() +
    theme(legend.position = c(0.8, 0.8),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))+
    xlim(min(d5000$Velocity_Pumping)-0.2, max(d5000$Velocity_No_Pumping)+0.2)

  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/kde_plots/kde_", nodeNum, "_5000_daily.png"), plot=plot1, height = 6, width = 7, units = "in")
  ggsave(filename=paste0("figures/kde_plots/kde_", nodeNum, "_3000_daily.png"), plot=plot2, height = 6, width = 7, units = "in")
  ggsave(filename=paste0("figures/kde_plots/kde_", nodeNum, "_2000_daily.png"), plot=plot3, height = 6, width = 7, units = "in")

  # Create df of overlap values ----------------------
  overlap_df <- data.frame(node = nodeNum, OMR = c(-5000, -3000, -2000), overlap = c(out5$OV, out3$OV, out2$OV))
  print(overlap_df)
  return(overlap_df)

}




diff_vel <- function(df, nodeNum, chanNum) {

  # Filter by monthly OMR, node and channel number, calculate Vdiff-----------------
  hourly <- df %>%
    filter(node == nodeNum & channel_number == chanNum) %>%
    mutate(OMR2 = case_when(monthlyOMR >= -5500 & monthlyOMR <= -4500 ~ -5000,
                            monthlyOMR >= -3500 & monthlyOMR <= -2500 ~ -3000,
                            monthlyOMR >= -2500 & monthlyOMR <= -1500 ~ -2000),
           InflowGrp = case_when(monthlySAC >=12453 & monthlySAC <=20905 & monthlySJR >=1273.5 & monthlySJR <=2122.5 ~ "A",
                                 monthlySAC >=12453 & monthlySAC <=20905 & monthlySJR >=1859.5 & monthlySJR <=3098.1 ~ "B",
                                 monthlySAC >=12453 & monthlySAC <=20905 & monthlySJR >=4209.8 & monthlySJR <=7016.2 ~ "C",
                                 monthlySAC >=9715.7 & monthlySAC <=16192.8 & monthlySJR >=1859.5 & monthlySJR <=3098.1 ~ "D",
                                 monthlySAC >=22911.8 & monthlySAC <=38186.2 & monthlySJR >=1859.5 & monthlySJR <=3098.1 ~ "E")) %>%
    dplyr::filter(OMR2 %in% c(-5000, -3000, -2000),
                  InflowGrp %in% c("A", "B", "C", "D", "E")) %>%
    mutate(OMR2 = factor(OMR2, levels = c("-2000", "-3000", "-5000"))) %>%
    mutate(vdiff = Velocity_Pumping-Velocity_No_Pumping,
           timestep = "hourly")

  # Dailify data -----------------------------------------
  daily <- hourly %>% group_by(date, channel_id, channel_number, node, monthlyOMR, InflowGrp, OMR2) %>%
    summarize(Velocity_Pumping = mean(Velocity_Pumping, na.rm = TRUE),
              Velocity_No_Pumping = mean(Velocity_No_Pumping, na.rm = TRUE),
              OMR = mean(OMR, na.rm = TRUE),
              SAC = mean(SAC, na.rm = TRUE),
              SJR = mean(SJR, na.rm = TRUE)) %>%
    ungroup()%>%
    mutate(vdiff = Velocity_Pumping-Velocity_No_Pumping,
           timestep = "daily")

  # Combine data --------------------------------------------
  vel_data <- bind_rows(hourly, daily)
  vel_data_long <- vel_data %>%
    dplyr::select(Velocity_Pumping, Velocity_No_Pumping, InflowGrp, OMR2, timestep) %>%
    rename(Vel_NP = Velocity_No_Pumping,
           Vel_P = Velocity_Pumping) %>%
    tidyr::pivot_longer(cols = c("Vel_NP", "Vel_P"), names_to = "Pumping", values_to = "Velocity")

  # Plot density distributions------------------------------------
  (plot1 <- ggplot(vel_data) +
    geom_boxplot(aes(x = OMR2, y = vdiff, color = timestep, fill = timestep), alpha = 0.5) +
     scale_color_manual(values = c("steelblue4", "indianred")) +
     scale_fill_manual(values = c("steelblue4", "indianred")) +
    labs(title = paste0("Velocity Difference Distribution \nNode:", nodeNum), y = "Velocity Difference (fps)") +
    theme_bw()+
    theme(legend.position = "top",
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12)))

  (plot2 <- ggplot(vel_data_long, aes(x = Velocity, fill = Pumping, alpha = Pumping)) +
      geom_histogram(position = "identity",  binwidth = 0.01) +
      facet_grid(timestep~OMR2, scales = "free_y" ) +
      scale_fill_manual(values = c("steelblue4", "indianred")) +
      scale_alpha_manual(values = c(0.8, 0.4)) +
      labs(title = paste0("Velocity Distributions \nNode:", nodeNum), x = "Velocity (fps)") +
      theme_bw()+
      theme(legend.position = "top",
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 12),
            strip.text = element_text(size = 13)))

  (plot3 <- ggplot(vel_data, aes(x = vdiff, fill = timestep, alpha = timestep)) +
      geom_histogram(position = "identity", binwidth = 0.001) +
      facet_wrap(~OMR2, dir = "v") +
      scale_fill_manual(values = c("steelblue4", "indianred")) +
      scale_alpha_manual(values = c(0.8, 0.4)) +
      labs(title = paste0("Velocity Difference Distribution \nNode:", nodeNum), x = "Velocity Difference (fps)") +
      theme_bw()+
      theme(legend.position = "top",
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 12),
            strip.text = element_text(size = 13)))


  # Save plots ----------------------------------
  ggsave(filename=paste0("figures/vel_plots/boxplots_", nodeNum, ".png"), plot=plot1, height = 6, width = 7, units = "in")
  ggsave(filename=paste0("figures/vel_plots/histogram_vel_", nodeNum, ".png"), plot=plot2, height = 6, width = 7, units = "in")
  ggsave(filename=paste0("figures/vel_plots/histogram_vdiff_", nodeNum, ".png"), plot=plot3, height = 6, width = 7, units = "in")
 # Return data
  return(vel_data)
}
