Zone of Influence README
by: Catarina Pien and Lisa Elliott

Although this code has been processed successfully on a computer system at the Bureau of Reclamation (Reclamation), 
no warranty expressed or implied is made regarding the display or utility of the code for other purposes, 
nor on all computer systems, nor shall the act of distribution constitute any such warranty. 
Reclamation or the U.S. Government shall not be held liable for improper or incorrect use of the code described and/or contained herein.

Files:

1. Inflow group crosswalk: 
	data_raw/Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result.xlsx
2. Inflow and OMR groupings based on NAA and detailed in: 
	make_plot_inflow_subsets_NAA.R
3. Make contour plots from proportion overlap data: 
	contour_maps_inflow.R
4. Channel length analyses and barplot-making code: 
	channel_lengths_barplots.R
5. Original contour map and channel length analyses based on month: 
	make_contours_updatedDSM2.R

Files not directly used in LTO: 

1. View nodes: 
	node_map.Rmd
2. See how months correspond to inflow groups: 
	make_month_subsetgroup_plots.R
3. Calculate frequency in OMR and inflow bins for each alternative:
	Calculate the calculate_frequency_bins_alts.R
