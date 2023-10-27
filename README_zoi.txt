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
	contour_maps_inflow_allalts.R
	functions_zoi.R	
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
	calculate_frequency_bins_alts.R

Data exports (data_export/):

1. Sample sizes for BA frequency analysis:
	bin_samplesizes_acrossalts_freq.csv
	bin_prop_samplesizes_acrossalts_freq.csv
2. Sample sizes for ZOI analysis: 
	bin_samplesizes_acrossalts_zoi.csv
	bin_prop_samplesizes_acrossalts_zoi.csv
2. Flow bins: 
	flow_subset_table_sumstats.csv (details the flow ranges under NAA)
	flow_omr_subsets_month_year_dectojun.csv (flow/omr sample sizes NAA; filters to months of interest)
	flow_omr_subsets_month_year_clean.csv (flow/omr sample sizes NAA; does not filter to month)
	
