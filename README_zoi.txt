Zone of Influence README
by: Catarina Pien and Lisa Elliott, Bureau of Reclamation
contact: cpien@usbr.gov, lelliott@usbr.gov
last updated: 11/2/2023

Although this code has been processed successfully on a computer system at the Bureau of Reclamation (Reclamation),
no warranty expressed or implied is made regarding the display or utility of the code for other purposes,
nor on all computer systems, nor shall the act of distribution constitute any such warranty.
Reclamation or the U.S. Government shall not be held liable for improper or incorrect use of the code described and/or contained herein.

Files:

Inputs

1. Inflow group crosswalk input (Calsim 3 data):
	data_raw/Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result.xlsx
2. Proportional overlap data for contour plots (from DSM2 data):
  data_raw/zoi/*
3. Channel lengths (combined here but not used specifically for contours):
  data_raw/DSM2_Version822_Grid_20231102.csv
4. Shapefile of bay delta for interpolation boundary:
  shapefiles/Bay_Delta_Poly_New.shp
5. Exported proportional overlap data from contour plots:
  data_export/prop_overlap_data_long.csv

Code
1. Inflow and OMR groupings based on NAA:
	make_plot_inflow_subsets_NAA.R
2. Make contour plots from proportion overlap data:
	contour_maps_inflow_allalts.R
	functions_zoi.R
3. Channel length analyses and barplot-making code, tables comparing channel length across alts:
	channel_lengths_barplots.R
	functions_zoi.R
4. Original contour map and channel length analyses based on month:
	archive/make_contours_updatedDSM2.R

Files not directly used in LTO:

1. View nodes:
	node_map.Rmd
2. See how months correspond to inflow groups:
	make_month_subsetgroup_plots.R
3. Calculate frequency in OMR and inflow bins for each alternative:
	calculate_frequency_bins_alts.R

Data exports (data_export/):

1. Sample sizes for BA frequency analysis (based on Calsim3; might change):
	bin_samplesizes_acrossalts_freq.csv
	bin_prop_samplesizes_acrossalts_freq.csv
2. Sample sizes for ZOI analysis (based on Calsim3):
	bin_samplesizes_acrossalts_zoi.csv
	bin_prop_samplesizes_acrossalts_zoi.csv
3. Flow bins:
	flow_subset_table_sumstats.csv (details the flow ranges under NAA)
	flow_omr_subsets_month_year_dectojun.csv (flow/omr sample sizes NAA; filters to months of interest)
	flow_omr_subsets_month_year_clean.csv (flow/omr sample sizes NAA; does not filter to month)
	flowbin_samplesizes_acrossalts_zoi.csv (sample sizes across alts; does not filter to OMR bin)
	data_not_included_allalts.csv (data not included based on OMR and Flow binning)
4. Channel length tables across alts (length, comparison with NAA) for BA and EIS:
  medium_hydro_channel_length_BA.csv
  medium_hydro_channel_length_EIS.csv

