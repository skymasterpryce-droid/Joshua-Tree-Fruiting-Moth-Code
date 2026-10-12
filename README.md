Joshua Tree Fruiting Project code

October 12 2026

Using TARDIS and iNaturalist to determine weather conditions driving flowering and fruiting of Joshua trees
-----
Files-

Protocol_manual- Contains protocol on how to go about identifying flowering and fruiting Joshua Trees on iNaturalist.

Scripts - contain R scripts and data needed to replicate analysis 

R- R scripts in order they should be used
	   1. inat_phenology_download.r
	   2. PRISM_data-management.r
           3. flowering_inat_phenology_data-management.r
           4. flowering_phenology_modeling.r  
           5. fruiting_inat_phenology_data-management.r
           6. fruiting_phenology_modeling.r  
           7. Combined_flowering_Fruiting_modeling.r
           8. validation_records_analysis
   
  Data- 
		YUCCA- contains Joshua tree range map from Esque et al. 2023 Unprecedented distribution data for Joshua trees. 
   Refugia- Refugia map data from Shyrock et al. 2025 Assessing uncertainty in forecasts of refugia for Joshua trees. Validation data 
    Spatial- contains mapping data for creating map figures in r
		Validation data- Contains all data used for validation of results. 


 
