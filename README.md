# RepoPgraminisTransboundary
Repository for Radici A., Martinetti D., Bevacqua D. -  “Global benefits and domestic costs of a cooperative surveillance strategy to control transboundary crop pathogens”

Codes have to be run in alphabetical order A -> J
Detail for each code:
-	0A_Network_Cd_Cv: this code loads the list of average annual connectivity matrices C and produces design network C_D (called B_C_i_c) and validation network C_V (called B_C_i_v).
-	0B_Network_Cd_Cd-T: this code produces a data frame representing odges (what node observes which node) in the cooperative (BC_c_df) and in the nooncoperative (BC_nc_df) strategy. In this second case, transboundary edges are identified from the shapefile “Wheat_nodes.shp” and removed (the “-T” in the name of the code stands for “without Transboundary”). The equivalent matrices are saved.
-	0C_Network_Cv_Cv-T: is analogous to the previous, but in the validation network.
-	0D_Set_cover_Cd_Cd-T: the set cover algorithm is applied, in both cooperative and non cooperative. In this later case we ran only one prioritization (the non-cooperative prioritization is conducted step by step at worldwide level, each time to increase the aggregated coverage (%) of the least surveilled country). Dataframe “surv_df” contains the  sentinel sets and the coverages sigma for both strategies (here called “algorithms”): cooperative ("coop-inter-setcover") and non-cooperative ("noncoop-inter-maximiseNationalCover")
-	0E_Set_cover_Cv_Cv-T: This code takes the output of the previous code and recompute the effective coverage with the same sentinel set but different connections (those from validation network C_V, corresponding to 2017-2018 air masses). This information is stored in “surv_df_val”.
-	0F_Set_cover_by_country_Cd_Cd-T: This code is compulsory for the computation of the cost-benefit index for each country and for strategy (algorithm). The output is, for each country and strategy, a data frame with the increasing domestic sentinel set and the corresponding coverage (sigma). The output is saved in a list (national_coverage_list)
-	0G_Set_cover_by_country_Cv_Cv-T: is analogous to the previous, but in the validation network.
-	0H_Sigma_Alfa_by_country_Cd_Cd-T: Analysis of the surveillance burden by country via the cost-benefit index alpha. Sentinel set size for sigma = 50 and 100 % is computed. Alpha by continent is computer as well.
-	0J_Sigma_Alfa_by_country_Cv_Cv-T: is analogous to the previous, but in the validation network.
-	0K_Correlation_Alfa_by_country_Cd_Cv: computes the correlation among average alpha computed with design network Cd (obtained via code 0H) and the validation one Cv (code 0J)
