rm(list = ls())

library(sf)
library(Matrix)
library(dplyr)
library(pracma)
library(ggplot2)
library(hrbrthemes)
library(lpSolve)

esf_shp <- st_read("Data/Wheat_nodes.shp")

n_cell = nrow(esf_shp)

load("Data/B_C_i_cnc_df_cal1316.RData")
load("Data/B_C_i_cnc_cal1316.RData")

################ Cooperative (cisc) + Cd

BC_var<- BC_c_df

unobs_cells <- n_cell

sentinel_cisc <- c() #sentinels ID (sx set_cover_gh)
coverage_cisc <- c() #how many are observed
sentinel_country_cisc <- c() #sentinel country
cum_coverage_cisc <- c() # cumsum

i = 1

tic() # Set cover algorithm
while(unobs_cells > 0){ 
  sentinel <- as.numeric(names(sort(table(BC_var$observer),decreasing=TRUE)[1]))
  sentinel_country <- esf_shp$Country[sentinel]
  sentinel_cisc <- c(sentinel_cisc, sentinel)
  sentinel_country_cisc <- c(sentinel_country_cisc, sentinel_country)
  
  coverage<- BC_var$observed[BC_var$observer == sentinel]
  coverage_cisc <- c(coverage_cisc, length(coverage))
  

  if(i == 1) {
    cum_coverage_cisc <- length(coverage)
  } else {
    cum_coverage_cisc <- c(cum_coverage_cisc, cum_coverage_cisc[i-1] + length(coverage))
  }

  unobs_cells <- unobs_cells - length(coverage)

  for (j in 1:length(coverage)) {
    BC_var <- BC_var %>%
      filter(observed != coverage[j])
  }

  i = i+1
}
toc()

df_cisc = data.frame(sentinel = sentinel_cisc,
                     s_country = sentinel_country_cisc,
                     coverage = cum_coverage_cisc)

################ Non-cooperative (ncimnc) + Cd

# In this version, the non-cooperative prioritization is conducted step by step 
# considering each time to increase the aggregated coverage (%) of the least surveilled country

Cells_nc_coverage <- data.frame(id = 1:n_cell) %>%
  mutate(country = esf_shp$Country) %>%
  mutate(coverage = as.matrix(table(BC_nc_df$observer))) %>%
  group_by(country) %>%
  mutate(Nat_coverage = n()) %>%
  mutate(Frac_Nat_coverage = coverage/Nat_coverage) %>%
  ungroup()

Countries_nc_coverage  <- Cells_nc_coverage %>%
  select("country", "Nat_coverage") %>%
  distinct() %>%
  mutate(missing_coverage = Nat_coverage) %>%
  mutate(abs_coverage = 0) %>%
  mutate(rel_coverage = 0)


BC_var<- BC_nc_df
Countries_nc_coverage_var <- Countries_nc_coverage

unobs_cells <- n_cell

sentinel_ncimnc <- c() #sentinels ID (sx set_cover_gh)
coverage_ncimnc <- c() #how many are observed
sentinel_country_ncimnc <- c() #sentinel country
cum_coverage_ncimnc <- c() # cumsum
min_coverage_by_country_ncimnc = c()

i = 1

# Set cover algorithm
tic()
while(unobs_cells > 0){ 
  
  least_country_df = Countries_nc_coverage_var %>%
    filter(rel_coverage == min(rel_coverage)) %>%
    filter(Nat_coverage == max(Nat_coverage))
  
  least_country = least_country_df$country
  
  if(length(least_country>1)) {
    least_country = sample(least_country, 1)
  }
  
  sentinel <- as.numeric(names(sort(table(BC_var$observer[BC_var$observer_country == least_country]),decreasing=TRUE)[1]))
  sentinel_country <- least_country
  sentinel_ncimnc <- c(sentinel_ncimnc, sentinel)
  sentinel_country_ncimnc <- c(sentinel_country_ncimnc, sentinel_country)
  
  coverage<- BC_var$observed[BC_var$observer == sentinel]
  coverage_ncimnc <- c(coverage_ncimnc, length(coverage))
  
  if(i == 1) {
    cum_coverage_ncimnc <- length(coverage)
  } else {
    cum_coverage_ncimnc <- c(cum_coverage_ncimnc, cum_coverage_ncimnc[i-1] + length(coverage))
  }
  
  unobs_cells <- unobs_cells - length(coverage)
  
  for (j in 1:length(coverage)) {
    BC_var <- BC_var %>%
      filter(observed != coverage[j])
  }
  
  Countries_nc_coverage_var$missing_coverage[Countries_nc_coverage_var$country == least_country] = 
    Countries_nc_coverage_var$missing_coverage[Countries_nc_coverage_var$country == least_country] - length(coverage)
  
  Countries_nc_coverage_var$abs_coverage[Countries_nc_coverage_var$country == least_country] = 
    Countries_nc_coverage_var$abs_coverage[Countries_nc_coverage_var$country == least_country] + length(coverage)
    
  Countries_nc_coverage_var$rel_coverage[Countries_nc_coverage_var$country == least_country] = 
    Countries_nc_coverage_var$abs_coverage[Countries_nc_coverage_var$country == least_country]/
    Countries_nc_coverage_var$Nat_coverage[Countries_nc_coverage_var$country == least_country]
      
  min_coverage_by_country_ncimnc[i] <- min(Countries_nc_coverage_var$rel_coverage)
  
  i = i+1
}
toc()

surv_df <- data.frame("algorithm" = c(rep("coop-inter-setcover", length(cum_coverage_cisc)),
                                      rep("noncoop-inter-maximiseNationalCover", length(cum_coverage_ncimnc))),
                      "coverage_size" = c(cum_coverage_cisc,
                                          cum_coverage_ncimnc)/n_cell*100,
                      "sentinel" = c(sentinel_cisc,
                                     sentinel_ncimnc),
                      "sentinel_country" = c(sentinel_country_cisc,
                                             sentinel_country_ncimnc),
                      "sentinel_set_size" = c(seq(1:length(cum_coverage_cisc)),
                                              seq(1:length(cum_coverage_ncimnc)))/n_cell*100)

save(surv_df, file = "Data/surv_df_cal1316.RData") # dataframe with both cooperative and noncooperative strategy
