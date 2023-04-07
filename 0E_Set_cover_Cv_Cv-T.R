rm(list = ls())

setwd("C:/Users/radya/Desktop/Alcuni file permanenti/Dottorato/ArticoliConferenze/08_Puccinia_2_transboundary/Codice_da_pulire")

library(sf)
library(Matrix)
library(dplyr)
library(pracma)
library(ggplot2)
library(hrbrthemes)
library(lpSolve)

esf_shp <- st_read("Data/Wheat_nodes.shp")

countries <- esf_shp$Country
n_cell = nrow(esf_shp)

load("Data/surv_df_cal1316.RData") # here we load the surveillance strategies (i.e., the cooperative and non-coop sentinel set)
load("Data/B_C_i_cnc_val1718.RData") # here is "what thery would observe in 2017-2018" (as matrix)
load("Data/B_C_i_cnc_df_val1718.RData") # here is "what thery would observe in 2017-2018" (as dataframe)

################ Cooperative (cisc) + Cv

sentinel_cisc <- surv_df$sentinel[surv_df$algorithm == "coop-inter-setcover"]
sentinel_country_cisc <- surv_df$sentinel_country[surv_df$algorithm == "coop-inter-setcover"]
coverage_cisc <- c()
cum_coverage_cisc <- c()

tic()
for(i in 1:length(sentinel_cisc)){ 
  sentinel <- sentinel_cisc[i]
  
  if(i == 1) {
    coverage_cisc[i] <- sum(B_C_c_i_v[,sentinel])
    cum_coverage_cisc[i] <- coverage_cisc[i]
  } else {
    cum_coverage_cisc <- c(cum_coverage_cisc, sum(rowSums(B_C_c_i_v[,sentinel_cisc[1:i]])>0))
    coverage_cisc[i] <- cum_coverage_cisc[i] - cum_coverage_cisc[i-1]
  }
}
toc()

df_cisc_val = data.frame(sentinel = sentinel_cisc,
                         sentinel_country = sentinel_country_cisc,
                         coverage = coverage_cisc,
                         cum_coverage = cum_coverage_cisc)

################ Non-cooperative (ncimnc) + Cv

sentinel_ncimnc <- surv_df$sentinel[surv_df$algorithm == "noncoop-inter-maximiseNationalCover"]
sentinel_country_ncimnc<- surv_df$sentinel_country[surv_df$algorithm == "noncoop-inter-maximiseNationalCover"]
coverage_ncimnc <- c()
cum_coverage_ncimnc <- c()

tic()
for(i in 1:length(sentinel_ncimnc)){ 
  sentinel <- sentinel_ncimnc[i]
  
  if(i == 1) {
    coverage_ncimnc[i] <- sum(B_C_c_i_v[,sentinel])
    cum_coverage_ncimnc[i] <- coverage_ncimnc[i]
  } else {
    cum_coverage_ncimnc <- c(cum_coverage_ncimnc, sum(rowSums(B_C_c_i_v[,sentinel_ncimnc[1:i]])>0))
    coverage_ncimnc[i] <- cum_coverage_ncimnc[i] - cum_coverage_ncimnc[i-1]
  }
}
toc()

surv_df_val <- data.frame("algorithm" = c(rep("coop-inter-setcover", length(cum_coverage_cisc)),
                                          rep("noncoop-inter-maximiseNationalCover", length(cum_coverage_ncimnc))),
                          "coverage_size" = c(cum_coverage_cisc,
                                              cum_coverage_ncimnc)/n_cell*100,
                          "sentinel" = c(sentinel_cisc,
                                         sentinel_ncimnc),
                          "sentinel_country" = c(sentinel_country_cisc,
                                                 sentinel_country_ncimnc),
                          "sentinel_set_size" = c(seq(1:length(cum_coverage_cisc)),
                                                  seq(1:length(cum_coverage_ncimnc)))/n_cell*100)

save(surv_df_val, file = "Data/surv_df_val1718.RData")  # dataframe with both cooperative and noncooperative strategy (tested on Cv and Cv-T)
