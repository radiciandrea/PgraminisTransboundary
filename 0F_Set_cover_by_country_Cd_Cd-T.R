rm(list = ls())

setwd("C:/Users/radya/Desktop/Alcuni file permanenti/Dottorato/ArticoliConferenze/08_Puccinia_2_transboundary/Codice_da_pulire")

library(sf)
library(Matrix)
library(dplyr)
library(pracma)
library(reshape2)

esf_shp <- st_read("Data/Wheat_nodes.shp")

n_cell = nrow(esf_shp)

load("Data/B_C_i_cnc_df_cal1316.RData")
load("Data/B_C_i_cnc_cal1316.RData")
load("Data/surv_df_cal1316.RData")

ncimnc_df <- surv_df %>% 
  filter(algorithm == "noncoop-inter-maximiseNationalCover") %>%
  select(sentinel, sentinel_country)

cisc_df <- surv_df %>% 
  filter(algorithm == "coop-inter-setcover") %>%
  select(sentinel, sentinel_country)

cell_country <- data.frame("id" = 1:n_cell,
                           "country" = esf_shp$Country)

M1 <- as.matrix(B_C_nc_i)
diag(M1) = 1

M2 <- as.matrix(B_C_i)
diag(M2) = 1

countries <- sort(unique(cell_country$country))
n_countries <- length(countries)

####### Countries: USA, China, Russia, France, Ethiopia, Australia, Argentina

national_coverage_df <- data.frame("algorithm" = character(nrow(ncimnc_df)+nrow(cisc_df)),
                                   "country" = character(nrow(ncimnc_df)+nrow(cisc_df)),
                                   "iteration" = numeric(nrow(ncimnc_df)+nrow(cisc_df)),
                                   "nat_set_size" = numeric(nrow(ncimnc_df)+nrow(cisc_df)),
                                   "nat_coverage" = numeric(nrow(ncimnc_df)+nrow(cisc_df)))

national_coverage_df$algorithm <- c(rep("noncoop-inter-maximiseNationalCover", times = nrow(ncimnc_df)),
                                    rep("coop-inter-setcover", times = nrow(cisc_df)))

national_coverage_list <- list() 

for(k in 1:n_countries) {
  national_coverage_df$country = rep(countries[k], times = nrow(ncimnc_df)+nrow(cisc_df))
  national_coverage_list[[k]] <- national_coverage_df 
}

###### Algorithm [strategiies]

## ncimnc [non cooperative]

tic()
for(i in 1:nrow(ncimnc_df)) { #
  
  for(k in 1:n_countries) {
    national_coverage_list[[k]]$iteration[i] <- i
    national_coverage_list[[k]]$nat_set_size[i] <- sum(ncimnc_df$sentinel_country[1:i]== countries[k])
    
    if(i == 1) {
      covered_cells <- which(M1[,ncimnc_df$sentinel[1]]>0)
    } else {
      covered_cells <- which(rowSums(M1[,ncimnc_df$sentinel[1:i]])>0)
    }
    
    national_coverage_list[[k]]$nat_coverage[i] <- sum(cell_country$country[covered_cells]== countries[k])
  }
  
  cat(i, "\n")
}
toc()

## cisc [cooperative]

tic()
for(j in 1:nrow(cisc_df)) {
  for(k in 1:n_countries) {
    national_coverage_list[[k]]$iteration[i+j] <- j
    national_coverage_list[[k]]$nat_set_size[i+j] <- sum(cisc_df$sentinel_country[1:j]== countries[k])
    
    if(j == 1) {
      covered_cells <- which(M2[,cisc_df$sentinel[1]]>0)
    } else {
      covered_cells <- which(rowSums(M2[,cisc_df$sentinel[1:j]])>0)
    }
    
    national_coverage_list[[k]]$nat_coverage[i+j] <- sum(cell_country$country[covered_cells]== countries[k])
  }
  cat(j, "\n")
}
toc()

save(national_coverage_list, file = "Data/national_coverage_list_cal1316.RData")