# Create dataframe for COOPERATIVE and NONCOOPERATIVE surveillance

rm(list = ls())

setwd("C:/Users/radya/Desktop/Alcuni file permanenti/Dottorato/ArticoliConferenze/08_Puccinia_2_transboundary/Codice_da_pulire")

library(sf)
library(Matrix)
library(dplyr)
library(pracma)
library(ggplot2)
library(hrbrthemes)

esf_shp <- st_read("Data/Wheat_nodes.shp")

countries <- esf_shp$Country
countries_df <- data.frame(countries = countries,
                           id = esf_shp$new_id)

n_cell = nrow(esf_shp)

load("Data/B_C_i_calval.RData")

B_C_c_i_v <- B_C_i_v

x <- sum(B_C_c_i_v)

BC_c_v_df <- data.frame(observer = rep(NaN, times = x),
                      observed = rep(NaN, times = x))

tic()
w = 0
for (i in 1:nrow(esf_shp)) {
  y <- sum(B_C_c_i_v[,i]>0)
  if (y >0) {
    BC_c_v_df$observer[w+1:y] <- i
    BC_c_v_df$observed[w+1:y] <- which(B_C_c_i_v[,i]>0)
    w = w+y
  }

}
toc()

BC_c_v_df$observer_country <- countries[BC_c_v_df$observer]
BC_c_v_df$observed_country <- countries[BC_c_v_df$observed]
BC_c_v_df$TRANSBOUNDARY <- BC_c_v_df$observed_country != BC_c_v_df$observer_country

# remove tranboundary edges
BC_nc_v_df <- BC_c_v_df %>%
  filter(TRANSBOUNDARY == FALSE)

save(BC_c_v_df, BC_nc_v_df , file = "Data/B_C_i_cnc_df_val1718.RData") # dataframe representing C_V [cooperative] and C_V^(-T) [non-cooperative] 

B_C_nc_i_v <- B_C_i_v*0

for(i in 1: nrow(BC_nc_v_df)) {
  # we keep the same meaning of the B_C matrix: ij means spores going from i to j (thus, j observes i)
  B_C_nc_i_v[BC_nc_v_df$observed[i],BC_nc_v_df$observer[i]] <-1
}

B_C_nc_i_v<-as(B_C_nc_i_v, "sparseMatrix")

save(B_C_c_i_v, B_C_nc_i_v, file = "Data/B_C_i_cnc_val1718.RData") #These matrix are C_V and C_V^(-T) (i.e. cooperative and not cooperative)

