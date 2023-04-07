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

n_cell = nrow(esf_shp)

load("Data/B_C_i_calval.RData")

BC_cal_df <- data.frame("observer" = rep(NaN, length.out = sum(B_C_i_c)),
                        "observed" = rep(NaN, length.out = sum(B_C_i_c)))
tic()
x = 0
for (i in 1:nrow(esf_shp)) {
  y <- sum(B_C_i_c[,i]>0)
  if (y >0) {
    BC_cal_df$observer[x+1:y] <- i
    BC_cal_df$observed[x+1:y] <- which(B_C_i_c[,i]>0)
    x = x+y
  }
}
toc()

save(BC_cal_df, file = "Data/B_C_i_c_df.RData") #  dataframe representing C_D ("who (sentinel) observes who (node)")

cell_to_country <- esf_shp %>%
  st_drop_geometry() %>%
  select("new_id", "Country")

BC_c_df <- left_join(
  BC_cal_df,
  cell_to_country,
  by = c("observer" = "new_id"))

BC_c_df <- BC_c_df %>%
  mutate(observer_country  = Country) %>%
  select(-c("Country"))

BC_c_df <- left_join(
  BC_c_df,
  cell_to_country,
  by = c("observed" = "new_id"))

BC_c_df <- BC_c_df %>%
  mutate(observed_country  = Country) %>%
  select(-c("Country")) %>%
  mutate(TRANSBOUNDARY  = (observed_country!=observer_country)) 

BC_nc_df <- BC_c_df %>%
  filter(TRANSBOUNDARY == FALSE) # here we remove all transboudary edges

save(BC_c_df, BC_nc_df, file = "Data/B_C_i_cnc_df_cal1316.RData") # dataframe representing C_D [cooperative] and C_D^(-T) [non-cooperative] 

### create matrix for noncooperative surveillance
B_C_i <- B_C_i_c

B_C_nc_i <- matrix(data = 0, nrow = nrow(B_C_i), ncol = ncol(B_C_i))

for(i in 1: nrow(BC_nc_df)) {
  # we keep the same meaning of the B_C matrix: ij means spores going from i to j (thus, j observes i)
  B_C_nc_i[BC_nc_df$observed[i],BC_nc_df$observer[i]] <-1
}

B_C_nc_i<-as(B_C_nc_i, "sparseMatrix")

save(B_C_i, B_C_nc_i, file = "Data/B_C_i_cnc_cal1316.RData") #These matrix are C_D and C_D^(-T) (i.e. cooperative and not cooperative)
