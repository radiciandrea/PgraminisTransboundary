# Analisis of observation burden for country and for strategy

rm(list = ls())

library(sf)
library(Matrix)
library(dplyr)
library(pracma)
library(ggplot2)
library(purrr)
library(reshape2)
library(scales)
library(RColorBrewer)
library(distillery)

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
                           "country" = esf_shp$Country,
                           "continent_bycell" = esf_shp$Continent)

# a df with all the countries characteristics is created (name, n_cells, continent)
country_df <- cell_country %>%
  group_by(country) %>%
  summarize(n_cell = n(), 
            continent = unique(continent_bycell))

countries = country_df$country

########### Load list

load("Data/national_coverage_list_cal1316.RData")

ldf <- nrow(national_coverage_list[[1]]) # n of lines of whatever dataframe 
ndf <- length(national_coverage_list)

national_coverage_df <- data.frame("algorithm" = character(ldf*ndf),
                                   "country" = character(ldf*ndf),
                                   "iteration" = numeric(ldf*ndf),
                                   "nat_set_size" = numeric(ldf*ndf),
                                   "nat_coverage" = numeric(ldf*ndf),
                                   "nat_rel_coverage" =  numeric(ldf*ndf),
                                   "continent" = character(ldf*ndf),
                                   "sentinel" = rep(c(ncimnc_df$sentinel, cisc_df$sentinel), times = ndf),
                                   "sentinel_country" = rep(c(ncimnc_df$sentinel_country, cisc_df$sentinel_country), times = ndf))

# A long dataframe with all the coverages and sentinels for each set cover iteration is created (for each country and algorithm/strategy)
for (i in 1: ndf) {
  national_coverage_df[(1+ldf*(i-1)) : (ldf*i), 1:5] <- national_coverage_list[[i]]
  national_coverage_df[(1+ldf*(i-1)) : (ldf*i), 6] <- national_coverage_list[[i]]$nat_coverage/country_df$n_cell[i]*100
  national_coverage_df[(1+ldf*(i-1)) : (ldf*i), 7] <- country_df$continent[i]
}

# ncimnc [non cooperative]

national_nc_coverage_df <- national_coverage_df %>%
  filter(algorithm == "noncoop-inter-maximiseNationalCover")

#plot 1: ncimnc (non cooperative, fig. 2a)

plot_national_nc_coverage_df <- national_nc_coverage_df %>%
mutate(countryplot = case_when(country == "United States" ~ "United States",
                               country == "Russia" ~ "Russia",
                               country == "Australia" ~ "Australia",
                               country == "Argentina" ~ "Argentina",
                               country == "Algeria" ~ "Algeria",
                               country == "France" ~ "France",
                               TRUE ~ "Other country"))

pl_ncimnc <- ggplot() +
  geom_line(data = plot_national_nc_coverage_df , aes(x = (nat_set_size+0), y = nat_rel_coverage, group = country), lwd = 0.5, color = "#a6a6a6") +
  geom_line(data = plot_national_nc_coverage_df [plot_national_nc_coverage_df $country == "Algeria",],
            aes(x = (nat_set_size+0), y = nat_rel_coverage), lwd = 1, color = "#003f5c") +
  geom_line(data = plot_national_nc_coverage_df [plot_national_nc_coverage_df $country == "Argentina",],
            aes(x = (nat_set_size+0), y = nat_rel_coverage), lwd = 1, color = "#444e86") +
  geom_line(data = plot_national_nc_coverage_df [plot_national_nc_coverage_df $country == "Australia",],
            aes(x = (nat_set_size+0), y = nat_rel_coverage), lwd = 1, color = "#955196") +
  geom_line(data = plot_national_nc_coverage_df [plot_national_nc_coverage_df $country == "France",],
            aes(x = (nat_set_size+0), y = nat_rel_coverage), lwd = 1, color = "#dd5182") +
  geom_line(data = plot_national_nc_coverage_df [plot_national_nc_coverage_df $country == "Russia",],
            aes(x = (nat_set_size+0), y = nat_rel_coverage), lwd = 1, color = "#ff6e54") +
  geom_line(data = plot_national_nc_coverage_df [plot_national_nc_coverage_df $country == "United States",],
            aes(x = (nat_set_size+0), y = nat_rel_coverage), lwd = 1, color = "#ffa600") +
  xlim(0, 420) +
  ylim(0, 100) +
  scale_x_continuous(trans='log2') +
  xlab("Sentinel set size by country") + 
  ylab("Relative coverage size by country (%)") + 
  geom_hline(yintercept=50, size=1) +
  theme_test()  

# cisc  [cooperative]  

surv_df0 <- surv_df %>%
  mutate(sentinel_country_sel = sentinel_country) 

surv_df0$sentinel_country_sel[(surv_df$sentinel_country != "Russia") &
                                 (surv_df$sentinel_country != "Australia") &
                                 (surv_df$sentinel_country != "United States") &
                                 (surv_df$sentinel_country != "Argentina") &
                                 (surv_df$sentinel_country != "Algeria") &
                                 (surv_df$sentinel_country != "France")] = "Other country"

cisc_df0 <- surv_df0 %>% 
  filter(algorithm == "coop-inter-setcover") %>%
  filter(sentinel_country_sel != "Other country") %>%
  select(coverage_size, sentinel_country_sel)

cisc_df_oc <- surv_df0 %>% 
  filter(algorithm == "coop-inter-setcover") %>%
  select(coverage_size, sentinel_country_sel) %>%
  mutate(sentinels = row_number())
  
cisc_df0 <- cisc_df0 %>%  
  mutate(Russia = 0) %>%
  mutate(Australia = 0) %>%
  mutate(United_States= 0) %>%
  mutate(Argentina = 0) %>%
  mutate(Algeria = 0) %>%
  mutate(France = 0) 

cisc_df0$sentinel_country_sel[1]
cisc_df0$Russia[1] = 1 # Russia needs to be the first

for (i in 2:nrow(cisc_df0)){
  cisc_df0$Russia[i] = cisc_df0$Russia[i-1] + (cisc_df0$sentinel_country_sel[i] == "Russia")*1
  cisc_df0$Australia[i] = cisc_df0$Australia[i-1] + (cisc_df0$sentinel_country_sel[i] == "Australia")*1
  cisc_df0$United_States[i] = cisc_df0$United_States[i-1] + (cisc_df0$sentinel_country_sel[i] == "United States")*1
  cisc_df0$Argentina[i] = cisc_df0$Argentina[i-1] + (cisc_df0$sentinel_country_sel[i] == "Argentina")*1
  cisc_df0$Algeria[i] = cisc_df0$Algeria[i-1] + (cisc_df0$sentinel_country_sel[i] == "Algeria")*1
  cisc_df0$France[i] = cisc_df0$France[i-1] + (cisc_df0$sentinel_country_sel[i] == "France")*1
 }

cisc_df0_res <- melt(cisc_df0, id.vars = c("coverage_size", "sentinel_country_sel"))

cisc_df0_res <- cisc_df0_res %>%
  mutate(sentinels = value) %>%
  mutate(country = variable) %>%
  select(sentinels, country, coverage_size) 

#plot 2: cisc  (cooperative, fig. 2b)

pl_cisc <- ggplot() + 
  geom_area(data = cisc_df_oc, aes(y = sentinels, x=coverage_size), fill ="#a6a6a6", alpha = 1, lwd = 0.1, color = "black") +
  geom_area(data = cisc_df0_res, aes(y = sentinels, x=coverage_size, fill = country), alpha=1, lwd = 0.1, color = "black") +
  xlab("Global coverage size (%)") +
  ylab("Global sentinel set size") +
  #ggtitle("Coop") +
  scale_fill_manual(values=c("#ff6e54", "#955196", "#ffa600", "#444e86", "#003f5c", "#dd5182")) +
  xlim(0, 100) +
  ylim(0, 1010) +
  #scale_y_continuous(trans='log10') + # breaks=c(2^3, 2^6, 2^9, 2^12),
  theme_test() +
  coord_flip() +
  theme(legend.position = "none")

#How many sentinels are needed to cover sigma = 50% of each country? with a ncinmc scenario?

sigma = 50

alpha_50_df <- national_coverage_df %>%
  filter(algorithm == "noncoop-inter-maximiseNationalCover") %>%
  group_by(country) %>%
  mutate(soglia1 = 1*(nat_rel_coverage >= sigma)) %>%
  group_by(country) %>%
  filter(soglia1 == 1) %>%
  filter(iteration == min(iteration))

n_sentinels_sigma50_ncinmc <- sum(alpha_50_df$nat_set_size) # 209 sentinels needed
coverage_sigma50_ncinmc <- sum(alpha_50_df$nat_coverage)/n_cell # effective coverage: 57.8%

#sigmas = 1:1:100 (to compute the cost-benefit index alpha)

sigmas = seq(1, 100, by = 1)
alpha_m <- matrix(NA, nrow = length(countries), ncol = length(sigmas))

for (k in 1:length(sigmas)) {
  alpha_temp_df <- national_coverage_df %>%
    mutate(cod_alg_country = paste(algorithm, country, sep = "_")) %>%
    group_by(cod_alg_country) %>%
    mutate(soglia1 = 1*(nat_rel_coverage >= sigmas[k])) %>%
    filter(soglia1 == 1) %>%
    filter(iteration == min(iteration))
  
  alpha_m[,k] <- alpha_temp_df$nat_set_size[alpha_temp_df$algorithm == "coop-inter-setcover" ]/
    alpha_temp_df$nat_set_size[alpha_temp_df$algorithm == "noncoop-inter-maximiseNationalCover"]
}

# We compute the ratio between needed sentinel set size for both strategy
alpha_df <- data.frame("Country" = countries,
                      "Continent" = country_df$continent,
                      "cells" = country_df$n_cell)

# Tab S1 modified: Unconnected U [CoopAdverse], Connected observeD CD [CoopBeneficial],
# Connected ObservinG CG [CoopAdverse], depending on sigma DG

alpha_df <- alpha_df %>%
  mutate(mean_alpha = rowMeans(alpha_m)) %>%
  mutate(min_alpha =  apply(alpha_m, 1, min)) %>%
  mutate(max_alpha =  apply(alpha_m, 1, max)) %>%
  group_by(Country) %>%
  mutate(alpha_pattern = case_when(min_alpha == 1 & 
                                    max_alpha == 1 ~ "U", 
                                  min_alpha < 1 & 
                                    max_alpha <= 1   ~ "CD", 
                                  min_alpha >= 1 & 
                                    max_alpha> 1  ~ "CG",
                                  min_alpha < 1 & 
                                    max_alpha > 1 ~ "DG")) %>%
  mutate(size = case_when(cells > 45 ~ "L",
                          cells <= 12 ~"S",
                          TRUE ~ "M")) 

#### Uncomment the following to plot each pattern of fig. S1

# for (i in 1:length(countries)){
# 
#   temp_df  <- national_coverage_df%>%
#     filter(country == alpha_df$Country[i]) %>%
#     mutate(sigma = nat_coverage/max(nat_coverage)*100)
#   
#   alpha_temp_df <- data.frame(sigma = 1:100,
#                              alpha = alpha_m[i,])
# 
#   # p <- ggplot(temp_df, aes(x = nat_set_size, y = nat_coverage, color = algorithm)) +
#   #   geom_line(lwd = 2) +
#   #   theme_test() +
#   #   #geom_hline(yintercept=50, size=1) +
#   #   ggtitle(countries[i]) +
#   #   theme(legend.position = c(0.75, 0.15))
#   
#   coeff = 3/max(temp_df$nat_set_size)
# 
#   p <- ggplot(temp_df, aes(x = sigma, y = nat_set_size)) +
#     geom_vline(xintercept=50, color="gray90", lwd = 0.5)+
#     geom_vline(xintercept=100, color="gray90", lwd = 0.5)+
#     geom_hline(yintercept=1/coeff, color="gray70", lwd = 0.5)+
#     geom_line(aes(color = algorithm, linetype = algorithm), lwd = 1.5) +
#     geom_line(data = alpha_temp_df, aes(y = alpha/coeff), color = "black", lwd = 0.5) +
#     #xlab("Sentinel set size") +
#     #ylab("Relative coverage size (%)") +
#     xlab("") +
#     ylab("") +
#     theme_test() +
#     #geom_hline(yintercept=50, size=1) +
#     
#     scale_y_continuous(
#       
#       # Features of the first axis
#       name = "Sentinel set size",
#       
#       # Add a second axis and specify its features
#       sec.axis = sec_axis(~.*coeff, name="Alpha")
#     )+
#     
#     ggtitle(countries[i]) +
#     theme(legend.position = "none")
# 
#   ggsave(paste("Data/",
#                alpha_df$Country[i], "_", alpha_df$size[i], "_", alpha_df$alpha_pattern[i], ".png",sep=""), dpi = 300, width = 3, height = 3)
# 
# }
         
# plot cost-benefit index (Fig. 3)

pl_alpha <-ggplot(alpha_df, aes(x = reorder(Country, mean_alpha), y = mean_alpha, fill = Continent)) +
  ylim(0, 1.75) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = 1) +
  coord_flip() +
  theme_test() +
  theme(legend.position = c(0.8,0.2),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

# width proportional to wheat production
# downloaded from: https://www.fao.org/faostat/en/#data/QCL)


Wheat_table = as.data.frame(read.csv2("Data/FAOSTAT_prod_wheat_2010_2020.csv", 
                        header = TRUE,
                        sep = ","))

Wheat_prod_bycountry = Wheat_table %>%
  mutate(average_production_t = 0) %>%
  group_by(Area) %>%
  summarise(average_production_t = sum(as.numeric(Value))) %>%
  ungroup()
  

alpha_df_prod <- alpha_df %>%
  ungroup() %>%
  left_join(Wheat_prod_bycountry, by = c("Country" = "Area")) %>%
  arrange(mean_alpha) %>%
  mutate(w = average_production_t) %>%
  mutate(r = cumsum(w)) %>%
  mutate(l = r - w) %>%
  mutate(pos = (l+r)/2) %>%
  mutate(pos_lab = 0.85*mean_alpha  - 0.075*is.even(row_number()) + 0.075*is.odd(row_number()))

pl_pr <- ggplot(alpha_df_prod, aes(ymin = 0)) + 
  ylim(-0.15, 1.75) +
  geom_rect(aes(xmin = l+0.02, xmax = r-0.02, ymax = mean_alpha, fill = Continent)) +
  geom_text(aes(label = Country, x = pos, y = pos_lab), size=2) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  theme_test() +
  theme(legend.position = c(0.85,0.15), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) #to be modified with inkscape

pl_pr_ink <- ggplot(alpha_df_prod, aes(ymin = 0)) + 
  ylim(0, 1.5) +
  geom_rect(aes(xmin = l+0.02, xmax = r-0.02, ymax = mean_alpha, fill = Continent)) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  theme_test() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position= c(0.85,0.15),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) #to be modified with inkscape

###plot map Fig. 4 main text

alpha_mean_df <- alpha_df %>%
  select(c("Country", "Continent", "mean_alpha", "cells", "alpha_pattern", "size"))

worlds_sf <- st_read("Data/World_Countries_m.shp")  

worlds_alpha_sf <- right_join(worlds_sf, alpha_mean_df, by = c("COUNTRY" = "Country"))

ggplot() +
  geom_sf(data = worlds_sf, fill = "gray70", colour = "grey50") +
  geom_sf(data = worlds_alpha_sf, aes(fill = mean_alpha), colour = "grey50")

## By continent [to be added to the map]

alpha_df_prod_cont <- left_join(alpha_df_prod, country_df, by = c("Country" =  "country")) %>%
  mutate(alpha_prod = mean_alpha*average_production_t)%>%
  group_by(continent) %>%
  summarize(tot_prod = sum(average_production_t, na.rm = T),
            tot_alpha_prod = sum(alpha_prod, na.rm = T)) %>%
  mutate(alpha_prod_mean = tot_alpha_prod/tot_prod) 

save(alpha_df, file = "Data/alpha_df_cal.RData")
