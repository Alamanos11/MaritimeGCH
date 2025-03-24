library(ggplot2)
library(readxl)
library(tidyverse)
library(ggthemes)
library(xlsx)
library(viridis)


# Capacity ----------------------------------------------------------------

cap <- read.csv('cap.csv')

write.xlsx(cap, 'MaritimeGCH_inputs.xlsx', sheetName = 'capacity', append = F, row.names = F)


# CII ---------------------------------------------------------------------

cii <- read.csv('CII_desired.csv')

write.xlsx(cii, 'MaritimeGCH_inputs.xlsx', sheetName = 'CIIdesired', append = T, row.names = F)

# CO2 Cap -----------------------------------------------------------------

co2_cap_fueleu <- read.csv('co2_cap_real.csv') %>% rename(co2_cap_fueleu = cap)
co2_cap_opt <- read.csv('co2_cap_opt.csv') %>% rename(co2_cap_opt = cap)
co2_cap_pess <- read.csv('co2_cap_pess.csv') %>% rename(co2_cap_pess = cap)
co2_cap_no <- read.csv('co2_cap_no.csv') %>% rename(co2_cap_no = cap)

# co2_cap_real <- co2_cap_med %>%
#   rename(cap = co2_cap_med) %>%
#   mutate(cap = c(7.4,
#                  7.3704,
#                  7.3408,
#                  7.3112,
#                  7.2816,
#                  7.252,
#                  6.956,
#                  6.886111111,
#                  6.816222222,
#                  6.746333333,
#                  6.676444444,
#                  6.606555556,
#                  6.536666667,
#                  6.466777778,
#                  6.396888889,
#                  6.327,
#                  6.0828,
#                  5.8386,
#                  5.5944,
#                  5.3502,
#                  5.106,
#                  4.6472,
#                  4.1884,
#                  3.7296,
#                  3.2708,
#                  2.812,
#                  2.5456,
#                  2.2792,
#                  2.0128,
#                  1.7464,
#                  1.48)) # FuelEU standards

# write.csv(co2_cap_real, 'co2_cap_real.csv', row.names = F)

co2_cap_all <- co2_cap_fueleu %>%
  left_join(co2_cap_opt) %>%
  left_join(co2_cap_pess) %>%
  left_join(co2_cap_no)  %>%
  pivot_longer(cols = c(co2_cap_fueleu, 
                        co2_cap_opt, 
                        co2_cap_pess, 
                        co2_cap_no),
               names_to = "scen", 
               values_to = "cap") %>%
  group_by(scen) %>%
  arrange(year) %>%
  mutate(yoy = (cap-lag(cap))/lag(cap), 
         cap = cap) # adjust based on Greece emissions

co2_cap_out <- co2_cap_fueleu %>%
  left_join(co2_cap_opt) %>%
  left_join(co2_cap_pess) %>%
  left_join(co2_cap_no)  %>%
  transmute(year, 
            `cap - no` = co2_cap_no, 
            `cap - pessimistic` = co2_cap_pess, 
            `cap - optimistic` = co2_cap_opt, 
            `cap - real` = co2_cap_fueleu)

write.xlsx(co2_cap_out, 'MaritimeGCH_inputs.xlsx', sheetName = 'co2 cap', append = T, row.names = F)


co2_cap_all %>%
  ggplot(aes(x = year, y = cap, color = scen)) + 
  geom_line(linewidth = 1) + 
  labs(x = "Year",
       y = "ETS Cap (MT CO2)",
       color = "Scenario") +
  
  # Apply clean theme with enhanced readability
  theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


# Shipping Demand ---------------------------------------------------------


ssp1 <- read.csv('demand_shippingSSP1.csv') %>% rename(ssp1 = demand)
ssp2 <- read.csv('demand_shippingSSP2.csv')  %>% rename(ssp2 = demand)
ssp5 <- read.csv('demand_shippingSSP5.csv')  %>% rename(ssp5 = demand)

oil_t <- ssp5 %>%
  arrange(year) %>%
  rename(oil_t = ssp5) %>%
  filter(ship_type == "T")


## Unused - oil tanker declnie based on Drewry Report

# 
# for (year in 2020:2050) {
#   
#   oil_t <- oil_t %>%
#     mutate(oil_t = case_when(year > 2024  & year <= 2030 & ship_type == "T"
#                              ~ lag(oil_t, 1) * 1.007,
#                              year > 2030  & year <= 2040 & ship_type == "T"
#                              ~ lag(oil_t, 1) * .99,
#                              year > 2040  & year <= 2050 & ship_type == "T"
#                              ~ lag(oil_t, 1) * .997,
#                              TRUE ~ oil_t))
#   
# }
# 
# oil_t %>%
#   ggplot(aes(x = year, y = oil_t, color = ship_type)) + 
#   geom_line()
# 
# oil_t1 <- oil_t %>%
#   bind_rows(ssp5 %>% filter(ship_type != "T") %>% rename(tanker = ssp5))
# #"https://www.bayes.city.ac.uk/__data/assets/pdf_file/0010/838414/Arjun-Batra-Tanker.pdf"



demand_all <- ssp1 %>%
  left_join(ssp2) %>%
  left_join(ssp5)  %>%
  pivot_longer(cols = c(ssp1,
                        ssp2, 
                        ssp5),
               names_to = "scen", 
               values_to = "demand")


demand_all %>%
  ggplot(aes(x = year, y = demand, color = scen)) + 
  facet_grid(~ship_type) + 
  geom_line(linewidth = 1) + 
  labs(x = "Year",
       y = "Shipping Demand (GTNM)",
       color = "Scenario") +
  
  # Apply clean theme with enhanced readability
  theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 9, color = "grey30"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

demand_out <- ssp1 %>%
  left_join(ssp2) %>%
  left_join(ssp5)  

write.xlsx(demand_out, 'MaritimeGCH_inputs.xlsx', sheetName = 'demand shipping', append = T, row.names = F)

# Emissions Factor --------------------------------------------------------

### Changed tool to use decreased fuel consumption for technologies rather than reduced EF

emissions_factor <- read.csv('emissions_factor_new.csv') %>% rename(emissions_factor = factor)
# emissions_factor_engin_opt <- read.csv('emissions_factor_engin_opt_new.csv') %>% rename(emissions_factor_engin_opt = factor)
# emissions_factor_hull <- read.csv('emissions_factor_hull_new.csv') %>% rename(emissions_factor_hull = factor)
# emissions_factor_route_opt <- read.csv('emissions_factor_route_opt_new.csv')  %>% rename(emissions_factor_route_opt = factor)
# emissions_factor_cc <- read.csv('emissions_factor_cc.csv')  %>% rename(emissions_factor_cc = factor)
# emissions_factor_propul <- read.csv('emissions_factor_propul_new.csv')  %>% rename(emissions_factor_propul = factor)
# emissions_factor_port_call <- read.csv('emissions_factor_port_call_new.csv')  %>% rename(emissions_factor_port_call = factor)


emissions_factor_all <- emissions_factor 

# %>%
#   left_join(emissions_factor_engin_opt) %>%
#   left_join(emissions_factor_hull) %>%
#   left_join(emissions_factor_route_opt)  %>%
#   left_join(emissions_factor_route_opt)  %>%
#   left_join(emissions_factor_cc) %>%
#   left_join(emissions_factor_propul) %>%
#   left_join(emissions_factor_port_call) %>%
#   pivot_longer(cols = c(emissions_factor, 
#                         emissions_factor_engin_opt, 
#                         emissions_factor_hull, 
#                         emissions_factor_route_opt, 
#                         emissions_factor_cc, 
#                         emissions_factor_port_call, 
#                         emissions_factor_propul),
#                names_to = "scen", 
#                values_to = "factor")

emissions_factor %>%
  ggplot(aes(x = fuel_type, y = emissions_factor)) +
  geom_point() + 
  labs(x = "Fuel type",
       y = "Emissions Factor (ton co2/ton fuel combustion)") +
  
  # Apply clean theme with enhanced readability
  theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# emissions_factor_out <- emissions_factor %>%
#   left_join(emissions_factor_engin_opt) %>%
#   left_join(emissions_factor_hull) %>%
#   left_join(emissions_factor_route_opt)  %>%
#   left_join(emissions_factor_route_opt)  %>%
#   left_join(emissions_factor_cc) %>%
#   left_join(emissions_factor_propul) %>%
#   left_join(emissions_factor_port_call)

write.xlsx(emissions_factor, 'MaritimeGCH_inputs.xlsx', sheetName = 'emissions factor', append = T, row.names = F)

# ETS Price ---------------------------------------------------------------

ets_price_mod <- read.csv('ets_price_mod.csv') %>% rename(mod = price)
ets_price_no <- read.csv('ets_price_no.csv') %>% rename(no = price)
ets_price_strict <- read.csv('ets_price_strict.csv') %>% rename(strict = price)


ets_all <- ets_price_mod %>%
  left_join(ets_price_no) %>%
  left_join(ets_price_strict) %>%
  pivot_longer(cols = c(mod, no, strict), 
               names_to = 'scen', 
               values_to = 'ets_price')

ets_low <- data.frame(year = seq(2020, 2050, 1), 
                      price = rep(80, 31), 
                      price_new = NA) %>%
  arrange(year)

## Apply growth rates for each scenario

for (i in 2:nrow(ets_low)) {
  
  ets_low$price_new[1] = 80
  
  
  ets_low$price_new[i] = ets_low$price_new[i-1] * 1.01
  
}

ets_base <- data.frame(year = seq(2020, 2050, 1), 
                       price = rep(80, 31), 
                       price_new = NA) %>%
  arrange(year)

for (i in 2:nrow(ets_base)) {
  
  ets_base$price_new[1] = 80
  
  
  ets_base$price_new[i] = ets_base$price_new[i-1] * 1.03
  
}

ets_high <- data.frame(year = seq(2020, 2050, 1), 
                       price = rep(80, 31), 
                       price_new = NA) %>%
  arrange(year)

for (i in 2:nrow(ets_high)) {
  
  ets_high$price_new[1] = 80
  
  
  ets_high$price_new[i] = ets_high$price_new[i-1] * 1.05
  
}


ets_all <- ets_low %>%
  rename(low = price_new) %>%
  left_join(ets_base%>%
              rename(base = price_new)) %>%
  left_join(ets_high%>%
              rename(high = price_new)) %>%
  pivot_longer(cols = c(low, base, high), 
               names_to = 'scen', 
               values_to = 'ets_price')

ets_out <- ets_low %>%
  rename(low = price_new) %>%
  left_join(ets_base%>%
              rename(base = price_new)) %>%
  left_join(ets_high%>%
              rename(high = price_new)) %>%
  rename(const = price)


ets_all %>%
  ggplot(aes(x = year, y = ets_price, color = scen)) + 
  geom_line(linewidth = 1) + 
  labs(x = "Year",
       y = "ETS Price (EUR/ton CO2)",
       color = "Scenario") +
  
  # Apply clean theme with enhanced readability
  theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


write.xlsx(ets_out, 'MaritimeGCH_inputs.xlsx', sheetName = 'ets price', append = T, row.names = F)



# Fuel Availability -------------------------------------------------------

fuel_avail_no <- read.csv('fuel_avail_no.csv')


write.xlsx(fuel_avail_no, 'MaritimeGCH_inputs.xlsx', sheetName = 'fuel avail', append = T, row.names = F)


# Fuel Consumption --------------------------------------------------------

# nuse gravimetric energy density
energy_density_ratios <- data.frame(fuel_type = c("Oil", "RefPO", "NH3", "LPG", "LNG", "H2", "MeOH"), 
                                    ratio = c(1, 1.05, .29, .63, .56, .21, .38))

# 1% increase in efficiency each year
eff_yr <- data.frame(year = seq(2020, 2050, 1), 
                     eff = rep(1,31))

for (i in 1:nrow(eff_yr)) {
  
  if (i == 1) {
    
    eff_yr$eff[i] =  eff_yr$eff[i] }
  
  else {
    
    eff_yr$eff[i] = eff_yr$eff[i-1] - .01
    
  }
  
}




fuel_consumption_fast <- read.csv('fuel_consumption_fast.csv') %>% rename(fuel_cons_fast = consumption) %>%
  filter(engine_type == "ME-LGI") %>% select(-engine_type)
fuel_consumption_med <- read.csv('fuel_consumption_med.csv')%>% rename(fuel_cons_med = consumption)  %>%
  filter(engine_type == "ME-LGI") %>% select(-engine_type)
fuel_consumption_slow <- read.csv('fuel_consumption_slow.csv')%>% rename(fuel_cons_med = consumption)  %>%
  filter(engine_type == "ME-LGI") %>% select(-engine_type)

# Fast --------------------------------------------------------------------
yearly_energy_dens <- fuel_consumption_fast %>%
  left_join(energy_density_ratios) %>%
  left_join(eff_yr) %>%
  mutate(fuel_dens = fuel_cons_fast * ratio) %>%
  group_by(ship_type, year, eff) %>%
  summarize(fuel_dens = sum(fuel_dens)) %>%
  ungroup() %>%
  group_by(ship_type) %>%
  mutate(fuel_dens_new = fuel_dens[year == "2020"] * eff) %>%
  ungroup() %>%
  select(year, ship_type, fuel_dens_new) %>%
  mutate(fuel_dens_new = fuel_dens_new/0.75)


fast_ratios <- fuel_consumption_fast %>%
  group_by(ship_type, year) %>%
  mutate(tot_fuel = sum(fuel_cons_fast)/0.75) %>%
  ungroup() %>%
  mutate(yrly_share = (fuel_cons_fast/0.75)/tot_fuel) %>%
  select(-tot_fuel) %>%
  group_by(year, ship_type)

fuel_cons_fast <- fuel_consumption_fast %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios) %>%
  left_join(fast_ratios) %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio) %>%
  rename(consumption = new_cons) %>%
  distinct(ship_type, fuel_type, year, consumption) %>%
  mutate(year = year + 5) %>%
  filter(year <= 2050)


fuel_cons_fast %>%
  group_by(year, fuel_type) %>%
  summarize(sum_fuel = sum(consumption)) %>%
  ungroup() %>%
  group_by(year) %>% 
  mutate(tot_sum = sum(sum_fuel)) %>%
  ungroup() %>%
  group_by(year, fuel_type) %>%
  summarize(perc = sum_fuel/tot_sum) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(names_from = fuel_type, 
              values_from = perc) 

write.csv(fuel_cons_fast  ,'fuel_cons_fast.csv')



# slow --------------------------------------------------------------------
yearly_energy_dens <- fuel_consumption_slow %>%
  left_join(energy_density_ratios) %>%
  left_join(eff_yr) %>%
  mutate(fuel_dens = fuel_cons_med * ratio) %>%
  group_by(ship_type, year, eff) %>%
  summarize(fuel_dens = sum(fuel_dens)) %>%
  ungroup() %>%
  group_by(ship_type) %>%
  mutate(fuel_dens_new = fuel_dens[year == "2020"] * eff) %>%
  ungroup() %>%
  select(year, ship_type, fuel_dens_new) %>%
  mutate(fuel_dens_new = fuel_dens_new/0.75)

slow_ratios <- fuel_consumption_slow %>%
  group_by(ship_type, year) %>%
  mutate(tot_fuel = sum(fuel_cons_med)/0.75) %>%
  ungroup() %>%
  mutate(yrly_share = ((fuel_cons_med)/0.75)/tot_fuel) %>%
  select(-tot_fuel) %>%
  group_by(year, ship_type)

fuel_cons_slow <- fuel_consumption_slow %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios) %>%
  left_join(slow_ratios) %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio)  %>%
  rename(consumption = new_cons) %>%
  distinct(ship_type, fuel_type, year, consumption) %>%
  mutate(year = year + 5) %>%
  filter(year <= 2050)


fuel_cons_slow %>%
  group_by(year, fuel_type) %>%
  summarize(sum_fuel = sum(consumption)) %>%
  ungroup() %>%
  group_by(year) %>% 
  mutate(tot_sum = sum(sum_fuel)) %>%
  ungroup() %>%
  group_by(year, fuel_type) %>%
  summarize(perc = sum_fuel/tot_sum) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(names_from = fuel_type, 
              values_from = perc)
  

write.csv(fuel_cons_slow,'fuel_cons_slow.csv', row.names = F)


# Fuel Cons MEd -----------------------------------------------------------




yearly_energy_dens <- fuel_consumption_med %>%
  left_join(energy_density_ratios) %>%
  left_join(eff_yr) %>%
  mutate(fuel_dens = fuel_cons_med * ratio) %>%
  group_by(ship_type, year, eff) %>%
  summarize(fuel_dens = sum(fuel_dens)) %>%
  ungroup() %>%
  group_by(ship_type) %>%
  mutate(fuel_dens_new = fuel_dens[year == "2020"] * eff) %>%
  ungroup() %>%
  select(year, ship_type, fuel_dens_new) %>%
  mutate(fuel_dens_new = fuel_dens_new/0.75)


med_ratios <- fuel_consumption_med %>%
  group_by(ship_type, year) %>%
  mutate(tot_fuel = sum(fuel_cons_med)/0.75) %>%
  ungroup() %>%
  mutate(yrly_share = (fuel_cons_med/0.75)/tot_fuel) %>%
  select(-tot_fuel) %>%
  group_by(year, ship_type) %>%
  mutate(h2_flag = ifelse(yrly_share[fuel_type == "H2"] > .15, 
                          1, 0), 
         h2_diff = ifelse(h2_flag == 1, yrly_share[fuel_type == "H2"] - .15, 0)) %>%
  ungroup()


fuel_cons_base <- fuel_consumption_med %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios) %>%
  left_join(med_ratios) %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio)  %>%
  rename(consumption = new_cons) %>%
  distinct(ship_type, fuel_type, year, consumption) %>%
  mutate(year = year + 5)  %>%
  filter(year <= 2050)


# fuel_cons_tech <- fuel_cons_new %>%
#   mutate(new_cons = new_cons * .9)
# 
# fuel_cons_tech %>%
#   filter(ship_type == "C") %>%
#   ggplot(aes(x = year, y = new_cons, color = fuel_type)) + 
#   geom_line()

write.csv(fuel_cons_base, 'fuel_cons_base.csv')

fuel_cons_base %>%
  group_by(year, fuel_type) %>%
  summarize(sum_fuel = sum(consumption)) %>%
  ungroup() %>%
  group_by(year) %>% 
  mutate(tot_sum = sum(sum_fuel)) %>%
  ungroup() %>%
  group_by(year, fuel_type) %>%
  summarize(perc = sum_fuel/tot_sum) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(names_from = fuel_type, 
              values_from = perc) 
fuel_cons_engine_opt <- fuel_cons_base %>%
  mutate(consumption = consumption * .93) %>%
  distinct(ship_type, fuel_type, year, consumption)

fuel_cons_route_opt <- fuel_cons_base %>%
  mutate(consumption = consumption * .965) %>%
  distinct(ship_type, fuel_type, year, consumption)

fuel_cons_port_call <- fuel_cons_base %>%
  mutate(consumption = consumption * .985) %>%
  distinct(ship_type, fuel_type, year, consumption)

fuel_cons_propul <- fuel_cons_base %>%
  mutate(consumption = consumption * .87) %>%
  distinct(ship_type, fuel_type, year, consumption)

fuel_cons_hull <- fuel_cons_base %>%
  mutate(consumption = consumption * .95) %>%
  distinct(ship_type, fuel_type, year, consumption)

fuel_cons_comb <- fuel_cons_base %>%
  mutate(consumption = consumption * .7) %>%
  distinct(ship_type, fuel_type, year, consumption)

write.csv(fuel_cons_engine_opt, 'fuel_engine_opt.csv')
write.csv(fuel_cons_route_opt, 'fuel_route_opt.csv')
write.csv(fuel_cons_port_call, 'fuel_port_call.csv')
write.csv(fuel_cons_propul, 'fuel_propul.csv')
write.csv(fuel_cons_hull, 'fuel_hull.csv')
write.csv(fuel_cons_comb, 'fuel_comb.csv')




# %>%
#   group_by(year, ship_type, h2_flag, h2_diff) %>%
#   mutate(yrly_share = case_when(h2_flag == 1 & fuel_type == "H2" ~ .15, 
#                                 h2_flag == 1 & fuel_type == "MeOH" ~ yrly_share + h2_diff/8, 
#                                 h2_flag == 1 & fuel_type == "NH3" ~ yrly_share + h2_diff/8, 
#                                 h2_flag == 1 & fuel_type == "LNG" ~ yrly_share + h2_diff/4,
#                                 h2_flag == 1 & fuel_type == "LPG" ~ yrly_share + h2_diff/2,
#                                 TRUE ~ yrly_share)) %>%
#   ungroup()

fuel_cons_meoh <- fuel_consumption_med %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios) %>%
  left_join(med_ratios) %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio)  %>%
  distinct(fuel_type, ship_type, year, yrly_share) %>%
  mutate(yrly_share = case_when(fuel_type == "LPG" & year >= 2040 ~ 0, 
                                fuel_type == "Oil" & year >= 2040 ~ 0, 
                                fuel_type == "MeOH" & year >= 2040 ~ .50, 
                                fuel_type == "H2" & year >= 2040 ~ .10,
                                fuel_type == "NH3" & year >= 2040 ~ .15,
                                fuel_type == "LNG" & year >= 2040 ~ .15,
                                fuel_type == "RefPO" & year >= 2040 ~ .0, 
                                TRUE ~ yrly_share)) %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios)  %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio) %>% 
  rename(consumption = new_cons) %>%
  mutate(year = year + 5) %>%
  filter(year <= 2050) %>%
  distinct(ship_type, fuel_type, year, consumption)



write.csv(fuel_cons_meoh, 'fuel_cons_meoh.csv')

fuel_cons_h2 <- fuel_consumption_med %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios) %>%
  left_join(med_ratios) %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio)  %>%
  distinct(fuel_type, ship_type, year, yrly_share) %>%
  mutate(yrly_share = case_when(fuel_type == "LPG" & year >= 2040 ~ 0.05, 
                                fuel_type == "Oil" & year >= 2040 ~ 0.05, 
                                fuel_type == "MeOH" & year >= 2040 ~ .25, 
                                fuel_type == "H2" & year >= 2040 ~ .2,
                                fuel_type == "NH3" & year >= 2040 ~ .3,
                                fuel_type == "LNG" & year >= 2040 ~ .15,
                                fuel_type == "RefPO" & year >= 2040 ~ .0, 
                                TRUE ~ yrly_share))  %>%
  left_join(yearly_energy_dens) %>%
  left_join(energy_density_ratios)  %>%
  mutate(new_cons = (fuel_dens_new * yrly_share)/ratio) %>% 
  rename(consumption = new_cons) %>%
  mutate(year = year + 5) %>%
  filter(year <= 2050) %>%
  distinct(ship_type, fuel_type, year, consumption)

write.csv(fuel_cons_h2 , 'fuel_cons_h2.csv')

fuel_cons_out <- fuel_cons_h2 %>%
  distinct(year,fuel_type, ship_type, consumption) %>%
  left_join(fuel_cons_meoh %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_meoh = consumption)) %>%
  left_join(fuel_cons_base %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_med = consumption)) %>%
  left_join(fuel_cons_engine_opt %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_med = consumption)) %>%
  left_join(fuel_cons_port_call %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_port_call = consumption)) %>%
  left_join(fuel_cons_route_opt %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_route_opt = consumption)) %>%
  left_join(fuel_cons_hull %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_hull = consumption)) %>%
  left_join(fuel_cons_propul %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_propul = consumption)) %>%
  left_join(fuel_cons_comb %>%
              distinct(year,fuel_type, ship_type, consumption) %>%
              rename(fuel_cons_comb = consumption))


  
write.xlsx(fuel_cons_out, 'MaritimeGCH_inputs.xlsx', sheetName = 'fuel consumption1', append = T, row.names = F)



# Fuel Cost ---------------------------------------------------------------

fuel_cost_med <- read.csv('fuel_cost_med.csv')

years <- 2020:2050

# Create a data frame with all combinations of years and fuel types
fuel_data <- expand.grid(
  year = years,
  fuel_type = c("Oil", "LNG", "LPG", "MeOH", "NH3", "RefPO", "H2")
)


### DNV Metrics

fuel_2050 <- fuel_data %>%
  distinct(fuel_type) %>%
  mutate(low = case_when(fuel_type == "Oil" ~ 250, 
                         fuel_type == "LNG" ~ 450, 
                         fuel_type == "LPG" ~ 450, 
                         fuel_type == "MeOH" ~ 700, 
                         fuel_type == "NH3" ~ 750, 
                         fuel_type == "RefPO" ~ 450, 
                         fuel_type == "H2" ~ 800, 
                         TRUE ~ NA), 
         high = case_when(fuel_type == "Oil" ~ 550, 
                          fuel_type == "LNG" ~ 600, 
                          fuel_type == "LPG" ~ 700, 
                          fuel_type == "MeOH" ~ 2100, 
                          fuel_type == "NH3" ~ 1800, 
                          fuel_type == "RefPO" ~ 700, 
                          fuel_type == "H2" ~ 2200, 
                          TRUE ~ NA), 
         base = (high + low)/2) %>%
  left_join(energy_density_ratios) %>%
  mutate(low_new = low * ratio/1000000, 
         base_new = base * ratio/1000000, 
         high_new = high * ratio/1000000)

fuel_cost_scenarios <- fuel_cost_med %>%
  left_join(fuel_2050) %>%
  group_by(fuel_type) %>%
  mutate(yr_inc_low = (low_new - cost[year == 2025])/30,
         yr_inc_base = (base_new - cost[year == 2025])/30,
         yr_inc_high = (high_new - cost[year == 2025])/30, 
         low = cost + (year - 2025) * yr_inc_low, 
         base = cost + (year-2025) * yr_inc_base, 
         high = cost + (year-2025) * yr_inc_high) %>%
  ungroup()

for (i in 6:30) {
  
  fuel_cost_scenarios$low[i] = fuel_cost_scenarios$cost[i]+(fuel_cost_scenarios$year[i]-2025) * fuel_cost_scenarios$yr_inc_low[i]
  fuel_cost_scenarios$base[i] = fuel_cost_scenarios$cost[i]+(fuel_cost_scenarios$year[i]-2025) * fuel_cost_scenarios$yr_inc_base[i]
  fuel_cost_scenarios$high[i] = fuel_cost_scenarios$cost[i]+(fuel_cost_scenarios$year[i]-2025) * fuel_cost_scenarios$yr_inc_high[i]
  
  
}

fuel_cost_out <- fuel_cost_scenarios %>%
  distinct(year, fuel_type, low, base, high)



fuel_cost_all <- fuel_cost_out %>%
  # left_join(fuel_cost_high) %>%
  # left_join(fuel_cost_med)  %>%
  pivot_longer(cols = c(low, 
                        high,
                        base),
               names_to = "scen", 
               values_to = "cost")

fuel_cost_all %>%
  ggplot(aes(x = year, y = cost, color = scen)) + 
  facet_grid(~fuel_type) + 
  geom_line(linewidth = 1) + 
  labs(x = "Year",
       y = "Fuel Cost (Million EUR/ton)",
       color = "Scenario") +
  
  # Apply clean theme with enhanced readability
  theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

fuel_cost_base <- fuel_cost_scenarios %>%
  distinct(year, fuel_type, base) %>%
  rename(cost = base) %>%
  filter(year >= 2025)

fuel_cost_low <- fuel_cost_scenarios %>%
  distinct(year, fuel_type, low) %>%
  rename(cost = low) %>%
  filter(year >= 2025)

fuel_cost_high <- fuel_cost_scenarios %>%
  distinct(year, fuel_type, high) %>%
  rename(cost = high) %>%
  filter(year >= 2025)


write.csv(fuel_cost_base, 'fuel_cost_base.csv', row.names = F)
write.csv(fuel_cost_high, 'fuel_cost_low.csv', row.names = F)
write.csv(fuel_cost_low, 'fuel_cost_high.csv', row.names = F)


write.xlsx(fuel_cost_out, 'MaritimeGCH_inputs.xlsx', sheetName = 'fuel costs', append = T, row.names = F)




# Init Age ----------------------------------------------------------------


init_age <- read.csv('init_age.csv') 

write.xlsx(init_age, 'MaritimeGCH_inputs.xlsx', sheetName = 'init age', append = T, row.names = F)


# Init Capacity Fleet -----------------------------------------------------

init_capacity <- read.csv('init_capacity_fleet.csv') 

write.xlsx(init_capacity, 'MaritimeGCH_inputs.xlsx', sheetName = 'init capacity', append = T, row.names = F)



# Investment Cost ---------------------------------------------------------

invest_cost <- read.csv('investment_cost.csv') 

write.xlsx(invest_cost, 'MaritimeGCH_inputs.xlsx', sheetName = 'investment cost', append = T, row.names = F)

# Investment Cost ---------------------------------------------------------

lifetime <- read.csv('lifetime.csv') 

write.xlsx(lifetime, 'MaritimeGCH_inputs.xlsx', sheetName = 'lifetime', append = T, row.names = F)


# Minim Capacity Fleet ----------------------------------------------------


minim_capacity_fleet <- read.csv('minim_capacity_fleet.csv') 

write.xlsx(minim_capacity_fleet, 'MaritimeGCH_inputs.xlsx', sheetName = 'minim capacity fleet', append = T, row.names = F)



# Op Cost ---------------------------------------------------------------

op_cost <- read.csv('op_cost.csv') %>% rename(op_cost = cost)
op_cost_hull <- read.csv('op_cost_hull.csv') %>% rename(op_cost_hull = cost)
op_cost_engine_opt <- read.csv('op_cost_engin_opt.csv') %>% rename(op_cost_engine_opt = cost)
op_cost_port_call <- read.csv('op_cost_port_call.csv') %>% rename(op_cost_port_call = cost)
op_cost_propul <- read.csv('op_cost_propul.csv') %>% rename(op_cost_propul = cost)
op_cost_route_opt <- read.csv('op_cost_route_opt.csv') %>% rename(op_cost_route_opt = cost)
op_cost_comb <- read.csv('op_cost_comb.csv') %>% rename(op_cost_comb = cost)
op_cost_cc <- read.csv('op_cost_co2_capture.csv') %>% rename(op_cost_cc = cost)



op_cost_all <- op_cost_hull %>%
  left_join(op_cost_engine_opt) %>%
  left_join(op_cost_port_call) %>%
  left_join(op_cost_propul) %>%
  left_join(op_cost_route_opt) %>%
  left_join(op_cost) %>%
  left_join(op_cost_comb) %>%
  left_join(op_cost_cc) %>%
  pivot_longer(cols = c(op_cost_hull, 
                        op_cost_engine_opt, 
                        op_cost_port_call, 
                        op_cost_propul, 
                        op_cost_route_opt, 
                        op_cost_comb, 
                        op_cost_cc), 
               names_to = "tech", 
               values_to = "cost")

# capex divided by 30 years, then added 

op_cost_all %>%
  ggplot(aes(x = tech, y = cost, color = ship_type)) +
  geom_point() + 
  labs(x = "Technology Scenario",
       y = "OPEX",
       color = "Ship Type") +
  
  # Apply clean theme with enhanced readability
  theme_minimal() +
  theme(
    # Text elements
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


op_cost_out <- op_cost_hull %>%
  left_join(op_cost_engine_opt) %>%
  left_join(op_cost_port_call) %>%
  left_join(op_cost_propul) %>%
  left_join(op_cost_route_opt) %>%
  left_join(op_cost) %>%
  left_join(op_cost_comb) %>%
  left_join(op_cost_cc)



write.xlsx(op_cost_out, 'MaritimeGCH_inputs.xlsx', sheetName = 'op cost', append = T, row.names = F)

# Production Capacity ----------------------------------------------------


prod_capacity <- read.csv('prod_capacity.csv') 

write.xlsx(prod_capacity, 'MaritimeGCH_inputs.xlsx', sheetName = 'production capacity', append = T, row.names = F)


# Emissions Factor --------------------------------------------------------



emissions_factor <- read.csv('emissions_factor.csv')

write.xlsx(emissions_factor, 'MaritimeGCH_inputs.xlsx', sheetName = 'emissions factor', append = T, row.names = F)

# Initial Emissions -------------------------------------------------------



today_emissions <- init_capacity %>%
  left_join(fuel_cons_base %>%
              filter(year == 2025) %>%
              select(ship_type, fuel_type, consumption)) %>%
  left_join(emissions_factor)

today_emissions1 <- today_emissions %>%
  mutate(emissions = capacity * consumption * factor * 10) %>%
  group_by(ship_type) %>%
  summarize(emissions = sum(emissions)/1000000) %>%
  ungroup() %>%
  mutate(tot_emissions = sum(emissions))
  
 