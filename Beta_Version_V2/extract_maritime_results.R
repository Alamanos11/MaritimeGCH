library(ggplot2)
library(readxl)
library(tidyverse)
library(ggthemes)
library(xlsx)
library(viridis)
library(cowplot)
library(scales)
library(patchwork)


# -------------------------------------------------------------------------

results <- read_excel('maritime_results_all_scenarios.xlsx', 
                      sheet = 'base')
sheets <- excel_sheets('maritime_results_all_scenarios.xlsx')



# Total Cost --------------------------------------------------------------


total_cost <- data.frame()
yearly_cost <- data.frame()
emissions <- data.frame()
fuel_cost <- data.frame()
op_cost <- data.frame()
ets_penalty <- data.frame()


for (i in 1:length(sheets)){


  scen = sheets[i]
  
  results <- read_excel('maritime_results_all_scenarios.xlsx', 
                        sheet = scen)
  
  tot_cost_i <- data.frame(scen = scen, 
                         tot_cost = results$Total_Cost[1])
  
  yearly_cost_i <- data.frame(scen = scen, 
                              year = results$Year,
                            yearly_cost = results$Total_Cost_Per_Year)
  
  emissions_i <- data.frame(scen = scen, 
                            year = results$Year,
                            emissions = results$CO2_Emissions)
  
  
  fuel_cost_i <- data.frame(scen = scen, 
                            year = results$Year,
                          fuel_cost = results$Fuel_Cost)
  
  op_cost_i <- data.frame(scen = scen, 
                          year = results$Year,
                        op_cost = results$Operational_Cost)
  
  ets_penalty_i <- data.frame(scen = scen, 
                              year = results$Year,
                            ets_penalty = results$ets_penalty)
  
  total_cost <- bind_rows(total_cost, tot_cost_i)
  yearly_cost <- bind_rows(yearly_cost, yearly_cost_i)
  emissions <- bind_rows(emissions, emissions_i)
  fuel_cost <- bind_rows(fuel_cost, fuel_cost_i)
  op_cost <- bind_rows(op_cost, op_cost_i)
  ets_penalty <- bind_rows(ets_penalty, ets_penalty_i)
  
}


fuel_cost_ssp <- ets_penalty %>%
  filter(str_detect(scen, 'ssp') | str_detect(scen, 'base')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= ets_penalty, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(title = "Annual Technology Cost Trajectories",
       subtitle = "Comparison of Different Technology Scenarios",
       x = "Year",
       y = "Fuel Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )




fuel_cost_me_h2 <- fuel_cost %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= fuel_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(title = "Annual Technology Cost Trajectories",
       subtitle = "Comparison of Different Technology Scenarios",
       x = "Year",
       y = "Fuel Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


fuel_cost_me_h2

emissions_me_h2 <- emissions %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= emissions, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(title = "Annual Technology Cost Trajectories",
       subtitle = "Comparison of Different Technology Scenarios",
       x = "Year",
       y = "Emissions (MT)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )



emissions_me_h2

ets_penalty_me_h2 <- ets_penalty %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= ets_penalty, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(title = "Annual Technology Cost Trajectories",
       subtitle = "Comparison of Different Technology Scenarios",
       x = "Year",
       y = "ETS Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

ets_penalty_me_h2

yearly_cost_me_h2 <- yearly_cost %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= yearly_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "Yearly Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    axis.text.x = element_text(
      angle = 45,          # Angle the text to prevent overlap
      hjust = 1,           # Align text for better readability
      size = 9             # Adjust text size
    ),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    
    # Legend position and spacing
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

yearly_cost_me_h2

tech_fuel_cost <- fuel_cost %>%
  filter(str_detect(scen, 'tech')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  mutate(scen = sub(".*-\\s*(.*)", "\\1", scen), 
         scen = factor(scen)) %>%
  ggplot(aes(x = year, y= fuel_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "Fuel Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


ets_penalty %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  ggplot() +
  geom_line(aes(x = year, y = ets_penalty, color = scen))

emissions %>%
  filter(str_detect(scen, 'tech')) %>%
  ggplot(aes(x = year, y = emissions, color = scen)) + 
  geom_line()

op_cost_tech <- op_cost %>%
  filter(str_detect(scen, 'tech')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  mutate(scen = sub(".*-\\s*(.*)", "\\1", scen), 
         scen = factor(scen)) %>%
  ggplot(aes(x = year, y = op_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "OPEX Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "none",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


yearly_cost %>%
  filter(str_detect(scen, 'tech')) %>%
  ggplot(aes(x = year, y = yearly_cost, color = scen)) + 
  geom_line()

yearly_cost_tech <- yearly_cost %>%
  filter(str_detect(scen, "tech")) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  mutate(scen = sub(".*-\\s*(.*)", "\\1", scen), 
         scen = factor(scen)) %>%
  ggplot(aes(x = year, y = yearly_cost, color = scen)) +
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "Total Annual Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "none",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

ets_penalty_tech <- ets_penalty %>%
  filter(str_detect(scen, 'tech')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  mutate(scen = sub(".*-\\s*(.*)", "\\1", scen), 
         scen = factor(scen)) %>%
  ggplot(aes(x = year, y = ets_penalty, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "ETS Penalty (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "none",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

common_theme <- theme(
  axis.text.x = element_text(
    angle = 45,          # Angle the text to prevent overlap
    hjust = 1,           # Align text for better readability
    size = 9             # Adjust text size
  ),
  axis.text.y = element_text(size = 9),
  axis.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  legend.title = element_text(size = 10, face = "bold"),
  plot.margin = margin(5, 5, 5, 5)
)

tech_fuel_cost <- tech_fuel_cost +
  common_theme +
  theme(legend.position = "none")

ets_penalty_tech <- ets_penalty_tech +
  common_theme +
  theme(legend.position = "none")

yearly_cost_tech <- yearly_cost_tech +
  common_theme +
  theme(legend.position = "none")

op_cost_tech <- op_cost_tech +
  common_theme +
  theme(legend.position = "none")

# Create the combined plot with a shared legend
combined_plot <- (tech_fuel_cost + ets_penalty_tech) / (yearly_cost_tech + op_cost_tech) +
  plot_annotation(
    #title = "Annual Technology Cost Trajectories",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = margin(20, 20, 20, 20)
    )
  ) &
  # Add a single legend on the right
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(0, 0, 0, 10)
  )

combined_plot


fuel_cost_me_h2 <- fuel_cost %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= fuel_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(title = "Annual Technology Cost Trajectories",
       subtitle = "Comparison of Different Technology Scenarios",
       x = "Year",
       y = "Annual Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

tech_fuel_cost <- fuel_cost %>%
  filter(str_detect(scen, 'tech')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y= fuel_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "Fuel Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "right",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


ets_penalty %>%
  filter(str_detect(scen, 'meoh') | str_detect(scen, 'h2')) %>%
  ggplot() +
  geom_line(aes(x = year, y = ets_penalty, color = scen))

emissions %>%
  filter(str_detect(scen, 'tech')) %>%
  ggplot(aes(x = year, y = emissions, color = scen)) + 
  geom_line()

op_cost_tech <- op_cost %>%
  filter(str_detect(scen, 'tech')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y = op_cost, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "OPEX Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "none",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


yearly_cost %>%
  filter(str_detect(scen, 'tech')) %>%
  ggplot(aes(x = year, y = yearly_cost, color = scen)) + 
  geom_line()

yearly_cost_tech <- yearly_cost %>%
  filter(str_detect(scen, "tech")) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y = yearly_cost, color = scen)) +
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "Total Annual Cost (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "none",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

ets_penalty_tech <- ets_penalty %>%
  filter(str_detect(scen, 'tech')) %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "bau_fuel_meoh" ~ "MeOH Dominant Fuel Mix", 
                          scen == "bau_fuel_h2" ~ "H2 Dominant Fuel", 
                          scen == "bau_ssp5" ~ "SSP5", 
                          scen == "bau_ssp1" ~ "SSP1", 
                          scen == "tech_ccs" ~ "Tech - CCS", 
                          scen == "tech_hull" ~ "Tech - Hull", 
                          scen == "tech_eng_opt" ~ "Tech - Engine Opt.", 
                          scen == "tech_port_call" ~ "Tech - Port Call", 
                          scen == "tech_route_opt" ~ "Tech - Route Opt.", 
                          scen == "tech_propul" ~ "Tech - Propulsion", 
                          scen == "techcomb" ~ "Tech - Combination", 
                          scen == "fuel_cost_high" ~ "High Fuel Cost", 
                          scen == "fuel_cost_low" ~ "Low Fuel Cost", 
                          scen == "co2_cap_pess" ~ "CO2 Cap Pess.", 
                          scen == "co2_cap_opt" ~ "CO2 Cap Opt.", 
                          scen == "ets_price_no" ~ "No ETS Price", 
                          scen == "fuel_cons_fast" ~ "Fuel Consumption - Fast", 
                          scen == "fuel_cons_slow" ~ "Fuel Consumption - Slow",
                          TRUE ~ NA)) %>%
  ggplot(aes(x = year, y = ets_penalty, color = scen)) + 
  # Add lines with appropriate thickness
  geom_line(linewidth = 1) +
  
  # Customize colors using a colorblind-friendly palette
  scale_color_brewer(palette = "Set2") +
  
  # Format y-axis to show dollars with commas
  scale_y_continuous(labels = label_dollar(scale = 1,
                                           prefix = "$",
                                           big.mark = ","),
                     expand = expansion(mult = c(0.05, 0.15))) +
  
  # Format x-axis
  scale_x_continuous(breaks = seq(min(yearly_cost$year),
                                  max(yearly_cost$year),
                                  by = 2),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  # Add labels
  labs(x = "Year",
       y = "ETS Penalty (Million EUR)",
       color = "Technology\nScenario") +
  
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
    legend.position = "none",
    legend.margin = margin(t = 20, l = 20),
    
    # Plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

common_theme <- theme(
  axis.text.x = element_text(
    angle = 45,          # Angle the text to prevent overlap
    hjust = 1,           # Align text for better readability
    size = 9             # Adjust text size
  ),
  axis.text.y = element_text(size = 9),
  axis.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  legend.title = element_text(size = 10, face = "bold"),
  plot.margin = margin(5, 5, 5, 5)
)



# Boxplots ----------------------------------------------------------------

tot_cost_box <- yearly_cost %>%
  mutate(lab = case_when(str_detect(scen, 'ssp') | scen == "base" ~ "Shipping Demand", 
                         str_detect(scen, 'fuel_cost') ~ "Fuel Cost", 
                         #str_detect(scen, 'co2') ~ "CO2 Cost", 
                         str_detect(scen, 'ets') ~ "ETS", 
                         str_detect(scen, 'fuel_cons') ~ "Fuel Consumption", 
                         TRUE ~ NA)) %>% 
  filter(!is.na(lab), 
         year >= 2025) %>%
  group_by(scen, lab) %>%
  summarize(cost = sum(yearly_cost, na.rm = T)) %>% 
  ungroup()


  
  # Calculate base scenario mean
  base_tot <- tot_cost_box$cost[tot_cost_box$scen == 'base']
  
  # Calculate percent changes
  scenario_pct_change <- tot_cost_box %>%
    mutate(pct_change = ((cost - base_tot) / base_tot) * 100)
  
  # Create the plot
  p <- ggplot(tot_cost_box, aes(x = lab, y = cost)) +
    # Enhanced boxplot with better colors and design
    geom_boxplot(
      fill = "#2C85B2",  # Deeper blue that looks more professional
      alpha = 0.8,
      color = "#1B3147",  # Darker outline
      width = 0.7,        # Slightly thinner boxes
      outlier.color = "#1B3147",
      outlier.alpha = 0.7
    ) +
    # Improved text annotations with better positioning
    geom_text(
      data = scenario_pct_change,
      aes(label = ifelse(scen != "base",
                         sprintf("%+.1f%%", pct_change),
                         "Baseline")),
      size = 3.5,
      vjust = -0.8,      # Move text up a bit
      fontface = "bold"
    ) +
    # Enhanced theme with better typography and spacing
    theme_minimal() +
    theme(
      # Title and subtitle formatting
      plot.title = element_text(
        size = 16, 
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "gray30",
        margin = margin(b = 20)
      ),
      # Axis formatting
      axis.title = element_text(
        size = 12,
        face = "bold",
        color = "gray20"
      ),
      axis.text = element_text(
        size = 10,
        color = "gray40"
      ),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        margin = margin(t = 5)
      ),
      # Grid formatting
      panel.grid.major = element_line(
        color = "gray90",
        linewidth = 0.3
      ),
      panel.grid.minor = element_line(
        color = "gray95",
        linewidth = 0.15
      ),
      # Add some padding
      plot.margin = margin(30, 30, 30, 30)
    ) +
    # Format y-axis with commas
    scale_y_continuous(
      labels = scales::dollar_format(prefix = "€"),
      expand = expansion(mult = c(0.1, 0.2))  # Add some space for labels
    ) +
    # Better labels
    labs(subtitle = "Percentage changes shown relative to base scenario",
         y = expression("Total Cost (millions)",), 
         x = ""# Proper subscript formatting
    )


p

emissions_box <- emissions %>%
  mutate(lab = case_when(str_detect(scen, 'ssp') | scen == "base" ~ "Shipping Demand", 
                        # str_detect(scen, 'fuel_cost') ~ "Fuel Cost", 
                         #str_detect(scen, 'co2') ~ "CO2 Cost", 
                        # str_detect(scen, 'ets') ~ "ETS", 
                         str_detect(scen, 'fuel_cons') ~ "Fuel Consumption", 
                         TRUE ~ NA)) %>% 
  filter(!is.na(lab), 
         year >= 2025) %>%
  group_by(scen, lab) %>%
  summarize(emissions = sum(emissions, na.rm = T)) %>% 
  ungroup()

# Calculate base scenario mean
base_tot <- emissions_box$emissions[emissions_box$scen == 'base']

# Calculate percent changes
scenario_pct_change <- emissions_box %>%
  mutate(pct_change = ((emissions - base_tot) / base_tot) * 100)

# Create the plot
p_emissions <- ggplot(emissions_box, aes(x = lab, y = emissions)) +
  # Enhanced boxplot with better colors and design
  geom_boxplot(
    fill = "#2C85B2",  # Deeper blue that looks more professional
    alpha = 0.8,
    color = "#1B3147",  # Darker outline
    width = 0.7,        # Slightly thinner boxes
    outlier.color = "#1B3147",
    outlier.alpha = 0.7
  ) +
  # Improved text annotations with better positioning
  geom_text(
    data = scenario_pct_change,
    aes(label = ifelse(scen != "base",
                       sprintf("%+.1f%%", pct_change),
                       "Baseline")),
    size = 3.5,
    vjust = -0.8,      # Move text up a bit
    fontface = "bold"
  ) +
  # Enhanced theme with better typography and spacing
  theme_minimal() +
  theme(
    # Title and subtitle formatting
    plot.title = element_text(
      size = 16, 
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray30",
      margin = margin(b = 20)
    ),
    # Axis formatting
    axis.title = element_text(
      size = 12,
      face = "bold",
      color = "gray20"
    ),
    axis.text = element_text(
      size = 10,
      color = "gray40"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      margin = margin(t = 5)
    ),
    # Grid formatting
    panel.grid.major = element_line(
      color = "gray90",
      linewidth = 0.3
    ),
    panel.grid.minor = element_line(
      color = "gray95",
      linewidth = 0.15
    ),
    # Add some padding
    plot.margin = margin(30, 30, 30, 30)
  ) +
  # Format y-axis with commas
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.1, 0.2))  # Add some space for labels
  ) +
  # Better. labels
  labs( x = "Parameter",
    y = expression("CO"[2]~"Emissions (MT)")  # Proper subscript formatting
  )


p_emissions


# Combine the plots vertically
combined_plot <- plot_grid(
  p, p_emissions, 
  ncol = 1, 
  align = 'v',
  label_size = 12
)

# Add a common title
title <- ggdraw() 

# Add common subtitle
subtitle <- ggdraw()
# + 
#   draw_label(
#    # "Percentage changes shown relative to base scenario",
#     fontface = 'plain',
#     color = "gray30",
#     size = 11,
#     x = 0.5,
#     y = 0.5
#   )

# Combine title, subtitle, and plots
final_plot <- plot_grid(
  title,
  subtitle,
  combined_plot,
  ncol = 1,
  rel_heights = c(0.005, 0.005, 1)
)

# Display the combined plot
final_plot



# Tech Emissions Chart ----------------------------------------------------

emissions_tech <- emissions %>%
  filter(year >= 2025) %>%
  filter(str_detect(scen, 'tech') | str_detect(scen, 'base')) %>%
  group_by(scen) %>%
  summarize(avg_emissions = mean(emissions)) %>%
  arrange(-avg_emissions) %>%
  select(scen) %>%
  left_join(emissions %>%
              filter(year >= 2025), by = "scen") %>%
  mutate(scen = case_when(scen == "base" ~ "Base", 
                          scen == "tech_port_call" ~ "Port Call", 
                          scen == "tech_route_opt" ~ "Route Optimization", 
                          scen == "tech_hull" ~ "Hull Cleaning", 
                          scen == "tech_eng_opt" ~ "Engine Optimization", 
                          scen == "tech_propul" ~ "Propulsion", 
                          scen == "techcomb" ~ "Combined Technology Scenario", 
                          scen == "tech_ccs" ~ "CCS"))

# Convert scenario to factor to preserve the ordering
emissions_tech$scen <- factor(emissions_tech$scen, levels = unique(emissions_tech$scen))

# Create the visualization with ordered filling
emissions_plot <- function(emissions_tech) {
  # Make sure scen is a factor with proper ordering
  if (!is.factor(emissions_tech$scen)) {
    # Get the unique scenarios ordered by their average emissions
    scen_order <- emissions_tech %>%
      group_by(scen) %>%
      summarize(avg_emissions = mean(emissions, na.rm = TRUE)) %>%
      arrange(desc(avg_emissions)) %>%
      pull(scen)
    
    # Convert to factor with explicit order
    emissions_tech$scen <- factor(emissions_tech$scen, levels = scen_order)
  }
  
  # Create a dataset for ribbons by properly pairing scenarios
  ribbon_data <- emissions_tech %>%
    filter(year >= 2025) %>%
    arrange(year, desc(scen)) %>%
    group_by(year) %>%
    mutate(
      next_scen = lead(scen),
      next_emissions = lead(emissions),
      fill_group = paste(scen, "to", lead(scen))
    ) %>%
    filter(!is.na(next_scen)) %>%
    ungroup()
  
  # Distinct colors for different technologies
  # Using highly distinguishable colors that work well together
  distinct_colors <- c(
    "#E41A1C",  # Red
             "#377EB8",  # Blue
             "#4DAF4A",  # Green
             "#984EA3",  # Purple
             "#FF7F00",  # Orange
             "#FFFF33",  # Yellow
             "#A65628",  # Brown
             "#F781BF",  # Pink
             "#999999"   # Grey
  )
  
  distinct_colors1 <- c(
    'black',
    "#E41A1C",  # Red
             "#377EB8",  # Blue
             "#4DAF4A",  # Green
             "#984EA3",  # Purple
             "#FF7F00",  # Orange
             "#FFFF33",  # Yellow
             "#A65628",  # Brown
             "#F781BF",  # Pink
             "#999999"   # Grey
  )
  
  # Ensure we have enough colors
  n_scenarios <- length(levels(emissions_tech$scen))
  if(n_scenarios > length(distinct_colors)) {
    # Add more colors if needed
    more_colors <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462")
    distinct_colors <- c(distinct_colors, more_colors)
    # If still not enough, use a color generation function
    if(n_scenarios > length(distinct_colors)) {
      distinct_colors <- colorRampPalette(distinct_colors)(n_scenarios)
    }
  }
  
  # Make sure we only use as many colors as we have scenarios
  distinct_colors <- distinct_colors[1:n_scenarios]
  
  # Create darker versions for the lines
  darken_color <- function(color, factor = 0.7) {
    rgb_col <- col2rgb(color)
    darker <- rgb_col * factor / 255
    return(rgb(darker[1,], darker[2,], darker[3,]))
  }
  
  line_colors <- sapply(distinct_colors, darken_color)
  
  # Create the improved plot
  ggplot() +
    # Add ribbons first (so lines appear on top)
    geom_ribbon(data = ribbon_data,
                aes(x = year, 
                    ymin = next_emissions, 
                    ymax = emissions,
                    fill = scen),
                alpha = 0.65) +
    
    # Add the lines for all scenarios with increased prominence
    geom_line(data = emissions_tech, 
              aes(x = year, y = emissions, color = scen), 
              linewidth = 1.5) +
    
    # Apply our distinct color schemes
    scale_fill_manual(values = distinct_colors, name = "Reduction from") +
    scale_color_manual(values = distinct_colors1) +
    
    # Improve labels and title
    labs(
    #  title = "Cumulative Emissions Reduction by Technology Scenario",
      subtitle = "Shaded areas represent emissions reductions between scenarios",
      x = "Year",
      y = "Emissions (MT CO₂e)",
      caption = "Each colored area shows the reduction benefit of each technology scenario"
    ) +
    
    # Enhanced theme
    theme_minimal() +
    theme(
      text = element_text(family = "sans", size = 12),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_line(color = "gray90"),
      axis.title = element_text(face = "bold"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ) +
    
    # Add proper x-axis breaks
    scale_x_continuous(breaks = seq(min(emissions_tech$year), 
                                    max(emissions_tech$year), 
                                    by = 5)) +
    
    # Add smooth transition between years
    coord_cartesian(expand = FALSE)
}


emissions_plot(emissions_tech = emissions_tech)
