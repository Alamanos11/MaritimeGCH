library(ggplot2)
library(readxl)
library(tidyverse)
library(ggthemes)
library(xlsx)
library(viridis)
library(scales)
library(patchwork)


# -------------------------------------------------------------------------

results <- read_excel('../../../MaritimeGCH/maritime_results_all_scenarios.xlsx', 
                      sheet = 'base')


# Total Cost --------------------------------------------------------------


total_cost <- data.frame()
yearly_cost <- data.frame()
emissions <- data.frame()
fuel_cost <- data.frame()
op_cost <- data.frame()
ets_penalty <- data.frame()


for (i in 1:length(sheets)){


  scen = sheets[i]
  
  results <- read_excel('../../../MaritimeGCH/maritime_results_all_scenarios.xlsx', 
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
    title = "Annual Technology Cost Trajectories",
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

sheets <- excel_sheets('../../../MaritimeGCH/maritime_results_all_scenarios.xlsx')
