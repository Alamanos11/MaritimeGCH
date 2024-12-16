# -*- coding: utf-8 -*-
"""
Created on Fri Nov 15 13:06:45 2024

@author: Chris Deranian
"""

# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 11:28:57 2024

@author: Chris Deranian
"""

import os
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from pulp import LpProblem, LpMinimize, LpVariable, lpSum, LpStatus
import traceback

class MaritimeScenarioAnalysis:
    def __init__(self, working_directory):
        self.working_directory = working_directory
        os.chdir(working_directory)
        
    def getParameters(self, scenario='base'):
        
        # Scenario-specific parameters
        scenario_files = {
            'low': {
                'init_capacity_fleet' : "init_capacity_fleet.csv", 
                'minim_capacity_fleet' : "minim_capacity_fleet.csv", 
                'fleet_age' : "init_age.csv", 
                'demand_shipping': "demand_shippingSSP1.csv", 
                'investment_cost' : "investment_cost.csv", 
                'op_cost' : "op_cost.csv",
                'emissions_factor' : "emissions_factor.csv",
                'prod_capacity' : "prod_capacity.csv", 
                'lifetime' : "lifetime.csv",
                'cap' : "cap.csv",
                'CII_desired' : "CII_desired.csv",
                'fuel_cost': "fuel_cost_low.csv",
                'ets_price': "ets_price_no.csv",
                'co2_cap': "co2_cap_opt.csv",
                'fuel_avail': "fuel_avail_no.csv",
                'fuel_consumption': "fuel_consumption_med.csv"

            },
            'base': {
                'init_capacity_fleet' : "init_capacity_fleet.csv", 
                'minim_capacity_fleet' : "minim_capacity_fleet.csv", 
                'fleet_age' : "init_age.csv", 
                'demand_shipping': "demand_shippingSSP5.csv", 
                'investment_cost' : "investment_cost.csv", 
                'op_cost' : "op_cost.csv",
                'emissions_factor' : "emissions_factor.csv",
                'prod_capacity' : "prod_capacity.csv", 
                'lifetime' : "lifetime.csv",
                'cap' : "cap.csv",
                'CII_desired' : "CII_desired.csv",
                'fuel_cost': "fuel_cost_low.csv",
                'ets_price': "ets_price_mod.csv",
                'co2_cap': "co2_cap_opt.csv",
                'fuel_avail': "fuel_avail_no.csv",
                'fuel_consumption': "fuel_consumption_med.csv"
            },
            'high': {
                'init_capacity_fleet' : "init_capacity_fleet.csv", 
                'minim_capacity_fleet' : "minim_capacity_fleet.csv", 
                'fleet_age' : "init_age.csv", 
                'demand_shipping': "demand_shippingSSP5.csv", 
                'investment_cost' : "investment_cost.csv", 
                'op_cost' : "op_cost.csv",
                'emissions_factor' : "emissions_factor.csv",
                'prod_capacity' : "prod_capacity.csv", 
                'lifetime' : "lifetime.csv",
                'cap' : "cap.csv",
                'CII_desired' : "CII_desired.csv",
                'fuel_cost': "fuel_cost_low.csv",
                'ets_price': "ets_price_mod.csv",
                'co2_cap': "co2_cap_opt.csv",
                'fuel_avail': "fuel_avail_no.csv",
                'fuel_consumption': "fuel_consumption_med.csv"
            }
        }
        
        # Load scenario-specific files
        files = scenario_files[scenario]
        params = {
            "years": range(2020, 2051),
            "ship_types": ["C", "T", "B", "G", "O"],
            "engine_types": ["ME-C", "ME-GI", "ME-LGI"],
            "init_capacity_fleet": (
                pd.read_csv(files['init_capacity_fleet'], index_col="ship_type")["capacity"].to_dict()
            ),
            "minim_capacity_fleet": (
                pd.read_csv(files['minim_capacity_fleet'], index_col="ship_type")["limit"].to_dict()
            ),
            "fleet_age": (
                pd.read_csv(files['fleet_age'], index_col="ship_type")["avr_age"].to_dict()
            ),
            "demand_shipping": (
                pd.read_csv(files['demand_shipping'])
                .set_index(["year", "ship_type"])["demand"]
                .to_dict()
            ),
            "investment_cost": (
                pd.read_csv(files['investment_cost'], index_col="ship_type")["cost"].to_dict()
            ),
            "op_cost": (
                pd.read_csv(files['op_cost'], index_col="ship_type")["cost"].to_dict()
            ),
            "emissions_factor": (
                pd.read_csv(files['emissions_factor'], index_col="fuel_type")["factor"].to_dict()
            ),
            "prod_capacity": (
                pd.read_csv(files['prod_capacity'])
                .set_index(["year", "ship_type"])["capacity"]
                .to_dict()
            ),
            "lifetime": (
                pd.read_csv(files['lifetime'], index_col="ship_type")["years"].to_dict()
            ),
            "cap": pd.read_csv(files['cap'], index_col="ship_type")["capacity"].to_dict(),
            "CII_desired": (
                pd.read_csv(files['CII_desired'], index_col="ship_type")["CII"].to_dict()
            ),
            "fuel_cost": pd.read_csv(files['fuel_cost'])
            .set_index(["fuel_type", "year"])["cost"]
            .to_dict(),
            "ets_price": pd.read_csv(files['ets_price'], index_col="year")["price"].to_dict(),
            "co2_cap": pd.read_csv(files['co2_cap'], index_col="year")["cap"].to_dict(),
            "fuel_avail": (
                pd.read_csv(files['fuel_avail'])
                .set_index(["fuel_type", "year"])["availability"]
                .to_dict()
            ),
            "fuel_consumption": (
                pd.read_csv(files['fuel_consumption'])
                .set_index(["ship_type", "fuel_type", "engine_type", "year"])["consumption"]
                .to_dict()
            )
        }
        
        params["fuel_types"] = list( pd.read_csv(files['fuel_cost'])["fuel_type"].unique())
        return params
    
        

    def createAndSolveModel(self, params):
        model = LpProblem(name="MaritimeGCHgr", sense=LpMinimize)

        # Decision Variables
        new_ship = {
            (y, s): LpVariable(name=f"new_ship_{y}_{s}", lowBound=0, cat="Integer")
            for y in params["years"]
            for s in params["ship_types"]
        }
        stock_ship = {
            (y, s): LpVariable(name=f"stock_ship_{y}_{s}", lowBound=0, cat="Integer")
            for y in params["years"]
            for s in params["ship_types"]
        }
        fuel_demand = {
            (y, f): LpVariable(name=f"fuel_demand_{y}_{f}", lowBound=0)
            for y in params["years"]
            for f in params["fuel_types"]
        }
        co2_emissions = {
            y: LpVariable(name=f"co2_emissions_{y}", lowBound=0) for y in params["years"]
        }
        excess_emissions = {
            y: LpVariable(name=f"excess_emissions_{y}", lowBound=0)
            for y in params["years"]
        }

        # Objective Function
        model += lpSum(
            new_ship[y, s] * params["investment_cost"].get(s, 0)
            + stock_ship[y, s] * params["op_cost"].get(s, 0)
            + fuel_demand[y, f] * params["fuel_cost"].get(f, y)
            for y in params["years"]
            for s in params["ship_types"]
            for f in params["fuel_types"]
        ) + lpSum(
            excess_emissions[y] * params["ets_price"].get(y, 0)
            for y in params["years"]
        )
            
        ##### Constraints

    # Fleet Capacity Constraint ### Here the demand is in million GtNM (I tried to change the units, but the problem gets infeasible..)
        for y in params["years"]:
            for s in params["ship_types"]:
                model += (
                    stock_ship[y, s] * params["cap"].get(s, 0) >= params["demand_shipping"].get((y, s), 0)
                )
        
        for y in params["years"]:
            for s in params["ship_types"]:
                model += new_ship[y, s] <= params["prod_capacity"].get((y, s), 0)

        for y in params["years"]:
            for s in params["ship_types"]:
                if y == 2020:
                    model += stock_ship[y, s] == params["init_capacity_fleet"].get(s, 0)
                else:
                    retired_ships = lpSum(
                        new_ship[max(2020, y - params["lifetime"].get(s, 1) + 1 - params["fleet_age"].get(s, 0)), s]
                        for y_prev in range(max(2020, y - params["lifetime"].get(s, 1) + 1), y)
                    )
                    model += stock_ship[y, s] == stock_ship[y-1, s] + new_ship[y, s] - retired_ships

        for y in params["years"]:
            for f in params["fuel_types"]:
                fuel_demand_value = lpSum(
                    stock_ship[y, s]
                    * params["fuel_consumption"].get((s, f, eng, y), 0)
                    * 1e-2
                    for s in params["ship_types"]
                    for eng in params["engine_types"]
                )
                model += fuel_demand[y, f] == fuel_demand_value
                model += fuel_demand[y, f] <= params["fuel_avail"].get((f, y), 0)

        for y in params["years"]:
            model += co2_emissions[y] == lpSum(
                fuel_demand[y, f] * params["emissions_factor"].get(f, 0) * 10e-6
                for f in params["fuel_types"]
            )

        for y in params["years"]:
            model += co2_emissions[y] <= params["co2_cap"].get(y, 0) + excess_emissions[y]

        for y in params["years"]:
            for s in params["ship_types"]:
                model += co2_emissions[y] <= params["cap"].get(s, 1) * params["CII_desired"].get(s, 1)

        # Solve the model
        model.solve()

        if LpStatus[model.status] == "Optimal":
            print(f"Total Cost : {model.objective.value()}", end="\n\nOPTIMAL VALUES:\n\n")
        else:
            print("No optimal solution found.")
        return model

    def extract_results(self, model, params):
        variables = model.variables()
        years = list(params['years'])
        results = {
            'Year': years,
            'CO2_Emissions': [0 for _ in years],
            'Total_Cost': [model.objective.value() for _ in years],
            'Investment_Cost': [0 for _ in years],
            'Operational_Cost': [0 for _ in years],
            'Fuel_Cost': [0 for _ in years],
            'excess_emissions': [0 for _ in years],
            'ets_penalty': [0 for _ in years],
        }
        
        for s in params['ship_types']:
            results[f'New_Ships_{s}'] = [0 for _ in years]
            results[f'Stock_Ships_{s}'] = [0 for _ in years]
        
        for f in params['fuel_types']:
            results[f'Fuel_Demand_{f}'] = [0 for _ in years]

        for v in variables:
            name_parts = v.name.split('_')
            if name_parts[0] == 'co2':
                year = int(name_parts[2])
                year_index = years.index(year)
                results['CO2_Emissions'][year_index] = v.varValue
            elif name_parts[0] == 'new' or name_parts[0] == 'stock':
                year = int(name_parts[2])
                year_index = years.index(year)
                ship_type = name_parts[3]
                if name_parts[0] == 'new':
                    results[f'New_Ships_{ship_type}'][year_index] = v.varValue
                    results['Investment_Cost'][year_index] += v.varValue * params['investment_cost'].get(ship_type, 0)
                else:
                    results[f'Stock_Ships_{ship_type}'][year_index] = v.varValue
                    results['Operational_Cost'][year_index] += v.varValue * params['op_cost'].get(ship_type, 0)
            elif name_parts[0] == 'fuel':
                year = int(name_parts[2])
                year_index = years.index(year)
                fuel_type = name_parts[3]
                results[f'Fuel_Demand_{fuel_type}'][year_index] = v.varValue
                results['Fuel_Cost'][year_index] += v.varValue * params['fuel_cost'].get((fuel_type, year), 0)
            elif name_parts[0] == 'excess':
                year = int(name_parts[2])
                year_index = years.index(year)
                results['excess_emissions'][year_index] = v.varValue

        for i, year in enumerate(years):
            results['ets_penalty'][i] = results['excess_emissions'][i] * params['ets_price'].get(year, 0)

        return pd.DataFrame(results)

def detect_scenario_differences(scenario_results):
    """
    Detect which variables differ significantly between scenarios.
    """
    differences = {
        'costs': False,
        'emissions': False,
        'fleet': False,
        'fuel_mix': False,
        'ets_penalty': False,
        'excess_emissions': False
    }
    
    base_data = scenario_results['base']
    scenarios = list(scenario_results.keys())
    
    threshold = 0.05  # 1% difference threshold
    
    for scenario in scenarios:
        if scenario == 'base':
            continue
            
        data = scenario_results[scenario]
        
        # Check cost differences
        cost_metrics = ['Total_Cost', 'Investment_Cost', 'Operational_Cost', 'Fuel_Cost']
        for metric in cost_metrics:
            rel_diff = abs(data[metric] - base_data[metric]).mean() / (base_data[metric].mean() + 1e-10)
            if rel_diff > threshold:
                differences['costs'] = True
                
        # Check emissions differences
        emissions_diff = abs(data['CO2_Emissions'] - base_data['CO2_Emissions']).mean() / (base_data['CO2_Emissions'].mean() + 1e-10)
        if emissions_diff > threshold:
            differences['emissions'] = True
            
        ets_diff = abs(data['ets_penalty'] - base_data['ets_penalty']).mean() / (base_data['ets_penalty'].mean() + 1e-10)
        if ets_diff > threshold:
            differences['ets_penalty'] = True           
            
        excess_emissions = abs(data['excess_emissions'] - base_data['excess_emissions']).mean() / (base_data['excess_emissions'].mean() + 1e-10)
        if excess_emissions > threshold:
            differences['excess_emissions'] = True               
            
        # Check fleet differences
        fleet_columns = [col for col in data.columns if 'Stock_Ships_' in col]
        for col in fleet_columns:
            fleet_diff = abs(data[col] - base_data[col]).mean() / (base_data[col].mean() + 1e-10)
            if fleet_diff > threshold:
                differences['fleet'] = True
                break
                
        # Check fuel mix differences
        fuel_columns = [col for col in data.columns if 'Fuel_Demand_' in col]
        for col in fuel_columns:
            fuel_diff = abs(data[col] - base_data[col]).mean() / (base_data[col].mean() + 1e-10)
            if fuel_diff > threshold:
                differences['fuel_mix'] = True
                break
                
            
    
    return differences

def create_plots(df, params, scenario):
    years = df['Year']

    # Set general plot style
    plt.rcParams['font.weight'] = 'bold'
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titleweight'] = 'bold'



    # Costs Plots
    cost_components = ['Investment_Cost', 'Operational_Cost', 'Fuel_Cost', 'ets_penalty']
    cost_components.append('Total_Cost')

    for component in cost_components:
       plt.figure(figsize=(12, 6))
       plt.plot(years, df[component], marker='o')
       plt.title(f'{component} over Years', fontweight='bold')
       plt.xlabel('Year', fontweight='bold')
       plt.ylabel('Costs [million Euros]', fontweight='bold')
       plt.grid(True, linestyle='--', alpha=0.7)
       plt.tight_layout()
       plt.savefig(f'{component.lower().replace(" ", "_")}_over_years.png')
       plt.close()



       # CO2 Emissions over Years
       plt.figure(figsize=(12, 6))

       # Plot total CO2 emissions
       plt.plot(years, df['CO2_Emissions'], label='Total CO2 Emissions', color='blue')

       # Plot CO2 cap
       plt.plot(years, [params['co2_cap'].get(y, 0) for y in years], label='CO2 Cap', color='red', linestyle='--')

       # Fill the area representing excess emissions
       plt.fill_between(years, 
                 [params['co2_cap'].get(y, 0) for y in years],
                 df['CO2_Emissions'],
                 where=(df['CO2_Emissions'] > [params['co2_cap'].get(y, 0) for y in years]),
                 color='red', alpha=0.3, label='Excess Emissions')

       plt.title('CO2 Emissions and Cap [million tonnes]', fontweight='bold')
       plt.xlabel('Year', fontweight='bold')
       plt.ylabel('CO2 Emissions', fontweight='bold')
       plt.legend()
       plt.grid(True, linestyle='--', alpha=0.7)
       plt.tight_layout()
       plt.savefig(f"co2_emissions_over_years_{scenario}.png")
       plt.close()

    
        

    
    # Fuel Demand
    plt.figure(figsize=(12, 6))
    bottom = np.zeros(len(years))
    for f in params['fuel_types']:
        plt.bar(years, df[f'Fuel_Demand_{f}'], bottom=bottom, label=f)
        bottom += df[f'Fuel_Demand_{f}']  # Update bottom to stack next fuel type on top

    # Add title and labels
    plt.title('Fuel Demand [tonnes]', fontweight='bold')
    plt.xlabel('Year', fontweight='bold')
    plt.ylabel('Fuel Demand', fontweight='bold')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig(f"fuel_demand_{scenario}.png")
    plt.close()

    
    # New Ships
    plt.figure(figsize=(12, 6))
    # Initialize the bottom position for stacking
    bottom = np.zeros(len(years))
    for s in params['ship_types']:
        plt.bar(years, df[f'New_Ships_{s}'], bottom=bottom, label=s)
        bottom += df[f'New_Ships_{s}']  
    plt.title('New Ships [number]', fontweight='bold')
    plt.xlabel('Year', fontweight='bold')
    plt.ylabel('Number of New Ships', fontweight='bold')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    # Save and close
    plt.tight_layout()
    plt.savefig(f"new_ships_{scenario}.png")
    plt.close()

    # Stock Ships
    
    plt.figure(figsize=(12, 6))
    bottom = np.zeros(len(years))
    for s in params['ship_types']:
        plt.bar(years, df[f'Stock_Ships_{s}'], bottom=bottom, label=s)
        bottom += df[f'Stock_Ships_{s}']
    plt.title('Stock Ships [number]', fontweight='bold')
    plt.xlabel('Year', fontweight='bold')
    plt.ylabel('Number of Stock Ships', fontweight='bold')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    # Save and close
    plt.tight_layout()
    plt.savefig(f"stock_ships_{scenario}.png")
    plt.close()

    print("Plots saved as PNG files in the working directory")
    
def create_scenario_comparison_plots(scenario_results, scenario_differences):
    """
    Create comparison plots for different scenarios based on detected differences.
    """
    years = scenario_results['base']['Year']
    scenarios = list(scenario_results.keys())
    
    # Map detected differences to specific plot requirements
    plot_mapping = {
        'costs': ['Total_Cost', 'Investment_Cost', 'Operational_Cost', 'Fuel_Cost'],
        'emissions': ['CO2_Emissions'],
        'ets_penalty': ['ets_penalty'],
        'fleet': [col for col in scenario_results['base'].columns if 'Stock_Ships_' in col],
        'fuel_mix': [col for col in scenario_results['base'].columns if 'Fuel_Demand_' in col]
    }
    
    # Filter parameters to plot based on detected differences
    parameters_to_plot = [
        param
        for category, params in plot_mapping.items()
        if scenario_differences[category]
        for param in params
    ]
    
    print(f"Parameters to plot: {parameters_to_plot}")
    
    # Set plot style
    plt.style.use('seaborn-darkgrid') 
    colors = {'low': 'green', 'base': 'blue', 'high': 'purple'}
    
    # Create subplots dynamically
    num_plots = len(parameters_to_plot)
    num_cols = 2
    num_rows = (num_plots + 1) // 2
    fig, axes = plt.subplots(num_rows, num_cols, figsize=(20, 6 * num_rows), squeeze=False)
    
    plot_idx = 0
    
    # Loop through selected parameters and create plots
    for param in parameters_to_plot:
        row = plot_idx // num_cols
        col = plot_idx % num_cols
        
        for scenario in scenarios:
            axes[row, col].plot(
                years, scenario_results[scenario][param],
                label=f'{scenario.capitalize()} Scenario',
                color=colors.get(scenario, 'black'), 
                linewidth = 3# Default to black if color not in dict
            )
        axes[row, col].set_title(f'{param.replace("_", " ").capitalize()} Sensitivity', fontsize=14, fontweight='bold')
        axes[row, col].set_xlabel('Year', fontsize = 14)
        axes[row, col].set_ylabel('Units', fontsize = 14)  # Adjust units dynamically if needed
        axes[row, col].legend(), 
        axes[row, col].grid(True),
        plot_idx += 1
        
        # Remove any unused subplots
        for idx in range(plot_idx, num_rows * num_cols):
            row = idx // num_cols
            col = idx % num_cols
            # fig.delaxes(axes[row, col])
    
    plt.tight_layout()
    plt.savefig('scenario_comparison_dynamic.png', dpi=300, bbox_inches='tight')
    print("Figure created")
    plt.close()
    
def create_combined_figure(df, params, scenario):
    years = df['Year']
    
    fig, axes = plt.subplots(4, 2, figsize=(20, 20))  # Create a 4x2 grid of subplots

    # Set general plot style
    plt.rcParams['font.size'] = 14
    plt.rcParams['font.weight'] = 'bold'
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titleweight'] = 'bold'

    # Stock Ships
    bottom = np.zeros(len(years))
    for s in params['ship_types']:
        axes[0, 0].bar(years, df[f'Stock_Ships_{s}'], bottom=bottom, label=s)
        bottom += df[f'Stock_Ships_{s}']  
    axes[0, 0].set_title('Stock Ships [number]', fontweight='bold')
    axes[0, 0].set_xlabel('Year', fontweight='bold')
    axes[0, 0].set_ylabel('Number of Stock Ships', fontweight='bold')
    axes[0, 0].legend()
    axes[0, 0].grid(True, linestyle='--', alpha=0.7)

    # New Ships
    bottom = np.zeros(len(years))
    for s in params['ship_types']:
        axes[0, 1].bar(years, df[f'New_Ships_{s}'], bottom=bottom, label=s)
        bottom += df[f'New_Ships_{s}']  
    axes[0, 1].set_title('New Ships [number]', fontweight='bold')
    axes[0, 1].set_xlabel('Year', fontweight='bold')
    axes[0, 1].set_ylabel('Number of New Ships', fontweight='bold')
    axes[0, 1].legend()
    axes[0, 1].grid(True, linestyle='--', alpha=0.7)

    # Investment Costs
    axes[1, 0].plot(years, df['Investment_Cost'], marker='o')
    axes[1, 0].set_title('Investment Costs [million Euros]', fontweight='bold')
    axes[1, 0].set_xlabel('Year', fontweight='bold')
    axes[1, 0].set_ylabel('Costs [million Euros]', fontweight='bold')
    axes[1, 0].grid(True, linestyle='--', alpha=0.7)

    # Operational Costs
    axes[1, 1].plot(years, df['Operational_Cost'], marker='o')
    axes[1, 1].set_title('Operational Costs [million Euros]', fontweight='bold')
    axes[1, 1].set_xlabel('Year', fontweight='bold')
    axes[1, 1].set_ylabel('Costs [million Euros]', fontweight='bold')
    axes[1, 1].grid(True, linestyle='--', alpha=0.7)

    # Fuel Demand
    bottom = np.zeros(len(years))
    for f in params['fuel_types']:
        axes[2, 0].bar(years, df[f'Fuel_Demand_{f}'], bottom=bottom, label=f)
        bottom += df[f'Fuel_Demand_{f}']
    axes[2, 0].set_title('Fuel Demand [tonnes]', fontweight='bold')
    axes[2, 0].set_xlabel('Year', fontweight='bold')
    axes[2, 0].set_ylabel('Fuel Demand', fontweight='bold')
    axes[2, 0].legend()
    axes[2, 0].grid(True, linestyle='--', alpha=0.7)

    # Fuel Costs
    axes[2, 1].plot(years, df['Fuel_Cost'], marker='o')
    axes[2, 1].set_title('Fuel Costs [million Euros]', fontweight='bold')
    axes[2, 1].set_xlabel('Year', fontweight='bold')
    axes[2, 1].set_ylabel('Costs [million Euros]', fontweight='bold')
    axes[2, 1].grid(True, linestyle='--', alpha=0.7)

    # CO2 Emissions and Cap
    axes[3, 0].plot(years, df['CO2_Emissions'], label='Total CO2 Emissions', color='blue')
    axes[3, 0].plot(years, [params['co2_cap'].get(y, 0) for y in years], label='CO2 Cap', color='red', linestyle='--')
    axes[3, 0].fill_between(years, 
                            [params['co2_cap'].get(y, 0) for y in years],
                            df['CO2_Emissions'],
                            where=(df['CO2_Emissions'] > [params['co2_cap'].get(y, 0) for y in years]),
                            color='red', alpha=0.3, label='Excess Emissions')
    axes[3, 0].set_title('CO2 Emissions and Cap [million tonnes]', fontweight='bold')
    axes[3, 0].set_xlabel('Year', fontweight='bold')
    axes[3, 0].set_ylabel('CO2 Emissions', fontweight='bold')
    axes[3, 0].legend()
    axes[3, 0].grid(True, linestyle='--', alpha=0.7)

    # ETS Penalty
    axes[3, 1].plot(years, df['ets_penalty'], marker='o')
    axes[3, 1].set_title('ETS Penalty [million Euros]', fontweight='bold')
    axes[3, 1].set_xlabel('Year', fontweight='bold')
    axes[3, 1].set_ylabel('Penalty Costs [million Euros]', fontweight='bold')
    axes[3, 1].grid(True, linestyle='--', alpha=0.7)

    # Adjust layout and save the figure
    plt.tight_layout(pad=4.0)
    plt.savefig(f"combined_figure_{scenario}.png")
    plt.close()

    print("Combined figure saved as 'combined_figure.png' in the working directory")    
        
def main():
    # Initialize scenario analysis
    analysis = MaritimeScenarioAnalysis('C:\\Users\Chris Deranian\OneDrive\Documents\Fulbright\MaritimeGCH')
    
    # Run scenarios
    scenarios = ['low', 'base', 'high']
    scenario_results = {}
    
    for scenario in scenarios:
        print(f"\nRunning {scenario} scenario...")
        params = analysis.getParameters(scenario)
        model = analysis.createAndSolveModel(params)
        results_df = analysis.extract_results(model, params)
        scenario_results[scenario] = results_df
        create_plots(results_df, params, scenario)
        create_combined_figure(results_df, params, scenario)

        # Save individual scenario results
        results_df.to_excel(f'maritime_results_{scenario}.xlsx', index=False)
    
    # Create comparison plots
    scenario_differences = detect_scenario_differences(scenario_results)
    create_scenario_comparison_plots(scenario_results, scenario_differences)

        
        # Create and save the combined figure
    
         
    # Save combined results
    with pd.ExcelWriter('maritime_results_all_scenarios.xlsx') as writer:
        for scenario in scenarios:
            scenario_results[scenario].to_excel(writer, sheet_name=scenario, index=False)
    
    print("\nScenario analysis completed. Results saved to Excel and plots generated.")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        traceback.print_exc()