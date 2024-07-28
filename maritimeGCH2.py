
"""
Created on July 2024

@author: Angelos, Jorge
"""

import os
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from pulp import LpProblem, LpMinimize, LpVariable, lpSum, LpStatus

# Set the working directory
working_directory = 'D:/MaritimeGCH/mymodel4'
os.chdir(working_directory)

# Read input data from CSV files
def getParameters():
    params = {
        "years": range(2020, 2051),  # Planning horizon
        "ship_types": ["C", "T", "B", "G", "O"],  # container, tanker, bulk, cargo, other
        "engine_types": ["ME-C", "ME-GI", "ME-LGI"],
        "init_capacity_fleet": (
            pd.read_csv("init_capacity_fleet.csv", index_col="ship_type")["capacity"].to_dict()
        ),
        "fleet_age": (
            pd.read_csv("init_age.csv", index_col="ship_type")["avr_age"].to_dict()
        ),
        "demand_shipping": (
            pd.read_csv("demand_shipping.csv")
            .set_index(["year", "ship_type"])["demand"]
            .to_dict()
        ),
        "investment_cost": (
            pd.read_csv("investment_cost.csv", index_col="ship_type")["cost"].to_dict()
        ),
        "op_cost": (
            pd.read_csv("op_cost.csv", index_col="ship_type")["cost"].to_dict()
        ),
        "fuel_cost": (
            pd.read_csv("fuel_cost.csv", index_col="fuel_type")["cost"].to_dict()
        ),
        "co2_cap": pd.read_csv("co2_cap.csv", index_col="year")["cap"].to_dict(),
        "ets_price": pd.read_csv("ets_price.csv", index_col="year")["price"].to_dict(),
        "emissions_factor": (
            pd.read_csv("emissions_factor.csv", index_col="fuel_type")["factor"].to_dict()
        ),
        "prod_capacity": (
            pd.read_csv("prod_capacity.csv")
            .set_index(["year", "ship_type"])["capacity"]
            .to_dict()
        ),
        "lifetime": (
            pd.read_csv("lifetime.csv", index_col="ship_type")["years"].to_dict()
        ),
        "fuel_consumption": (
            pd.read_csv("fuel_consumption.csv")
            .set_index(["ship_type", "fuel_type", "engine_type"])["consumption"]
            .to_dict()
        ),
        "fuel_avail": (
            pd.read_csv("fuel_avail.csv")
            .set_index(["fuel_type", "year"])["availability"]
            .to_dict()
        ),
        "cap": pd.read_csv("cap.csv", index_col="ship_type")["capacity"].to_dict(),
        "CII_desired": (
            pd.read_csv("CII_desired.csv", index_col="ship_type")["CII"].to_dict()
        ),
    }
    params["fuel_types"] = list(params["fuel_cost"].keys())
    return params


def printInputParams(params):
    for k, v in params.items():
        print(k)
        if type(v) == dict:
            for l, m in v.items():
                print(l, end="\t")
                print(m)
            print("")
        else:
            print(v, end="\n\n")


def createAndSolveModel(params):
    # Create the LP problem instance
    model = LpProblem(name="MaritimeGCH2", sense=LpMinimize)

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



##### Objective Function: Minimize total cost
    model += lpSum(
        new_ship[y, s] * params["investment_cost"].get(s, 0)
        + stock_ship[y, s] * params["op_cost"].get(s, 0)
        + fuel_demand[y, f] * params["fuel_cost"].get(f, 0)
        for y in params["years"]
        for s in params["ship_types"]
        for f in params["fuel_types"]
    ) + lpSum(
        excess_emissions[y] * params["ets_price"].get(y, 0)
        for y in params["years"]
    )


        ##### Constraints

    # Fleet Capacity Constraint
    for y in params["years"]:
        for s in params["ship_types"]:
            model += (
                stock_ship[y, s] * params["cap"].get(s, 0) >= params["demand_shipping"].get((y, s), 0)
            )

    # Ship Production Constraint
    for y in params["years"]:
        for s in params["ship_types"]:
            model += new_ship[y, s] <= params["prod_capacity"].get((y, s), 0)



    # Fleet Stock Update Constraint
    for y in params["years"]:
        for s in params["ship_types"]:
            if y == 2020:
                model += stock_ship[y, s] == params["init_capacity_fleet"].get(s, 0)
            else:
                # Calculate the number of ships that will be retired this year
                retired_ships = lpSum(
                    new_ship[max(2020, y - params["lifetime"].get(s, 1) + 1 - params["fleet_age"].get(s, 0)), s]
                    for y_prev in range(max(2020, y - params["lifetime"].get(s, 1) + 1), y)
                )
                
                model += stock_ship[y, s] == stock_ship[y-1, s] + new_ship[y, s] - retired_ships
                
                

    # Fuel Demand and Availability Constraints # Fuel consumption was divided by 10000 to get the units right, for this example.
    for y in params["years"]:
        for f in params["fuel_types"]:
            model += fuel_demand[y, f] == lpSum(
                [
                    stock_ship[y, s]
                    * params["fuel_consumption"].get((s, f, eng), 0)
                    * 1e-4
                    for s in params["ship_types"]
                    for eng in params["engine_types"]
                ]
            )

            model += fuel_demand[y, f] <= params["fuel_avail"].get((f, y), 0)

    # Emissions Constraint
    for y in params["years"]:
        model += co2_emissions[y] == lpSum(
            fuel_demand[y, f] * params["emissions_factor"].get(f, 0)
            for f in params["fuel_types"]
        )

    # ETS Emissions Cap (threshold) Constraint, plus any excess emissions (which will be then purchased/penalized):
    for y in params["years"]:
        model += co2_emissions[y] <= params["co2_cap"].get(y, 0) + excess_emissions[y]

    # Carbon Intensity Indicator Constraint
    for y in params["years"]:
        for s in params["ship_types"]:
            model += co2_emissions[y] <= params["cap"].get(s, 1) * params["CII_desired"].get(s, 1)



    # Solve the model
    model.solve()


    # Check the status of the solution
    if LpStatus[model.status] == "Optimal":
        # Print the total cost and other results
        print(f"Total Cost : {model.objective.value()}", end="\n\nOPTIMAL VALUES:\n\n")
        # Print other relevant results as needed
        for v in model.variables():
            print(v, "\t", v.value())
    else:
        print("No optimal solution found.")
    return model


# Extract the results into an excel file, showing all the cost components
def extract_results(model, params):
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
            results['Fuel_Cost'][year_index] += v.varValue * params['fuel_cost'].get(fuel_type, 0)
        elif name_parts[0] == 'excess':
            year = int(name_parts[2])
            year_index = years.index(year)
            results['excess_emissions'][year_index] = v.varValue



    # Calculate the penalized (ETS-taxed) excess CO2 emissions
    for i, year in enumerate(years):
        results['ets_penalty'][i] = results['excess_emissions'][i] * params['ets_price'].get(year, 0)

    return pd.DataFrame(results)


# Save the results in an Excel file
def save_results_to_excel(df):
    df.to_excel('maritime_results.xlsx', index=False)
    print("Results saved to 'maritime_results.xlsx'")


# Create and save plots with the results
def create_plots(df, params):
    years = df['Year']

    # Set general plot style
    plt.rcParams['font.weight'] = 'bold'
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titleweight'] = 'bold'



    # Costs Plots
    cost_components = ['Investment_Cost', 'Operational_Cost', 'Fuel_Cost', 'ets_penalty']
    cost_components.append('Total_Cost')  # Add total cost to the list

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

       plt.title('CO2 Emissions and Cap [tonnes]', fontweight='bold')
       plt.xlabel('Year', fontweight='bold')
       plt.ylabel('CO2 Emissions', fontweight='bold')
       plt.legend()
       plt.grid(True, linestyle='--', alpha=0.7)
       plt.tight_layout()
       plt.savefig('co2_emissions_over_years.png')
       plt.close()

    
        

    # Fuel Demand
    plt.figure(figsize=(12, 6))
    for f in params['fuel_types']:
        plt.plot(years, df[f'Fuel_Demand_{f}'], label=f)
    plt.title('Fuel Demand [tonnes]', fontweight='bold')
    plt.xlabel('Year', fontweight='bold')
    plt.ylabel('Fuel Demand', fontweight='bold')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig('fuel_demand.png')
    plt.close()

    # New Ships
    plt.figure(figsize=(12, 6))
    for s in params['ship_types']:
        plt.plot(years, df[f'New_Ships_{s}'], label=s)
    plt.title('New Ships [number]', fontweight='bold')
    plt.xlabel('Year', fontweight='bold')
    plt.ylabel('Number of New Ships', fontweight='bold')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig('new_ships.png')
    plt.close()

    # Stock Ships
    plt.figure(figsize=(12, 6))
    for s in params['ship_types']:
        plt.plot(years, df[f'Stock_Ships_{s}'], label=s)
    plt.title('Stock Ships [number]', fontweight='bold')
    plt.xlabel('Year', fontweight='bold')
    plt.ylabel('Number of Stock Ships', fontweight='bold')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig('stock_ships.png')
    plt.close()


    print("Plots saved as PNG files in the working directory")

if __name__ == "__main__":
    try:
        p = getParameters()
        printInputParams(p)
        m = createAndSolveModel(p)
        m.writeLP("./maritimeLP.txt")
        
        # Debug: Print some sample variable names
        print("Sample variable names:")
        for v in list(m.variables())[:10]:  # Print first 10 variable names
            print(v.name)
        
        # Extract results
        results_df = extract_results(m, p)
        
        # Save results to Excel
        save_results_to_excel(results_df)
        
        # Create plots
        create_plots(results_df, p)
        
        print("Script executed successfully. Excel file and plots have been created.")
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        print("Error details:")
        import traceback
        traceback.print_exc()
        print("Excel file and plots may not have been created due to the error.")
        
        
################  combined plots  

import matplotlib.pyplot as plt

def create_combined_figure(df, params):
    years = df['Year']

    fig, axes = plt.subplots(4, 2, figsize=(20, 20))  # Create a 4x2 grid of subplots

    # Set general plot style
    plt.rcParams['font.size'] = 14
    plt.rcParams['font.weight'] = 'bold'
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titleweight'] = 'bold'

    # Stock Ships
    for s in params['ship_types']:
        axes[0, 0].plot(years, df[f'Stock_Ships_{s}'], label=s)
    axes[0, 0].set_title('Stock Ships [number]', fontweight='bold')
    axes[0, 0].set_xlabel('Year', fontweight='bold')
    axes[0, 0].set_ylabel('Number of Stock Ships', fontweight='bold')
    axes[0, 0].legend()
    axes[0, 0].grid(True, linestyle='--', alpha=0.7)

    # New Ships
    for s in params['ship_types']:
        axes[0, 1].plot(years, df[f'New_Ships_{s}'], label=s)
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
    for f in params['fuel_types']:
        axes[2, 0].plot(years, df[f'Fuel_Demand_{f}'], label=f)
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
    axes[3, 0].set_title('CO2 Emissions and Cap [tonnes]', fontweight='bold')
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
    plt.savefig('combined_figure.png')
    plt.close()

    print("Combined figure saved as 'combined_figure.png' in the working directory")

if __name__ == "__main__":
    try:
        p = getParameters()
        printInputParams(p)
        m = createAndSolveModel(p)
        m.writeLP("./maritimeLP.txt")
        
        # Debug: Print some sample variable names
        print("Sample variable names:")
        for v in list(m.variables())[:10]:  # Print first 10 variable names
            print(v.name)
        
        # Extract results
        results_df = extract_results(m, p)
        
        # Save results to Excel
        save_results_to_excel(results_df)
        
        # Create plots
        create_plots(results_df, p)
        
        # Create and save the combined figure
        create_combined_figure(results_df, p)
        
        print("Script executed successfully. Excel file and plots have been created.")
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        print("Error details:")
        import traceback
        traceback.print_exc()
        print("Excel file and plots may not have been created due to the error.")




