#!/usr/bin/env python
# coding: utf-8

import numpy as np


# Define the input and output file names
input_file = "../QBO/qbo_data/singapore.dat"
output_file = "singapore.highres.dat"

# Initialize variables to store data
year = None
pressure_levels = []
velocity_data = []
newyear = 0
vel = []

# Open the input and output files
with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
    outfile.write("# QBO.highres.dat\n")
    outfile.write("#\n")
    outfile.write("# This file contains monthly mean zonal wind data at Singapore (48698), 1N/104E.\n")
    outfile.write("#\n")
    outfile.write("# Units: m/s\n")
    outfile.write("#\n")
    outfile.write("# year month      10      12      15      20      25      30      35      40      45      50      60      70      80      90\n")

    for line in infile:
        # Remove leading and trailing whitespace
        line = line.strip()

        if not line:
            # Skip empty lines
            continue

        if line.isdigit():
            year = int(line)

        elif year is not None and not line.startswith('hPa'):
            # Split the line into columns
            columns = line.split()

            # The first column contains the pressure level
            pressure_level = int(columns[0])

            # Extract velocity data and transpose it
            velocity_values = [float(column) / 10.0 for column in columns[1:]]

            # Store the data in the respective lists
            pressure_levels.append(pressure_level)
            if newyear != year:
                if velocity_data:  # Check if there's existing data
                    vel.append(np.array(velocity_data).T)
                velocity_data = []
                velocity_data.append([str(year)] * 12)  # Year repeated 12 times as strings
                velocity_data.append(['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'])
            velocity_data.append(velocity_values)

            newyear = year

    # Append the last year's data after the loop
    if velocity_data:  # Check if there's existing data
        vel.append(np.array(velocity_data).T)

    # Write the 'vel' data as it is to the output file with proper formatting
    for data in vel:
        for i, row in enumerate(data):
            year_val = int(row[0])
            month_val = int(row[1])
            if i % 12 == 0:  # Add a comment symbol every 12 months
                outfile.write("#\n")
            outfile.write(f"{year_val:4d}    {month_val:02d}      ")

            for val in row[2:16]:
                outfile.write(f"{float(val):<8.1f}")  # Format the values as floating-point numbers and left-align
            outfile.write("\n")

print(f"Data has been successfully saved to '{output_file}'.")





