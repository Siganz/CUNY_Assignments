import pandas as pd
import numpy as np
from datetime import datetime, timedelta

# Load real data
real = pd.read_csv(r'C:\Users\ganzs\Documents\CUNY_Assignments\604\Week_3\2018_Central_Park_Squirrel_Census_-_Squirrel_Data_20260620.csv')

np.random.seed(42)
n = 1000
fake = pd.DataFrame()

# Coordinates
fake['X'] = np.random.uniform(real['X'].min(), real['X'].max(), n).round(8)
fake['Y'] = np.random.uniform(real['Y'].min(), real['Y'].max(), n).round(8)

# Location / AM-PM
fake['Hectare'] = np.random.choice(real['Hectare'].dropna().unique(), n)
fake['Shift'] = np.random.choice(real['Shift'].dropna().unique(), n)

# Date 
start = datetime(2018, 10, 6)
fake['Date'] = [(start + timedelta(days=int(np.random.randint(0, 15)))).strftime('%m/%d/%Y') for _ in range(n)]

fake['Hectare Squirrel Number'] = np.random.randint(1, 31, n)
fake['Age'] = np.random.choice(real['Age'].dropna().unique(), n)

# Fur colors
fake['Primary Fur Color'] = np.random.choice(real['Primary Fur Color'].dropna().unique(), n)
fake['Highlight Fur Color'] = np.random.choice(real['Highlight Fur Color'].dropna().unique(), n)
fake['Combination of Primary and Highlight Color'] = np.random.choice(
    real['Combination of Primary and Highlight Color'].dropna().unique(), n
)
fake['Color notes'] = np.random.choice(real['Color notes'].dropna().unique(), n)

# Location
fake['Location'] = np.random.choice(real['Location'].dropna().unique(), n)

# Above Ground height (logical rule kept)
fake['Above Ground Sighter Measurement'] = 'FALSE'
above_ground = fake['Location'] == 'Above Ground'
heights = np.random.choice([2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 20, 25, 30, 40], 
                           size=above_ground.sum()).astype(str)
fake.loc[above_ground, 'Above Ground Sighter Measurement'] = heights

fake['Specific Location'] = np.random.choice(real['Specific Location'].dropna().unique(), n)

# Boolean columns - pure random (true/false), not probability weighted
bool_cols = ['Running', 'Chasing', 'Climbing', 'Eating', 'Foraging',
             'Kuks', 'Quaas', 'Moans', 'Tail flags', 'Tail twitches',
             'Approaches', 'Indifferent', 'Runs from']

for col in bool_cols:
    fake[col] = np.random.choice(['true', 'false'], n)

fake['Other Activities'] = np.random.choice(real['Other Activities'].dropna().unique(), n)

# Unique Squirrel ID
fake['Unique Squirrel ID'] = [
    f"{h}-{s}-{np.random.randint(100000, 999999)}-{np.random.randint(1, 30):02d}"
    for h, s in zip(fake['Hectare'], fake['Shift'])
]

# Save
fake.to_csv('fake_squirrel_data_1000_v3_pure_random.csv', index=False)
print("Generated fake_squirrel_data_1000_v3_pure_random.csv")
print(fake.head(3).to_string())