import pandas as pd
import numpy as np
from datetime import datetime, timedelta

real = pd.read_csv(r'C:\Users\ganzs\Documents\CUNY_Assignments\604\Week_3\2018_Central_Park_Squirrel_Census_-_Squirrel_Data_20260620.csv')
np.random.seed(42)
n = 1000
fake = pd.DataFrame()
start = datetime(2018, 10, 6)


def sample_from_real_distribution(column, n):
    probs = real[column].value_counts(normalize=True)
    return np.random.choice(probs.index, n, p=probs.values)


# Coordinates
fake['X'] = np.random.uniform(real['X'].min(), real['X'].max(), n).round(8)
fake['Y'] = np.random.uniform(real['Y'].min(), real['Y'].max(), n).round(8)

# # Location / AM-PM
fake['Hectare'] = sample_from_real_distribution('Hectare', n)
fake['Shift'] = sample_from_real_distribution('Shift', n)

# Date
fake['Date'] = [(start + timedelta(days=int(np.random.randint(0, 15)))).strftime('%m/%d/%Y') for _ in range(n)]

# Squirrel Info
fake['Hectare Squirrel Number'] = np.random.randint(1, 31, n)
fake['Age'] = sample_from_real_distribution('Age', n)

# Fur Color
fake['Primary Fur Color'] = sample_from_real_distribution('Primary Fur Color', n)
fake['Highlight Fur Color'] = sample_from_real_distribution('Highlight Fur Color', n)
fake['Combination of Primary and Highlight Color'] = sample_from_real_distribution(
    'Combination of Primary and Highlight Color', n
)
fake['Color notes'] = sample_from_real_distribution('Color notes', n)

# Location 
fake['Location'] = sample_from_real_distribution('Location', n)
# Above Ground height
fake['Above Ground Sighter Measurement'] = 'FALSE'
above_ground = fake['Location'] == 'Above Ground'
heights = np.random.choice([2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 20, 25, 30, 40], 
                           size=above_ground.sum()).astype(str)
fake.loc[above_ground, 'Above Ground Sighter Measurement'] = heights

fake['Specific Location'] = sample_from_real_distribution('Specific Location', n)

# Boolean columns
bool_cols = ['Running', 'Chasing', 'Climbing', 'Eating', 'Foraging',
             'Kuks', 'Quaas', 'Moans', 'Tail flags', 'Tail twitches',
             'Approaches', 'Indifferent', 'Runs from']

for col in bool_cols:
    true_rate = (real[col] == True).mean()
    fake[col] = np.random.choice(['true', 'false'], n, p=[true_rate, 1 - true_rate])

fake['Other Activities'] = sample_from_real_distribution('Other Activities', n)

# Unique Squirrel ID
fake['Unique Squirrel ID'] = [
    f"{h}-{s}-{np.random.randint(100000, 999999)}-{np.random.randint(1, 30):02d}"
    for h, s in zip(fake['Hectare'], fake['Shift'])
]

fake.to_csv('fake_squirrel_data_1000_empirical_distribution.csv', index=False)