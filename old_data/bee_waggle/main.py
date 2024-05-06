import pandas as pd
import json


with open('bee_waggle/bee_waggle_6.json') as f:
    data = json.load(f)

normalized_data = pd.json_normalize(data)
df = pd.DataFrame(normalized_data)

data = df['series'][0]
time = df['time.index'][0]
x, y, sin_theta, cos_theta = data[0]['raw'], data[1]['raw'], data[2]['raw'], data[3]['raw']

df = pd.DataFrame({'x': x, 'y': y, 'sin_theta': sin_theta, 'cos_theta': cos_theta}, index=time)

df.to_csv('bee_waggle/bee_waggle_6.csv')
print(df.head())
