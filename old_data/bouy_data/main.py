import pandas as pd

df = pd.read_csv('bouy_data/bouy_data.csv')

na_counts = df.isna().sum()
na_percentage = (na_counts / len(df)) * 100

info_df = pd.DataFrame({'na_counts': na_counts, 'na_percentage': na_percentage})
print(info_df)
print(df.head())