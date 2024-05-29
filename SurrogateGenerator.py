import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.neighbors import NearestNeighbors
from os.path import join
from data_functions import get_ant_datafiles_info, preprocess_antcolony_data
import multiSyncPy
from scipy.stats import rankdata
from numpy.fft import fft, ifft


# important functions

# segm_shuffle - generates a surrogate data by shuffling segments of the original data
# surrogate_aaft - generates a surrogate data by applying the amplitude adjusted Fourier transform (AAFT) method

# trajectory_plot - plots the trajectory of the ants from original or surrogate data
# get_n_aafts - generates n surrogate data by applying the surrogate_aaft function to a DataFrame
# get_n_segm_shuffles - generates n surrogate data by applying the segm_shuffle function to a DataFrame


# testing

# null hypothesis synchrony: extracted features for synchrony are not due to randomness
# null hypothesis rqa: extracted features for recurrence quantification analysis are not due to randomness
# different surrogate data generation methods can test for isolated parts of randomness in the data (e.g. long/short term temporal/spatial randomness)

# generate n surrogate data via AAFT or segment shuffling
# extract and store measures for each surrogate data
# normalize the distribution of each measure
# compare the original data measures to the surrogate data measures - signifance tests


def get_nth_colony(n: int):
    ant_data = get_ant_datafiles_info()
    df = preprocess_antcolony_data(ant_data['file'][n])
    return df


def get_data():
    global colonies
    colonies = [ ]
    for i in range (10):
        colonies.append(f'df{i}')
        exec(f'df{i} = get_nth_colony({i})')


def get_change(df: pd.DataFrame, x_col: str, y_col: str, id_col: str):
    # for each ant_id subtract the previous value (row above) from the current value (current row)
    df['dx'] = df.groupby(id_col)[x_col].diff()
    df['dy'] = df.groupby(id_col)[y_col].diff()
    # fill the NaN values with 0 at 'dx' and 'dy' columns
    df['dx'].fillna(0, inplace=True)
    df['dy'].fillna(0, inplace=True)


def calculate_new_xy(df: pd.DataFrame, x_col: str, y_col: str, id_col: str):
    # for each ant_id calculate the new x and y values
    for ant_id in df[id_col].unique():
        ant_data = df[df[id_col] == ant_id]
        # iterate over ant_data and calculate the new x and y values, each new value is the sum of the previous value and the change
        for i in range(1, len(ant_data)):
            ant_data.iloc[i, ant_data.columns.get_loc(x_col)] = ant_data.iloc[i-1][x_col] + ant_data.iloc[i]['dx']
            ant_data.iloc[i, ant_data.columns.get_loc(y_col)] = ant_data.iloc[i-1][y_col] + ant_data.iloc[i]['dy']
        df[df[id_col] == ant_id] = ant_data


def shuffle_change_segments(df: pd.DataFrame, id_col: str, segm_length: int):
    # Convert segm_length to integer if it's not
    segm_length = int(segm_length)

    # Create a new DataFrame to store shuffled data
    shuffled_df = pd.DataFrame()
    
    # Iterate over each unique ant_id
    for ant_id in df[id_col].unique():
        ant_data = df[df[id_col] == ant_id].copy()  # Make a copy of the ant data
        
        # Calculate the number of full segments
        full_segments_count = len(ant_data) // segm_length
        
        # Extract full segments
        segments = []
        for i in range(full_segments_count):
            start_index = i * segm_length
            end_index = start_index + segm_length
            segments.append(ant_data.iloc[start_index:end_index])
        
        # Handle any remaining data that didn't fit into a full segment
        if len(ant_data) % segm_length != 0:
            segments.append(ant_data.iloc[full_segments_count * segm_length:])
        
        # Shuffle the segments with a random seed
        np.random.seed(42)
        np.random.shuffle(segments)
        
        # Concatenate shuffled segments back together
        shuffled_segments = pd.concat(segments, ignore_index=True)
        
        # Assign the shuffled data back to the appropriate rows in the main DataFrame
        df.loc[df[id_col] == ant_id, ['dx', 'dy']] = shuffled_segments[['dx', 'dy']].values
    
    return df


def segm_shuffle(df: pd.DataFrame, x_col: str, y_col: str, id_col: str, segm_length: int):

    df = df.copy()
    get_change(df, x_col, y_col, id_col)
    shuffle_change_segments(df, id_col, segm_length)
    calculate_new_xy(df, x_col, y_col, id_col)
    return df


def trajectory_plot(df: pd.DataFrame, x_col: str, y_col: str, id_col: str):
    ant_id = df[id_col].unique()[0]
    # convert array to pandas series
    for i in df[id_col].unique():
        ant_surrogate_x = pd.Series(df.loc[df[id_col] == i, x_col])
        ant_surrogate_y = pd.Series(df.loc[df[id_col] == i, y_col])
        # plot the trajectory
        plt.plot(ant_surrogate_x, ant_surrogate_y)
    plt.xlabel(x_col)
    plt.ylabel(y_col)
    plt.title(f'Trajectory of ants')
    plt.show()


def aaft(data):
    # Transform the data into ranks and create sorted Gaussian noise
    ranks = rankdata(data, method='ordinal')
    np.random.seed(42)
    random_data = np.random.normal(size=len(data))
    random_sorted = np.sort(random_data)

    # Initialize the surrogate with Gaussian data according to original data ranks
    surrogate = random_sorted[np.argsort(np.argsort(ranks))]

    # Apply Fourier transform
    fourier_transform = fft(surrogate)
    random_phases = np.exp(2j * np.pi * np.random.random(size=len(data)))

    # Adjust Fourier components by random phases and inverse Fourier transform
    adjusted_fourier = fourier_transform * random_phases
    new_surrogate = ifft(adjusted_fourier).real

    # Match the sorted surrogate to the original data's sorting
    final_surrogate = np.empty_like(data)
    final_surrogate[np.argsort(data)] = np.sort(new_surrogate)

    return final_surrogate


def scale_data(original: np.ndarray, surrogate: np.ndarray):
    min_val = original.min()
    max_val = original.max()
    scaled_surrogate = (surrogate - surrogate.min()) / (surrogate.max() - surrogate.min()) * (max_val - min_val) + min_val
    return scaled_surrogate


def surrogate_aaft(df: pd.DataFrame, x_col: str, y_col: str, id_col: str):
    surrogate_data = pd.DataFrame(columns=[id_col, 'surrogate_x', 'surrogate_y'])

    # Apply AAFT and scale for each ant's 'x' and 'y' data
    for ant_id in df[id_col].unique():
        ant_data = df[df[id_col] == ant_id]
        surrogate_x = aaft(ant_data[x_col].values)
        surrogate_y = aaft(ant_data[y_col].values)

        # Scale surrogate data to match the original data range
        scaled_surrogate_x = scale_data(ant_data[x_col], surrogate_x)
        scaled_surrogate_y = scale_data(ant_data[y_col], surrogate_y)

        # Update original DataFrame with scaled surrogate data
        df.loc[df[id_col] == ant_id, x_col] = scaled_surrogate_x
        df.loc[df[id_col] == ant_id, y_col] = scaled_surrogate_y

        # Create a temporary DataFrame for current ant's scaled data
        temp_df = pd.DataFrame({
            id_col: ant_id,
            'surrogate_x': scaled_surrogate_x,
            'surrogate_y': scaled_surrogate_y
        })

        # Append current ant's scaled data to the surrogate_data DataFrame
        surrogate_data = pd.concat([surrogate_data, temp_df], ignore_index=True)

    return surrogate_data


'''
def get_n_aafts(df: pd.DataFrame, n: int, x_col: str, y_col: str, id_col: str):
    surrogates = []
    for i in range(n):
        surrogates.append(f'surrogate{i}')
        exec(f'df_aaft_{i} = surrogate_aaft(df, x_col, y_col, id_col)')
    return surrogates
'''


def get_n_aafts(df: pd.DataFrame, n: int, x_col: str, y_col: str, id_col: str):
    surrogates = {}
    for i in range(n):
        surrogate_name = f'surrogate{i}'
        surrogates[surrogate_name] = surrogate_aaft(df, x_col, y_col, id_col)
    return surrogates


def get_n_segm_shuffles(df: pd.DataFrame, n: int, x_col: str, y_col: str, id_col: str, segm_length: int):
    surrogates = {}
    for i in range(n):
        surrogate_name = f'surrogate{i}'
        surrogates[surrogate_name] = segm_shuffle(df, x_col, y_col, id_col, segm_length)
    return surrogates

