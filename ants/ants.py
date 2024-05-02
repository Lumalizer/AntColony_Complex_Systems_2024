from sktime.utils.plotting import plot_correlations
import pandas as pd
import os
import matplotlib.pyplot as plt
import math

indoor = [("indoor", 'ants/Ant_dataset_noimg/IndoorDataset/'+p +
          "/gt/gt.txt") for p in os.listdir('ants/Ant_dataset_noimg/IndoorDataset')]
outdoor = [("outdoor", 'ants/Ant_dataset_noimg/OutdoorDataset/'+p +
           "/gt/gt.txt") for p in os.listdir('ants/Ant_dataset_noimg/OutdoorDataset')]

data_locations = pd.DataFrame(indoor+outdoor, columns=['recording_location', 'file'])
print(data_locations)

# take a single "trial" (one group of ants moving through time and space) as an example
ants = pd.read_csv(data_locations['file'][0], names=['frameno', 'ant_id',
                                                     'bbleft', 'bbtop', 'bbwidth', 'bbheight', 'confidence'])

# there are multiple rows per frame, each row represents one ant in one frame, so there are multiple ants per frame
# the ant_id is the unique identifier for each ant
# ants have bounding boxes (boxes in which they are detected), so bbleft and bbtop can be used as x and y coordinates
ants['x'] = ants['bbleft'] - ants['bbwidth'] / 2
ants['y'] = ants['bbtop'] - ants['bbheight'] / 2

ants.drop(columns=['bbleft', 'bbtop', 'bbwidth', 'bbheight', 'confidence'], inplace=True)

# use groupby to make sure the shifted postions do not "leak" between different ants
ants['next_x'] = ants.groupby('ant_id')['x'].shift(-1)
ants['next_y'] = ants.groupby('ant_id')['y'].shift(-1)

ants['speed'] = ((ants['next_y'] - ants['y'])**2 +
                 (ants['next_x'] - ants['x'])**2)**0.5
heading = (ants['next_y'] - ants['y']) / (ants['next_x'] - ants['x'])

# convert heading to angle to ensure remaining in cos domain
ants['angle'] = heading.apply(lambda x: math.atan(x) if x != 0 else 0)
ants['cos(angle)'] = ants['angle'].apply(lambda x: math.cos(x))

# we now have a dataset with the following columns:
# frameno  ant_id        x        y   next_x   next_y      speed     angle    cos(angle)

print(f"Number of unique ants: {ants['ant_id'].nunique()}")


# a starting approach can be to investigate one single ant only:
ant_1 = ants[ants['ant_id'] == 11]
print(ant_1)

plot_correlations(ant_1['speed'])
plt.show()
