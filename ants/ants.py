from sktime.utils.plotting import plot_correlations
import pandas as pd
import os
import matplotlib.pyplot as plt

indoor = [("indoor", 'ants/Ant_dataset_noimg/IndoorDataset/'+p +
          "/gt/gt.txt") for p in os.listdir('ants/Ant_dataset_noimg/IndoorDataset')]
outdoor = [("outdoor", 'ants/Ant_dataset_noimg/OutdoorDataset/'+p +
           "/gt/gt.txt") for p in os.listdir('ants/Ant_dataset_noimg/OutdoorDataset')]

data_locations = pd.DataFrame(indoor+outdoor, columns=['recording_location', 'file'])
print(data_locations)

tryout_ants = pd.read_csv(data_locations['file'][0], names=['frameno', 'ant_id',
                          'bbleft', 'bbtop', 'bbwidth', 'bbheight', 'confidence'])
print(tryout_ants.head())

print(f"Number of unique ants: {tryout_ants['ant_id'].nunique()}")

# there are multiple rows per frame, each row represents one ant in one frame, so there are multiple ants per frame
# the ant_id is the unique identifier for each ant
# ants have bounding boxes (boxes in which they are detected), so bbleft and bbtop can be used as x and y coordinates
# a starting approach can be to investigate one single ant only:

ant_1 = tryout_ants[tryout_ants['ant_id'] == 11]
print(ant_1.head())


plot_correlations(ant_1['bbtop'])
plt.show()
