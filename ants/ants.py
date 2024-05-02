import os
import math
import pandas as pd
import numpy as np


os.environ['QT_DEVICE_PIXEL_RATIO'] = ''  # disable annoying warnings on plotting


def get_datafiles_info(prepath="ants/"):
    indoor = [("indoor", prepath+'Ant_dataset_noimg/IndoorDataset/'+p +
               "/gt/gt.txt") for p in os.listdir(prepath+'Ant_dataset_noimg/IndoorDataset')]
    outdoor = [("outdoor", prepath+'Ant_dataset_noimg/OutdoorDataset/'+p +
                "/gt/gt.txt") for p in os.listdir(prepath+'Ant_dataset_noimg/OutdoorDataset')]

    data_locations = pd.DataFrame(indoor+outdoor, columns=['recording_location', 'file'])
    return data_locations


def preprocess_antcolony_data(datafile):
    ants = pd.read_csv(datafile, names=['frameno', 'ant_id',
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
    ants['speed'] = ((ants['next_y'] - ants['y'])**2 + (ants['next_x'] - ants['x'])**2)**0.5
    heading = (ants['next_y'] - ants['y']) / (ants['next_x'] - ants['x'])

    # convert heading to angle to ensure remaining in cos domain
    ants['angle'] = heading.apply(lambda x: math.atan(x) if x != 0 else 0)
    ants['cos(angle)'] = ants['angle'].apply(lambda x: math.cos(x))
    return ants
