import os
import math
import pandas as pd
from multisync_functions import get_multisync_metrics


os.environ['QT_DEVICE_PIXEL_RATIO'] = ''  # disable annoying warnings on plotting


def process_bird_data(birds, selected_class, target_col_pre, n_scenes=10):
    birds = birds[birds['Class'] == selected_class]
    convert = []
    results = []
    for scene in range(n_scenes):
        for bird_id in range(1, 20):
            bird_id = scene * 20 + bird_id
            target_col = target_col_pre + str(bird_id)
            data = birds[target_col]
            for d in data:
                convert.append((scene, bird_id, d))

    birds_convert = pd.DataFrame(convert, columns=['scene', 'bird_id', target_col_pre])

    for scene in range(n_scenes):
        target_birds = birds_convert[birds_convert['scene'] == scene]
        result = get_multisync_metrics(target_birds, target_var=target_col_pre,
                                       id_column='bird_id', n_observations_per_unit=1000)
        results.append((scene, selected_class, *result))
    return results


def get_ant_datafiles_info(prepath=""):
    indoor = [("indoor", prepath+'dataset/ants/IndoorDataset/'+p +
               "/gt/gt.txt") for p in sorted(os.listdir(prepath+'dataset/ants/IndoorDataset'))]
    outdoor = [("outdoor", prepath+'dataset/ants/OutdoorDataset/'+p +
                "/gt/gt.txt") for p in sorted(os.listdir(prepath+'dataset/ants/OutdoorDataset'))]

    data_locations = pd.DataFrame(indoor+outdoor, columns=['recording_location', 'file'])
    data_locations = augment_ant_data(data_locations)
    return data_locations

def augment_ant_data(df):
    location = ["china","china","china","china","china",'russia','greece',"russia","russia","usa"]
    n_ants = [10, 10, 10, 10, 10, 73, 162, 133, 193, 101]
    species_name = ["japanese_arched_ant","japanese_arched_ant","japanese_arched_ant","japanese_arched_ant","japanese_arched_ant","carpenter_ant","little_black_ant","carpenter_ant","carpenter_ant","little_black_ant"]
    enterance = [0,0,0,0,0,1,1,1,1,0]

    df['location'] = location
    df['n_ants'] = n_ants
    df['species_name'] = species_name
    df['enterance'] = enterance
    return df

def preprocess_antcolony_data(datafile, n_ants=10):
    ants = pd.read_csv(datafile, names=['frameno', 'ant_id', 'bbleft', 'bbtop', 'bbwidth', 'bbheight', 'confidence'])

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

    # sample n_ants
    ids = set(ants['ant_id'])
    info = []
    for ant_id in ids:
        info.append((ant_id, len(ants[ants['ant_id'] == ant_id])))

    info = sorted(info, key=lambda x: x[1], reverse=True)
    info = info[:n_ants]

    ants = ants[ants['ant_id'].isin([x[0] for x in info])]

    return ants

if __name__ == "__main__":
    data_locations = get_ant_datafiles_info()
    print(data_locations)
    preprocess_antcolony_data(data_locations['file'][0])