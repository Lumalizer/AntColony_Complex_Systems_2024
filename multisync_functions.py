import numpy as np
import pandas as pd
from multiSyncPy import synchrony_metrics as sm
from scipy import stats
from scipy.signal import hilbert
from data_functions import preprocess_antcolony_data

def get_unit_dict(colony_data: pd.DataFrame, id_column='ant_id', n_observations_per_unit=100, surrogates=False):
    """Returns a dictionary of units with at least n_observations_per_unit observations. Each unit is a DataFrame."""
    unit_dict = {unit_id: colony_data[colony_data[id_column] == unit_id]
                 for unit_id in colony_data[id_column].unique()}
    
    full_len = len(unit_dict)
    
    unit_dict = {unit_id: colony_data[:n_observations_per_unit]
                 for unit_id, colony_data in unit_dict.items() if len(colony_data) > n_observations_per_unit}
    
    if surrogates:
        for unit_data in unit_dict.values():
            unit_data['x'] = np.random.permutation(unit_data['x'].values)
            unit_data['y'] = np.random.permutation(unit_data['y'].values)
    
    removed = full_len - len(unit_dict)
    
    # if removed:
    #     print(f"Removed {removed} out of {full_len} units with less than {n_observations_per_unit} observations")

    return unit_dict


def get_multisync_metrics(colony_data: pd.DataFrame, target_var: str = 'x', id_column='ant_id', round_digits=3, surrogates=False):
                          
    unit_dict = get_unit_dict(colony_data, id_column=id_column, surrogates=surrogates)

    team_data = np.array([unit_data[target_var].values for unit_data in unit_dict.values()])
    data_phases = np.angle(hilbert(team_data))

    coherence = sm.coherence_team(team_data)
    symbolic_entropy = sm.symbolic_entropy(team_data)
    rho = sm.rho(data_phases)[1]
    csd = sm.sum_normalized_csd(team_data)

    coherence = round(coherence, round_digits)
    csd = round(csd, round_digits)
    symbolic_entropy = round(symbolic_entropy, round_digits)
    rho = round(rho, round_digits)

    return coherence, csd, symbolic_entropy, rho

def add_multisync_metrics(ant_colonies: pd.DataFrame, target_var='x', id_column='ant_id', surrogates=False):
    ant_colonies = ant_colonies.copy()
    preprocessed_colonies = [preprocess_antcolony_data(colony) for colony in ant_colonies['file'].tolist()]
    metrics = [get_multisync_metrics(colony, target_var=target_var, id_column=id_column, surrogates=surrogates) for colony in preprocessed_colonies]
    coherence, csd, symbolic_entropy, rho = zip(*metrics)

    ant_colonies['coherence'] = coherence
    ant_colonies['csd'] = csd
    ant_colonies['symbolic_entropy'] = symbolic_entropy
    ant_colonies['rho'] = rho
    return ant_colonies


def get_multisync_statistics(group1, group2, round_digits=10):
    metrics = ['coherence', 'csd', 'symbolic_entropy', 'rho']
    ttest_pvalues = []
    ttest_statistic = []
    mannwhitneyu_pvalues = []
    mannwhitneyu_statistic = []

    for metric in metrics:
        ttest_result = stats.ttest_ind(group1[metric], group2[metric])
        mannwhitneyu_result = stats.mannwhitneyu(group1[metric], group2[metric])

        ttest_pvalues.append(round(ttest_result.pvalue, round_digits))
        ttest_statistic.append(round(ttest_result.statistic, round_digits))
        mannwhitneyu_pvalues.append(round(mannwhitneyu_result.pvalue, round_digits))
        mannwhitneyu_statistic.append(round(mannwhitneyu_result.statistic, round_digits))

    results_df = pd.DataFrame({
        'Metric': metrics,
        'T-test (p-value)': ttest_pvalues,
        'T-test (statistic)': ttest_statistic,
        'Mann-Whitney U (p-value)': mannwhitneyu_pvalues,
        'Mann-Whitney U (statistic)': mannwhitneyu_statistic
    })

    return results_df


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
