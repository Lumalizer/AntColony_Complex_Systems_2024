import numpy as np
import pandas as pd
from multiSyncPy import synchrony_metrics as sm
from scipy import stats
from scipy.signal import hilbert


def get_multisync_metrics(colony_data: pd.DataFrame, target_var: str = 'x',
                          n_observations_per_unit: int = 100, id_column='ant_id'):

    unit_dict = {unit_id: colony_data[colony_data[id_column] == unit_id]
                 for unit_id in colony_data[id_column].unique()}

    # for key, item in unit_dict.items():
    #     print(f"Unit {key} has {len(item)} observations")

    full_len = len(unit_dict)

    unit_dict = {unit_id: colony_data[:n_observations_per_unit]
                 for unit_id, colony_data in unit_dict.items() if len(colony_data) > n_observations_per_unit}

    removed = full_len - len(unit_dict)
    # if removed:
    #     print(f"Removed {removed} out of {full_len} units with less than {n_observations_per_unit} observations")

    team_data = np.array([unit_data[target_var].values for unit_data in unit_dict.values()])
    data_phases = np.angle(hilbert(team_data))

    coherence = sm.coherence_team(team_data)
    symbolic_entropy = sm.symbolic_entropy(team_data)
    rho = sm.rho(data_phases)[1]

    return coherence, symbolic_entropy, rho


def get_multisync_statistics(group1, group2):
    metrics = ['coherence', 'symbolic_entropy', 'rho']
    ttest_pvalues = []
    ttest_statistic = []
    mannwhitneyu_pvalues = []
    mannwhitneyu_statistic = []

    for metric in metrics:
        ttest_result = stats.ttest_ind(group1[metric], group2[metric])
        mannwhitneyu_result = stats.mannwhitneyu(group1[metric], group2[metric])

        ttest_pvalues.append(ttest_result.pvalue)
        ttest_statistic.append(ttest_result.statistic)
        mannwhitneyu_pvalues.append(mannwhitneyu_result.pvalue)
        mannwhitneyu_statistic.append(mannwhitneyu_result.statistic)

    results_df = pd.DataFrame({
        'Metric': metrics,
        'T-test (p-value)': ttest_pvalues,
        'T-test (statistic)': ttest_statistic,
        'Mann-Whitney U (p-value)': mannwhitneyu_pvalues,
        'Mann-Whitney U (statistic)': mannwhitneyu_statistic
    })

    return results_df
