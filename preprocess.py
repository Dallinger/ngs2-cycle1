import json
import math

import numpy as np
import pandas as pd

import dallinger


def analyze(id, label):
    data = dallinger.data.load(id)

    print("\n\nDataset: {}".format(id))
    print("Condition: {}".format(label))

    valid_participant_ids = [int(n[11]) for n in data.nodes.list if n[11] >= 0]

    average_bonus = data.participants.df['bonus'].replace(0, np.NaN).mean()
    print("\nAverage bonus: {}".format(average_bonus))

    infos_df = data.infos.df
    dataState = infos_df.loc[infos_df['type'] == 'state']
    final_state = json.loads(dataState.iloc[-1][-2])
    players = final_state['players']
    player_scores = [player['score'] for player in players]
    average_score = float(sum(player_scores)) / len(player_scores)
    print("\nAverage score: {}".format(average_score))

    DIFI_scores = [float(json.loads(r[13])['DIFI_distance']) for r in data.questions.list if int(r[10]) in valid_participant_ids]
    average_DIFI_score = np.median(np.array(DIFI_scores))
    print("\nDIFI scores: {}".format(DIFI_scores))
    print("Median DIFI score: {}".format(average_DIFI_score))

    scores = np.zeros(20)
    for i in range(20):
        item = 'leach_{}'.format(i+1)
        this_item_scores = []
        for r in data.questions.list:
            if int(r[10]) in valid_participant_ids:
                j = json.loads(r[13])
                try:
                    this_item_scores.append(float(j[item]))
                except KeyError:
                    pass
        scores[i] = pd.DataFrame(this_item_scores).replace(0, np.NaN).mean()
    print("\nLeach et al. (2008) scores:")
    for i, score in enumerate(scores):
        print("Item {}: {}".format(i+1, score))

    print("Mean Leach et al. (2008) score: {}".format(scores[0:15].mean()))

    move_count = 0
    ingroup_donation_count = 0
    public_donation_count = 0
    for r in data.infos.df['details'].iteritems():
        if pd.notnull(r[1]):
            j = json.loads(r[1])
            if j['type'] == 'move':
                move_count += 1
            if j['type'] in ['donation_submitted', 'donation_processed']:
                if "group" in j['recipient_id']:
                    ingroup_donation_count += 1
                elif "all" in j['recipient_id']:
                    public_donation_count += 1

    print("\nMove count: {}".format(move_count))
    print("\nIn-group contribution count: {}".format(ingroup_donation_count))
    print("\nPublic contribution count: {}".format(public_donation_count))

    return {
        "ingroup_donation_count": ingroup_donation_count,
        "public_donation_count": public_donation_count,
        "difi": average_DIFI_score,
        "leach": scores[0:15].mean(),
        "move_count": move_count,
    }


if __name__ == "__main__":

    runs = json.load(open('runs.json'))
    move_counts = []
    ingroup_donation_counts = []
    public_donation_counts = []
    for condition in runs:
        print(condition)
        contributions = []
        difis = []
        leaches = []
        for run in runs[condition]:
            results = analyze(run, condition)
            print(run)
            move_counts.append(results['move_count'])
            print(results['move_count'])
            ratio = (1.0+results['ingroup_donation_count']) / (1.0+results['public_donation_count'])
            ingroup_preference = math.log(ratio, 2)
            contributions.append(ingroup_preference)
            difis.append(results['difi'])
            leaches.append(results['leach'])
            ingroup_donation_counts.append(results['ingroup_donation_count'])
            public_donation_counts.append(results['public_donation_count'])

    print("contributions")
    print(contributions)
    print(np.array(contributions).mean())
    print(np.array(contributions).std() / np.sqrt(10))
    print("difi")
    print(difis)
    print(np.array(difis).mean())
    print(np.array(difis).std() / np.sqrt(10))
    print("leach")
    print(leaches)
    print(np.array(leaches).mean())
    print(np.array(leaches).std() / np.sqrt(10))
    print("=========")
