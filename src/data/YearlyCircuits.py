import requests as r
import pandas as pd
from typing import List
import re
import itertools

EVENT_PATTERN: re.Pattern = re.compile("[A-Z]{3}")


def get_yearly_events(year: int) -> List:
    wiki_base = "https://en.wikipedia.org/wiki/"
    YEAR = str(year)

    BASE_URL_OLD = wiki_base + "{YEAR}_Grand_Prix_motorcycle_racing_season".format(
        YEAR=YEAR
    )

    BASE_URL_NEW = wiki_base + "{YEAR}_MotoGP_World_Championship".format(YEAR=YEAR)

    for possible_url in [BASE_URL_NEW, BASE_URL_OLD]:
        try:
            r.get(BASE_URL_OLD)
            LIST_OF_TABLES: List[pd.Dataframe] = pd.read_html(
                possible_url, flavor="html5lib"
            )
        except:
            continue

    for df in LIST_OF_TABLES:
        if "CAT" in df.columns:
            events = df

    try:
        event_fields = events.columns.to_list()
    except:
        return []
        
    return [
        EVENT_PATTERN.findall(x)[0] for x in event_fields if EVENT_PATTERN.findall(x)
    ]


if __name__ == "__main__":
    years = []
    events = []

    for year in range(2006, 2022):
        _events = get_yearly_events(year)
        events.append(_events)
        years.append([year] * len(_events))
        print(year, _events)
    years = list(itertools.chain(*years))
    events = list(itertools.chain(*events))
    pd.DataFrame(events, years).to_csv("Yearly_Circuits.csv")
