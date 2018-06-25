import feather
import numpy as np
import pandas as pd

DATA_DIR = "/Users/tylermuffly/data/" # guessing here
FEATHER_FILE = DATA_DIR + "data.feather"
df = feather.read_dataframe(FEATHER_FILE)
years = np.arange(df["pubYear"].min(), df["pubYear"].max()+1)
authors = df["full_name"].unique()


def get_author(dat, author):
    """
    :type dat: data.frame
    :type author: str
    :rtype: data.frame
    """
    return dat[dat["full_name"] == author]


def get_year_range(dat, author):
    """
    :type dat: data.frame
    :type author: str
    :rtype: List[int]
    """
    return np.arange(get_author(dat, author)['pubYear'].min(), get_author(dat, author)['pubYear'].max()+1)


def get_citations(dat, year):
    """
    :type dat: data.frame
    :type year: int
    :rtype: data.frame
    """
    return dat[(dat["pubYear"] <= year) & (dat["pubYear"] >= dat["minPubYear"])]["citedByCount"].tolist()


def hIndex(citations): 
    """
    :type citations: List[int]
    :rtype: int
    """
    citations.sort(reverse=True)
    h = 0
    for x in citations:
        if x >= h + 1:
            h += 1
        else:
            break
    return h


h_indices = {author: {year: hIndex(get_citations(get_author(df, author), year)) for year in get_year_range(df, author)} for author in authors}
feather.write_dataframe(df=pd.DataFrame(h_indices), dest=DATA_DIR + "h_indices.feather")
