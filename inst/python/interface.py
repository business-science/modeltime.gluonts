from gluonts.dataset import common
from gluonts.trainer import Trainer
from gluonts.model import deepar

import pandas as pd

def prepare_data_univariate(index, values, freq):
    
    data = common.ListDataset([{
        "start": index[0],
        "target": values
    }], freq=freq)
    
    return data


