from gluonts.dataset import common

def prepare_data_univariate(index, values, freq):
    
    data = common.ListDataset([{
        "start": index[0],
        "target": values
    }], freq=freq)
    
    return data

def prepare_data(index, values, freq):
    
    data = common.ListDataset([{
        "start": index[0],
        "target": values
    }], freq=freq)
    
    return data
