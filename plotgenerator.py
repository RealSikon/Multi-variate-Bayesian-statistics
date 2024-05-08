from DataSynthesizer.ModelInspector import ModelInspector
from DataSynthesizer.lib.utils import read_json_file

from matplotlib import pyplot as plt
import pandas as pd
import os

def main(dataset_name, data_directory):
   real_data        = data_directory + '\\' + dataset_name
   synthetic_data   = data_directory + '\\synthetic_data.csv'
   description_file = data_directory + '\\description.json'

   # Read both datasets using Pandas.
   real_df = pd.read_csv(real_data, skipinitialspace=True)
   synthetic_df = pd.read_csv(synthetic_data)
   # Read attribute description from the dataset description file.
   attribute_description = read_json_file(description_file)['attribute_description']

   inspector = ModelInspector(real_df, synthetic_df, attribute_description)

   for attribute in synthetic_df.columns:
      inspector.compare_histograms(attribute)
   
   inspector.mutual_information_heatmap()
   plt.show()

if __name__ == '__main__':
    main()