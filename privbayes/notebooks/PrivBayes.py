from DataSynthesizer.DataDescriber import DataDescriber
from DataSynthesizer.DataGenerator import DataGenerator
from DataSynthesizer.ModelInspector import ModelInspector
from DataSynthesizer.bib.utils import read_json_file, display_bayesian_network

import pandas as pd
import os

def main():
   current_file_path = (__file__)
   parent_directory  = os.path.dirname(current_file_path)
   data_directory    = os.path.join(parent_directory, 'data')
   dataset_path      = os.path.join(data_directory, dataset_name)

   dataset_name = "CEData"

   threshold_value            = 20
   categorical_attributes     = {'UrbanRural': True, 'Race': True}
   candidate_keys             = {}
   epsilon                    = 10000  # Differential privacy value
   degree_of_bayesian_network = 2    # Number of edges a node can have
   num_tuples_to_generate     = 5126 # Number of tuples generated in synthetic dataset. NOTE: This cannot be less than cut in Hornby&Hu, else an error is generated.
   

   real_data        = data_directory + '\\' + dataset_name
   synthetic_data   = data_directory + '\\synthetic_data.csv'
   description_file = data_directory + '\\description.json'

   describer = DataDescriber(category_threshold=threshold_value)
   describer.describe_dataset_in_correlated_attribute_mode(dataset_file=real_data, 
                                                         epsilon=epsilon, 
                                                         k=degree_of_bayesian_network,
                                                         attribute_to_is_categorical=categorical_attributes,
                                                         attribute_to_is_candidate_key=candidate_keys)
   describer.save_dataset_description_to_file(description_file)

   display_bayesian_network(describer.bayesian_network)

   generator = DataGenerator()
   generator.generate_dataset_in_correlated_attribute_mode(num_tuples_to_generate, description_file)
   generator.save_synthetic_data(synthetic_data)

if __name__ == '__main__':
    main()