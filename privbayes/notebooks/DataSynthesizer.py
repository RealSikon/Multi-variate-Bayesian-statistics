from privbayes.DataSynthesizer.DataDescriber import DataDescriber
from privbayes.DataSynthesizer.DataGenerator import DataGenerator
from privbayes.DataSynthesizer.ModelInspector import ModelInspector
from privbayes.DataSynthesizer.bib.utils import read_json_file, display_bayesian_network

import pandas as pd
import os

def main(dataset_name, data_directory, threshold_value, categorical_attributes, candidate_keys, epsilon, epsilon_string, run, degree_of_bayesian_network, num_tuples_to_generate, seed):
   real_data        = data_directory + '\\' + dataset_name
   synthetic_data   = data_directory + '\\synthetic_data' + epsilon_string + '_' + str(run) + '.csv'
   description_file = data_directory + '\\description' + epsilon_string + '_' + str(run) + '.json'

   describer = DataDescriber(category_threshold=threshold_value)
   describer.describe_dataset_in_correlated_attribute_mode(dataset_file=real_data, 
                                                         epsilon=epsilon, 
                                                         k=degree_of_bayesian_network,
                                                         attribute_to_is_categorical=categorical_attributes,
                                                         attribute_to_is_candidate_key=candidate_keys)
   describer.save_dataset_description_to_file(description_file)

   display_bayesian_network(describer.bayesian_network)

   generator = DataGenerator()
   generator.generate_dataset_in_correlated_attribute_mode(num_tuples_to_generate, description_file, seed)
   generator.save_synthetic_data(synthetic_data)

if __name__ == '__main__':
    main()