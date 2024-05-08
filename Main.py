import os
import datasetconverter
import plotgenerator
import privbayes.notebooks.datasynthesizer as privbayes

if __name__ == '__main__': # This line is to stop Privbayes from spawning infinite threads
   ## Global variables ##
   dataset_name        = 'adult_tiny'
   dataset_is_numeric  = False
   generate_histograms = False

   ## datasetconverter variables ##
   labels_to_encode = ['education', 'sex', 'relationship', 'maritalstatus', 'income']

   ## privbayes variables ##
   # An attribute is categorical if its domain size is less than this threshold.
   threshold_value            = 20
   categorical_attributes     = {'education': True, 'sex': True, 'relationship': True, 'marital_status': True, 'income': True}
   candidate_keys             = {'': True}
   epsilon                    = 1    # Differential privacy value
   degree_of_bayesian_network = 2    # Number of edges a node can have
   num_tuples_to_generate     = 1000 # Number of tuples generated in synthetic dataset. NOTE: This cannot be less than cut in Hornby&Hu, else an error is generated.


   # paths for the I/O files
   current_file_path = (__file__)
   parent_directory  = os.path.dirname(current_file_path)
   data_directory    = os.path.join(parent_directory, 'data')
   dataset_path      = os.path.join(data_directory, dataset_name)

   if dataset_is_numeric == False:
      print('Converting ' + dataset_name +'.csv ' + 'to numeric as ' + dataset_name + '_num.csv')
      dataset_name = datasetconverter.main(dataset_name, dataset_path, labels_to_encode)

   print('Executing PrivBayes')
   privbayes.main(dataset_name, data_directory, threshold_value, categorical_attributes, candidate_keys, epsilon, degree_of_bayesian_network, num_tuples_to_generate)

   if generate_histograms == True:
      print('Generating histograms between real and synthetic data')
      plotgenerator.main(dataset_name, data_directory)
      