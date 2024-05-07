import os
import datasetconverter


dataset_name     = 'adult_tiny'
labels_to_encode = ['education', 'sex', 'relationship', 'maritalstatus', 'income']

# paths for the I/O files
current_file_path = (__file__)
parent_directory  = os.path.dirname(current_file_path)
data_directory    = os.path.join(parent_directory, 'data')
dataset_path      = os.path.join(data_directory, dataset_name)

dataset_is_numeric = False

if dataset_is_numeric == False:
   dataset_name = datasetconverter.main(dataset_path, labels_to_encode)

