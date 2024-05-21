import os
import preprocessor
import plotgenerator
import privbayes.notebooks.datasynthesizer as privbayes

if __name__ == '__main__': # This line is to stop Privbayes from spawning infinite threads
   ## Global variables ##
   dataset_name        = 'CEData.csv' #there is an error where .csv is problematic when dataset_is_numeric is False
   dataset_is_numeric  = True # hornbyhu required numeric datasets
   generate_histograms = False # generates histograms and correlation matrices from privbayes

   # paths for the I/O files
   current_file_path = (__file__)
   parent_directory  = os.path.dirname(current_file_path)
   data_directory    = os.path.join(parent_directory, 'data')
   dataset_path      = os.path.join(data_directory, dataset_name)

   ## preprocessor variables ##
   labels_to_encode = []

   ## privbayes variables ##
   # An attribute is categorical if its domain size is less than this threshold.
   threshold_value            = 20
   categorical_attributes     = {'UrbanRural': True, 'Race': True, 'Income': True}
   candidate_keys             = {}
   epsilon                    = 1000    # Differential privacy value
   degree_of_bayesian_network = 2    # Number of edges a node can have
   num_tuples_to_generate     = 1000 # Number of tuples generated in synthetic dataset. NOTE: This cannot be less than cut in Hornby&Hu, else an error is generated.

   ## hornbyhu ##
   # use '" "' quotes if there is a space in the directory like: '"C:\Program Files\R\R-4.3.2\\bin\Rscript"'
   rscript_location = '"C:\Program Files\R\R-4.3.2\\bin\Rscript"'
   adr_location = parent_directory + "\hornbyhu\AttributeDisclosureRisk.R"

   if dataset_is_numeric == False:
      print('Converting ' + dataset_name +'.csv ' + 'to numeric as ' + dataset_name + '_num.csv')
      dataset_name = preprocessor.main(dataset_name, dataset_path, labels_to_encode, num_tuples_to_generate)

   print(dataset_name)
   print('Executing PrivBayes')
   privbayes.main(dataset_name, data_directory, threshold_value, categorical_attributes, candidate_keys, epsilon, degree_of_bayesian_network, num_tuples_to_generate)

   if generate_histograms == True:
      print('Generating histograms between real and synthetic data')
      plotgenerator.main(dataset_name, data_directory)
      
   print('Executing HornbyHu')
   #powershell command for executing hornbyhu
   #rscript_location --vanilla attributedisclosurerisk.r_location, hornbyhu_location, dataset_name
   hornbyhu_args = [parent_directory + "\hornbyhu", dataset_name]
   cmd = r"{} --vanilla {} {} {}".format(rscript_location, adr_location, hornbyhu_args[0], hornbyhu_args[1])
   os.system(cmd)
