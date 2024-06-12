import os
import random
import preprocessor
import plotgenerator
import privbayes.notebooks.datasynthesizer as privbayes

if __name__ == '__main__': # This line is to stop Privbayes from spawning infinite threads
   ### VARIABLES ###

   ## Global variables ##
   dataset_name        = 'DPHP_num.csv' # There is an big where .csv is problematic when dataset_is_numeric is False
   dataset_is_numeric  = True # Hornbyu required numeric datasets
   run_hornbyhu        = False # Highly recomended to hornbyhu for a single epsilon
   generate_histograms = False # generates histograms and correlation matrices from privbayes

   # paths for the I/O files
   current_file_path = (__file__)
   parent_directory  = os.path.dirname(current_file_path)
   data_directory    = os.path.join(parent_directory, 'data')
   dataset_path      = os.path.join(data_directory, dataset_name)

   ## preprocessor variables ##
   labels_to_encode = ['Gender', 'CALC', 'FAVC', 'SCC', 'SMOKE', 'FHWO', 'CAEC', 'MTRANS', 'NObeyesdad']

   ## PrivBayes variables ##
   # An attribute is categorical if its domain size is less than this threshold.
   threshold_value            = 42
   seed                       = 0
   categorical_attributes     = {}
   candidate_keys             = {}
   epsilon                    = [0]  # Differential privacy values, will generate for each epsilon
   epsilon_string             = ['0']
   degree_of_bayesian_network = 2    # Number of incoming edges a node has
   num_tuples_to_generate     = 1000 # Number of tuples generated in synthetic dataset. NOTE: This cannot be less than cut in Hornby&Hu, else an error is generated.

   ## hornbyhu ##
   # use '" "' quotes if there is a space in the directory like: '"C:\Program Files\R\R-4.3.2\\bin\Rscript"'
   rscript_location = '"C:\Program Files\R\R-4.3.2\\bin\Rscript"'
   adr_location = parent_directory + "\hornbyhu\AttributeDisclosureRisk.R"


   ### EXECUTION ###

   ## Preprocessor ##
   if dataset_is_numeric == False:
      print('Converting ' + dataset_name +'.csv ' + 'to numeric as ' + dataset_name + '_num.csv')
      dataset_name = preprocessor.main(dataset_name, dataset_path, labels_to_encode, num_tuples_to_generate)

   
   for e in range(len(epsilon)):
      for run in range(1):
         seed = round(random.random()*100000000)
         print("seed:" + str(seed))

         print(dataset_name)
         print('Executing PrivBayes')
         privbayes.main(dataset_name, data_directory, threshold_value, categorical_attributes, candidate_keys, epsilon[e], epsilon_string[e], run, degree_of_bayesian_network, num_tuples_to_generate, seed)

      if generate_histograms == True:
         print('Generating histograms between real and synthetic data')
         plotgenerator.main(dataset_name, data_directory)
      
      if run_hornbyhu == True:
         print('Executing HornbyHu')
         #powershell command for executing hornbyhu
         #rscript_location --vanilla attributedisclosurerisk.r_location, hornbyhu_location, dataset_name
         hornbyhu_args = [parent_directory + "\hornbyhu", dataset_name]
         cmd = r"{} --vanilla {} {} {}".format(rscript_location, adr_location, hornbyhu_args[0], hornbyhu_args[1])
         os.system(cmd)
