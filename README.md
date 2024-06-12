# Estimation of Bayesian Attribute Disclosure Risk of PrivBayes
Structure
   data: Contains real data, synthetic data and bayesian network
   hornby&hu: Contains Hornby and Hu's Attribute disclosure risk
   privbayes: Contains PrivBayes synthesizer
   plots: output plots from hornby&hu

Prerequisite
   Installation of R       version 4.3.3 (Not all packages was updated to work on 4.4.0)
   Installation of Python  version 3.7.9

Notable files
   Main.py
      -Executes datasetconverter.py (if specified), Privbayes and Hornby&Hu in sequence
   datasetconverter.py
      -Converts a dataset with strings as values to numbers
   description.json
      -Contains the Bayesian network and metadata like bin sample probabilities and more
   syntheticdata.csv
      -Output data from PrivBayes


How to run:
1. Install requirements.txt
2. Run Main.py




If you got errors read this
   P: HornbyHu does not accept strings as values, such as M and F or TRUE and False
   S: Use the preprocessor to convert them to numbers
   
   P: Low GLM iterations can cause floating point errors potentially halting the code
   S: Increase the iterations

   P: Attribute names does not support most special characters, attribute names like %cancer will give an error
   S: Use only letters, numbers and some symbols like '-' and '_' should work
   
   P: size errors
   S: make sure that real data and synthetic data needs to be the same size
