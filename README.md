# Multi-variate-Bayesian-statistics
Structure
   data: Contains real data, synthetic data and bayesian network
   hornby&hu: Contains Hornby and Hu's Attribute disclosure risk
   privbayes: Contains PrivBayes synthesizer
   plots: output plots from hornby&hu

Notable files
   Main.py
      -Executes datasetconverter.py (if specified), Privbayes and Hornby&Hu in sequence
   datasetconverter.py
      -Converts a dataset with strings as values to numbers, as Hornby&Hu handle datasets with strings



How to run:

1. Install requirements.txt
2. Run Main.py

Troubleshooting # TODO add error codes
   HornbyHu does not accept strings as values
   low glm iterations can cause floating point errors potentially halting the code
   attribute names can only be alphanumeric
   real data and synthetic data needs to be the same size   