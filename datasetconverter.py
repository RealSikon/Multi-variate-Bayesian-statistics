import pandas as pd
from sklearn.preprocessing import LabelEncoder

def main(dataset_path, labels_to_encode):
   # Creating a instance of label Encoder.
   le = LabelEncoder()

   print('Converting ' + dataset_path +'.csv ' + 'to numeric')
   #importing data using .read_csv() function
   df = pd.read_csv(dataset_path + '.csv')

   for label in labels_to_encode:

      # Using .fit_transform function to fit label
      # encoder and return encoded label
      lab = le.fit_transform(df[label])

      # removing the column from df
      # as it is of no use now.
      df.replace(label, inplace=True)

      # Appending the array to our dataFrame 
      df[label] = lab

   # printing Dataframe
   num_dataset_path = dataset_path + '_num.csv'
   df.to_csv((num_dataset_path), index=False)
   print('Numeric dataset: ' + num_dataset_path)
   
   return(num_dataset_path)


if __name__ == '__main__':
    main()
