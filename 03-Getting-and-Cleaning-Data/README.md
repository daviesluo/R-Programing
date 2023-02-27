# cleaning-data-assignment
For the initial part of my analysis,

we stored the dataset X_test.txt in read_test_x which is our main test data set

we stored the dataset X_train.tx in read_train_X which is our main train data set

we stored the subjects datasets into respective variables (read_subject_train,read_subject_test)

we then created a new column in respective main datasets and stored the subject data sets in them.

we stored the subjects datasets into respective variables (read_test_Y,read_subject_train_Y)

we then created a new column in respective main datasets and stored the Activity data sets in them.

we converted the main dataframes into data table 

we merge the two main data by the use of merge function.

we stored the feature values into the file features.txt

we rename the merge table according to the feature values.

we label each row according to the activity label.

we search for the terms such as mean and stats by use of condition "like" function in the normal column index.

we subset those terms in a separate a tidy data set named "read_subset_X"

then we use aggregate function to store the mean and standard deviation of every activity on the tidy dataset.
