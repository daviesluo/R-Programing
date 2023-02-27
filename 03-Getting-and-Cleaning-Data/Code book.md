we stored the data sets under the following names

 X_test.txt <-    read_test_x
 
 X_train.txt <-   read_train_X
 
 subject_test.txt <- read_subject_test
  
 subject_train.txt <- read_subject_train
    
 y_test.txt <- read_test_Y.txt
    
 y_train.txt <- read_train_Y
 
 features.txt <- features
 
 merged main datasets <- read_table_X
 
 subset dataframe containing mean and standard deviation  <- read_subset_X
 
 aggregated form of tidy data set <- new_tidy_Dataset.
  
 transformations used 
  1. reading of general "files.txt" <- read.table()
  2. naming of columns in the merged data sets <- names(merged data set)
  3. conversion of dataframe to datatable <- data.table(file)
  4. merging of the main datasets <- merge.data.table(,all.x= TRUE, all.y=TRUE)
  5. subset of mean and standard deviation <- use of like function=g[,colnames(g) %like% "-std()"| colnames(g) %like% "-mean()"|colnames(g) %like% "Subject"]
  6. aggregation of values <- aggregate()
  
