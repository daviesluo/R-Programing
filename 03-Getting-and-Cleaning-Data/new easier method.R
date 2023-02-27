read_test_x <- read.table("X_test.txt")
read_train_X <- read.table("X_train.txt")
read_subject_test <- read.table("subject_test.txt")
read_subject_train <- read.table("subject_train.txt")
read_train_X$Subject <- read_subject_train["V1"]
read_test_x$Subject <- read_subject_test["V1"]
read_test_Y <-  read.table("y_test.txt")
read_train_Y <- read.table("y_train.txt")
read_test_x$ActivityLabels <- read_test_Y[["V1"]]
read_train_X$ActivityLabels <- read_train_Y[["V1"]]
table_test_x <- data.table(read_test_x)
table_train_x <- data.table(read_train_X)
read_table_X <- merge.data.table(table_test_x,table_train_x,all.x = TRUE,all.y = TRUE)
features <- read.table("features.txt")
names(read_table_X)[1:561] <- features[["V2"]]
read_table_X$Activity[read_table_X$ActivityLabels=="1"]<- "WALKING"
read_table_X$Activity[read_table_X$ActivityLabels=="2"]<- "WALKING_UPSTAIRS"
read_table_X$Activity[read_table_X$ActivityLabels=="3"]<- "WALKING_DOWNSTAIRS"
read_table_X$Activity[read_table_X$ActivityLabels=="5"]<- "STANDING"
read_table_X$Activity[read_table_X$ActivityLabels=="4"]<- "SITTING"
read_table_X$Activity[read_table_X$ActivityLabels=="6"]<- "LAYING"
g<- as.data.frame(read_table_X)
read_subset_X <-  g[,colnames(g) %like% "-std()"| colnames(g) %like% "-mean()"|colnames(g) %like% "Subject" |colnames(g)== "Activity"]
new_tidy_Dataset <- aggregate(. ~ Subject+Activity, data = read_subset_X, FUN = mean)
new_tidy$Activity <- as.factor(new_tidy$Activity)
new_tidy$Subject <- as.factor(new_tidy$Subject)
new_tidy <- data.table(new_tidy_Dataset)
write.table(new_tidy,"table4.txt")

