#Script for the daibetes dataset

#Read the diabetes file in
my_data <- read.csv("c:/diabetes-md.csv")

#What data type is my_data?
class(my_data)

#number of rows (observations) and cols (variables)
dim(my_data)

#Define the new column names
col_names <- c("Patient Name", "NI address", "Type", "Age", "Health status")
str(my_data)

#Set the new column names
colnames(my_data) <- col_names
my_data

#Configure Type to an unordered factor with 2 levels - it currently has 3 as one is recognised as NA
my_data$Daibetes.type <- factor(my_data$Daibetes.type, order = FALSE, levels = c("Type 1", "Type 2"))

#Configure status so that it is an ordered factor attribute
my_data$Status <- factor(my_data$Status, order = TRUE, levels = c("Poor", "Improved", "Excellent"))
#Look again at the structure - type now only contains 2 levels and status is now an ordered factor
str(my_data)

#Lets look at the class of each element in the data frame using the lapply function
class_list <- lapply(my_data, class)
class_list

#Lets show the class of each element in a vector using sapply
sapply(my_data, class)

#Lets extract out the names of the data frame into a new data frame called names
names <- my_data[, 1:1]
class(names)
head(names, n=10)

head(my_data)
str(my_data)

#Lets find out if there are any NA's in the data
# complete.cases returns a vector with no missing values - swapped by using the !
my_na <- my_data[!complete.cases(my_data),]
my_na #Every TRUE value shows corresponding element is NA
dim(my_na)

#Alternative way to remove all NA's from the dataset
final_df <- na.omit(my_data)
dim(final_df)
final_df
#Another method...
final_df <- my_data[complete.cases(my_data)]

