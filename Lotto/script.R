# Lotto example using datasets on Blackboard
# Data up to end of 2017

# All data files are downloaded from blackboard to c:/lotto

# always necessary to examine data
# Examine all files in the c:/lotto folder and 
# store list of all csv lotto files in charcater vector
csv_file_list <- list.files(path = "c:/lotto", pattern = "*.csv")
csv_file_list
class(csv_file_list)

# Alternative - and better method
# Store all data in a folder in project
# Note that folder location is a subfolder of current working directory
# This line amalgamates 2 strings
file_location <- paste(getwd(), "/Data", sep = "")
file_location
csv_file_list <- list.files(path =  file_location, pattern = "*.csv")
csv_file_list
class(csv_file_list)

# We can also get location path by browing to it
# using the choose.files() function to 
# execute the file dialog window
location_path <- choose.files(default = "", caption = "Select files",
             multi = TRUE, filters = Filters,
             index = nrow(Filters))
location_path


#Function that reads in all csv files into one data frame and returns the result.
combine.results <- function(file_list, file_location) {
    #Initialise all_lotto_data variable
    #Note that it hasn't been assigned to a specific type yet
    all_lotto_data <- NULL
    for (csv_file in file_list) {
        #Read each of the csv files in turn and skip the first line as it
        #contains a header line
        lotto_data <- read.csv(header = TRUE, paste("c:/lotto/", csv_file, sep = ""), stringsAsFactors = FALSE)
        #Only select attributes we're interested in
        #We don't need the first attribute
        data_of_interest <- lotto_data[1:length(lotto_data)]
        # append to the allCrimeData data frame
        all_lotto_data <- rbind(all_lotto_data, data_of_interest)
    }
    return(all_lotto_data)
}

#Call the function and return the result to a data frame
my_lotto_data <- combine.results(csv_file_list)
#show the contents of my_lotto_data
my_lotto_data
str(my_lotto_data)

# Remove unnecessary primary key
my_lotto_data <- my_lotto_data[1:length(my_lotto_data)]
str(my_lotto_data)

# Reformat Date field
str(my_lotto_data$Date)
my_lotto_data$Date <- as.Date(my_lotto_data$Date)

# Rename attribute data
col_names <- c("PK", "Date", "Ball1", "Ball2", "Ball3", "Ball4", "Ball5", "Ball6", "Bonus")
colnames(my_lotto_data) <- col_names
str(my_lotto_data)

#Save the contents of my_lotto_data to a csv file called "ld.csv"
write.csv(my_lotto_data, file = "ld.csv", quote = FALSE, na = "", row.names = FALSE)

# Examine lotto data for missing values
# First row contains headers
library(mice)
md.pattern(my_lotto_data)


library(VIM)
aggr(my_lotto_data, prop = FALSE, numbers = TRUE)

# Show matrixplot of lotto data - red = missing
matrixplot(my_lotto_data)

#Check if there's any NA's in the lotto data
my_na <- my_lotto_data[!complete.cases(my_lotto_data),]
my_na #Every TRUE value shows corresponding element is NA

#Show structure of my_na
dim(my_na)

final_my_lotto_data <- na.omit(my_lotto_data)
dim(final_my_lotto_data)
final_my_lotto_data

install.packages("Hmisc")
library(Hmisc)
my_lotto_vars <- c("Ball1", "Ball2", "Ball3", "Ball4", "Ball5", "Ball6", "Bonus")
describe(my_lotto_data[my_lotto_vars])
# GMD = Gini's mean difference - measure of absolute difference between pairs of observations
? describe
summary(my_lotto_data)

# Alternate way to get mean number for first ball
mean_ball <- mean(my_lotto_data$`Ball 1`)
mean_ball
# Max number - largest value
max_ball1 <- max(my_lotto_data$`Ball 1`)
max_ball1

# Using group manipulation
library(magrittr)
lotto_set <- select(my_lotto_data, my_lotto_vars)
lotto_set <- tbl_df(lotto_set)
attach(lotto_set)
lotto_set
summarise(lotto_set, MeanNo = mean(lotto_set$`Ball 1`))
lotto_set %>% summarise(AvgNo = mean(Ball1))

# plot data
hist(my_lotto_data$Ball1, main = "Lotto results for Ball 1", )

plot(my_lotto_data$Ball1 ~ my_lotto_data$Date, data = my_lotto_data)



str(my_lotto_data)
hist(my_lotto_data$`Ball 1`, main="Lotto histogram", xlab="number")
attach(my_lotto_data)
plot(my_lotto_data$`Ball 1`, my_lotto_data$PK)