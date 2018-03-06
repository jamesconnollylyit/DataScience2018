# Lotto example using datasets on Blackboard
# Data up to end of 2017

#All data files are downloaded from blackboard to c:/lotto

#Examine all files in the c:/lotto folder and 
#store list of all csv lotto files in charcater vector
csv_file_list <- list.files(path = "c:/lotto", pattern = "*.csv")
csv_file_list
class(csv_file_list)

#Function that reads in all csv files into one data frame and returns the result.
combine.results <- function(file_list) {
    #Initialise lotto_data variable
    #Note thtat it hasn't been assigned to a specific type yet
    all_lotto_data <- NULL
    for (csv_file in file_list) {
        #Read each of the csv files in turn and skip the first line as it
        #contains a header line
        lotto_data <- read.csv(header = TRUE, paste("c:/lotto/", csv_file, sep = ""), stringsAsFactors = FALSE)
        #Only select attributes we're interested in
        #We don't need the first attribute
        data_of_interest <- lotto_data[1:8]
        # append to the allCrimeData data frame
        all_lotto_data <- rbind(all_lotto_data, data_of_interest)
    }
    return(all_lotto_data)
}

#Call the function and return the result to a data frame
my_lotto_data <- combine.results(csv_file_list)
#show the contents of my_lotto_data
my_lotto_data

#Save the contents of my_lotto_data to a csv file called "ld.csv"
write.csv(my_lotto_data, file = "ld.csv", quote = FALSE, na = "", row.names = FALSE)

plot(my_lotto_data)

#Check if there's any NA's in the lotto data
my_na <- my_lotto_data[!complete.cases(my_lotto_data),]
my_na #Every TRUE value shows corresponding element is NA

#Show structure of my_na
dim(my_na)


final_my_lotto_data <- na.omit(my_lotto_data)
dim(final_my_lotto_data)
final_my_lotto_data



any_na