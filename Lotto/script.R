# Lotto example using datasets on Blackboard

#All data files are downloaded from blackboard to c:/lotto

#Store list of all csv lotto files in charcater vector
csv_file_list <- list.files(path = "c:/lotto", pattern = "*.csv")
csv_file_list
class(csv_file_list)

#Function that reads in all csv files into one data frame and returns the result.
combine.results <- function(file_list) {
all_lotto_data <- NULL
    for (csv_file in file_list) {
        lotto_data <- read.csv(header = TRUE, paste("c:/lotto/", csv_file, sep = ""), stringsAsFactors = FALSE)
        # append to the allCrimeData data frame
        all_lotto_data <- rbind(all_lotto_data, lotto_data)
        print(all_lotto_data)
    }
    return(all_lotto_data)
}


my_lotto_data <- combine.results(csv_file_list)
my_lotto_data
write.csv(my_lotto_data, file = "ld.csv", quote = FALSE, na = "", row.names = FALSE)

