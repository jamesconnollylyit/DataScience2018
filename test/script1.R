colnames <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")
date_col <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99)
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5,NA, 2)
q5_col <- c(5, 5, 2,NA, 1)

my_data <- data.frame(date_col, country_col, gender_col, age_col, q1_col, q2_col, q3_col, q4_col, q5_col)
colnames(my_data) <- colnames
my_data
head(my_data)
str(my_data)
# recategorise 99 to NA
my_data$Age[my_data$Age == 99] <- NA
my_data

#Create a new column called AgeCat and categorise
my_data$AgeCat[my_data$Age >= 45] <- "Elder"
my_data$AgeCat[my_data$Age > 26 & my_data$Age <= 44] <- "Middle Aged"
my_data$AgeCat[my_data$Age <= 25] <- "Young"
my_data$AgeCat[is.na(my_data$Age)] <- "Elder"
#Show contents of my_data data frame
my_data

AgeCat <- factor(my_data$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))
AgeCat
my_data$AgeCat <- AgeCat
summary_col <- q1_col + q2_col + q3_col + q4_col + q5_col
summary_col
my_data <- data.frame (my_data, summary_col)
my_data

