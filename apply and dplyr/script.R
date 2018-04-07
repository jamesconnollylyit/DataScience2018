# Using lapply to manipulate a list to apply the sum function
# to each element in the list
my_list <- list(a = matrix(1:9, 3), b = 1:5, c = matrix(1:4, 2), d = 2)
my_list
lapply(my_list, sum)

# mapply()
first_list <- list(a = matrix(1:9, 3), b = 1:5, c = matrix(1:16, 2), d = 2)
first_list
second_list <- list(a = matrix(1:9, 3), b = 1:7, c = matrix(1:16, 8), d = 2)
second_list

mapply(identical, first_list, second_list)

simple_function <- function(first_list, second_list) {
    # nrow cannot handle vectors, but
    # NROW treats each input as a vector and can 
    # treat each row as a vector
    NROW(first_list) + NROW(second_list)
}
mapply(simple_function, first_list, second_list)

# lapply can take a vector as an input
the_names <- c("James", "John", "Joe")
lapply(the_names, nchar)



data(diamonds) # cant find dataset - needs package =
data(diamonds, package = "ggplot2")
head(diamonds, 4) # show 4 x 10 items from diamonds dataset
dim(head(diamonds, 4)) # get dimensions of this view

# Alternate way to do this with pipes
library(magrittr)
diamonds %>% head(4) %>% dim

# Initialize x
x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

# Compute the logarithm of x, return suitable differences, 
# compute the exponential function and round the result
round(exp(diff(log(x))), 1)

# Alternative way to do this with pipes
library(magrittr)

# Perform the same computations on x as above
# No need to reload x - just showing alternate example in full format
x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

x %>% log() %>%
    diff() %>%
    exp() %>%
    round(1)

# Before using a tibble
head(diamonds)
class(diamonds)

# Install and use dplyr to access tibbles
install.packages("dplyr")
library(dplyr)
head(diamonds)
class(diamonds)

# Select example
select(diamonds, carat, price)
# with pipes alternative syntax
diamonds %>% select(carat, price)

# Or using a vector containing columns
diamonds %>% select(c(carat, price))

# If attributes are stored in a variable, pass to .dots
my_attributes <- c('carat', 'price')
select_(diamonds, .dots = my_attributes)
# pipes alternative
diamonds %>% select_(.dots = my_attributes)

# Newer method
select(diamonds, one_of(my_attributes))
# pipes alternative
diamonds %>% select(one_of(my_attributes))

# Select using square brackets
diamonds[, my_attributes]
# and using their positions in tbl
select(diamonds, 1, 7)
# pipes alternative
diamonds %>% select(1,7)

# Starts with, ends with, contains
# This shows all attributes starting with "c"
select(diamonds, starts_with("c"))
# pipes alternative
diamonds %>% select(starts_with("c"))

# Attributes ending with "e" - not case sensitive
select(diamonds, ends_with("e"))
#pipes alternative
diamonds %>% select(ends_with("e"))

# Attributes containing the letter "l"
select(diamonds, contains("l"))
# pipes alternative
diamonds %>% select(contains("l"))

# Regular expression to find any letter followed by
# a wildcard and then any letter
select(diamonds, matches("r.+t"))
# pipes alternative
diamonds %>% select(matches("r.+t"))

# Designate columns not for selection
select(diamonds, -carat, - price)
# pipes alternative
diamonds %>% select(-carat, - price)
# or by position
select(diamonds, c(-1, -7))
# pipes alternative
diamonds %>% select(-1, -7)

# Filter logical expression
filter(diamonds, cut == "Ideal")
# pipes alternative
diamonds %>% filter(cut == "Ideal")
# old method
diamonds[diamonds$cut == "Ideal",]

# Using %in% operator
filter(diamonds, cut %in% c("Ideal", "Good"))
# Pipes alternative
diamonds %>% filter(cut %in% c("Ideal", "Good"))

# Using compound filtering
filter(diamonds, carat > 2, price < 14000)
# Pipes alternative
diamonds %>% filter(carat > 2, price < 14000)
# Logical statement
filter(diamonds, carat < 1 | carat > 5)
# Pipes alternative
diamonds %>% filter(carat < 1 | carat > 5)

# Slice used to return rows of requested results
slice(diamonds, c(1:5, 8, 15:20))
# Pipes alternative
diamonds %>% slice(c(1:5, 8, 15:20))

# Mutate to add a new column and give it a name
mutate(diamonds, ratio = price/carat)
# Pipes alternative
diamonds %>% mutate(ratio = price / carat)

# We can also select some variables before mutate
# Here we need to do this in 2 lines
set <- select(diamonds, carat, price)
mutate(set, ratio = price / carat)
# Pipes alternative - 1 line
diamonds %>% select (carat, price) %>% mutate (ratio = price/carat)

# Using attributes immediately within mutate call
set <- select(diamonds, carat, price)
mutate(set, Ratio = price / carat, Double = Ratio*2)
# Pipes alternative
diamonds %>% select(carat, price) %>% mutate(Ratio = price / carat, Double = Ratio * 2)

# Using the summarise function
summarise(diamonds, mean(price))
# Pipes alternative
diamonds %>% summarise(mean(price))

# Naming results and performing multiple calculations
summarise(diamonds, AvgPrice = mean(price),
                    MedianPrice = median(price),
                    AvgCarat = mean(carat))
# Pipes alternative
diamonds %>% summarise(AvgPrice = mean(price),
                       MedianPrice = median(price),
                       AvgCarat = mean(carat))

# Group_by example
data_grouped <- group_by(diamonds, cut)
summarise(data_grouped, AvgPrice = mean(price))
# Pipes alternative
diamonds %>% group_by(cut) %>% summarise(AvgPrice = mean(price))

# Group_by with multiple attributes and summaries
data_grouped <- group_by(diamonds, cut, color)
summarise(data_grouped, AvgPrice = mean(price), SumCarat = sum(carat))
# Pipes alternative
diamonds %>%
  group_by(cut, color) %>%
  summarise(AvgPrice = mean(price), SumCarat = sum(carat))

# Arrange function
data_grouped <- group_by(diamonds, cut)
data_summary <- summarise(data_grouped, AvgPrice = mean(price), SumCarat = sum(carat))
arrange(data_summary, AvgPrice)
# Pipes alternative
diamonds %>%
  group_by(cut) %>%
  summarise(AvgPrice = mean(price), SumCarat = sum(carat)) %>%
  arrange(AvgPrice)


# Probability
# 30 random numbers between -3 to +3
x <- pretty(c(-3, 3), 30)
x

#dnorm - create a density distribution using the probability function (see notes)
y <- dnorm(x)
y

help(plot)
plot(x, y,
  type = "l", # l = lines
  xlab = "Normal Deviate",
  ylab = "Density",
  yaxs = "i",
  main = "Main heading"
)

# pnorm - calculates the area under the standard normal curve to the left of z=1.96
pnorm(1.96)

# qnorm - input the required percentile of a normal distribution with mean 500 and sd of 100
qnorm(.9, mean = 500, sd = 100)
qnorm

# rnorm - randomly generate normal deviates
# this example - randomly generate 50 numbers with mean=50
# and SD=50
rnorm(50, mean = 50, sd = 10)
rnorm

# runif() function is used to generate pseudo-random numbers from a uniform distribution on the interval 0 to 1
runif(5)
# different each time
runif(5)

# Set seed value - reproduce results
set.seed(1234)
runif(5)
# repeat
set.seed(6)
runif(0)

# Applying functions to matrices and data frames
c <- matrix(runif(12), nrow = 3)
c
log(c)
mean(c) # only provides overall mean

# We can select 
mydata <- matrix(rnorm(30), nrow = 6)
mydata
# Calculate 6 row means - indicated by margin set to 1
apply(mydata, 1, mean)

# Now calculate 5 column means - margin set to 2 to represent column calculation
apply(mydata, 2, mean)

# Calculate trimmed column means - ignore bottom and top 20% of values
apply(mydata, 2, mean, trim = 0.2)

# Solving the example problem
options(digits = 2)

# Create data frame
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose", "David Jones", "Janice Markhammer", "Cheryl Cushing", "Reuven Ytzrhak", "Greg Knox", "Joel England", "Mary Rayburn")
Maths <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18, 19)

# Set stringsAsFactors = FALSE to ensure each number vector 
# remains unchanged
roster <- data.frame(Student, Maths, Science, English, stringsAsFactors = FALSE)

# Use SD units to represent each test - standardises
# each result to make them all compatible
z <- scale(roster[, 2:4])
z

score <- apply(z, 1, mean)
score
# get a performance score for each student by calculating the row
# means using the mean() function and adding them to the roster 
# using the cbind() function:

score <- apply(z, 1, mean)
roster <- cbind(roster, score)
roster


y <- quantile(roster$score, c(.8, .6, .4, .2))
y

# Using logical operators, you can recode students’ percentile 
# ranks into a new categorical grade variable. 
# This code creates the variable grade in the roster data frame
# and creates grades using the quantile function

roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"
roster

# Now we split the Student column of data into 
# two values on " "
name <- strsplit((roster$Student), " ")
name

# sapply() function to take the first element of each component 
# and put it in a Firstname vector, and the second element of 
# each component and put it in a Lastname vector. 
# "[" is a function that extracts part of an object
# here the first or second component of the name string
Firstname <- sapply(name, "[", -1)
Firstname
Lastname <- sapply(name, "[", 2)
Lastname

# use cbind() to add these elements to the roster
# roster[,-1] removes the first column , -2  removes 2nd etc
help(cbind)
roster <- cbind(Firstname, Lastname, roster[, -1])
roster

# Sort the dataset by first and last name using the order() function
roster <- roster[order(Lastname, Firstname),]
roster
# Does not work - look at order function
# repetition and looping --------------------------------

# For
for (i in 1:10)
    print("Hello")

# Break statement

repeat {
    action <- ""
    message("Happy Groundhog Day!")
    action <- sample(
    c(
    "Learn French",
    "Make an ice statue",
    "Rob a bank",
    "Advance a day"
    ),
    1
    )
    message(" action = ", action)
    if (action == "Advance a day") break
    }




# While
i <- 10
while (i > 0) { print("Hello"); i <- i - 1 }

action <- sample(
  c(
    "Learn French",
    "Make an ice statue",
    "Rob a bank",
    "Advance a day"
  ),
  1
)
while (action != "Advance a day") {
    message("Happy Groundhog Day!")
    action <- ""
    action <- sample(
  c(
    "Learn French",
    "Make an ice statue",
    "Rob a bank",
    "Advance a day"
  ),
  1
  )
    message("action = ", action)
}

# For loops
for (i in 1:5)
    message("i = ", i)

# execute multiple expressions, as with other loops they must be surrounded
# by curly braces:

for (i in 1:5) {
    j <- i ^ 2
    message("j = ", j)
}

# particularly flexible in that they are not limited to integers, or even
# numbers in the input. We can pass character vectors, logical vectors, or lists:

for (month in month.name) {
    message("The month of ", month)
}

for (yn in c(TRUE, FALSE, NA)) {
    message("This statement is ", yn)
}

l <- list(
  pi,
  LETTERS[1:5],
  charToRaw("not as complicated as it looks"),
  list(
    TRUE
  )
)
for (i in l) {
    print(i)
}


# If-else
Eg
# Set value for grade
grade <- c(1, 2, 3, 4)
if (is.character(grade)) grade <- as.factor(grade)
if (!is.factor(grade)) grade <- as.factor(grade) else print("Grade already is a factor")


# Switch
feelings <- c("sad", "afraid")
for (i in feelings)
    print(
    switch(i,
      happy = "I am glad you are happy",
      afraid = "There is nothing to fear",
      sad = "Cheer up",
      angry = "Calm down now"
    )
  )


# Functions ----------------------------------------------------------

mystats <- function(x, parametric = TRUE, print = FALSE) {
    if (parametric) {
        centre <- mean(x);
        spread <- sd(x)
    } else {
        centre <- median(x);
        spread <- mad(x)
    }
    if (print & parametric) {
        cat("Mean=", centre, "\n", "SD=", spread, "\n")
    } else if (print & !parametric) {
        cat("Median=", centre, "\n", "MAD=", spread, "\n")
    }
    result <- list(center = centre, spread = spread)
    return(result)
}

mydate <- function(type = "long") {
    switch(type,
    long = format(Sys.time(), "%A %B %d %Y"),
    short = format(Sys.time(), "%m-%d-%y"),
    cat(type, "is not a recognised type\n")
  )
}

# Call the function 
mydate("long")
mydate("short")
mydate()
mydate("medium")

set.seed(1234)
x <- rnorm(500)
y <- mystats(x)
y

meanANDsd <- function(x) {
    av <- mean(x)
    sdev <- sd(x)
    c(mean = av, sd = sdev) # The function returns this vector 
}
# Aggregating data -------------------------------------------------------
# When you aggregate data, you replace groups of observations 
# with summary statistics based on those observations

# When you reshape data, you alter the structure 
# (rows and columns) determining how the data is organised


# Transpose a dataset
cars <- mtcars[1:5, 1:4]
cars

# Swap rows with columns
t(cars)

# Aggregate mtcars data by number of cylinders and gears
# returning means for each of the numeric variables

options(digits = 3)
attach(mtcars)
aggdata <- aggregate(mtcars, by = list(cyl, gear), FUN = mean, na.rm = TRUE)
aggdata

# Reshape2 package
ID <- c(1, 1, 2, 2)
Time <- c(1, 2, 1, 2)
X1 <- c(5, 3, 6, 2)
X2 = c(6, 5, 1, 4)

mydata <- data.frame(ID, Time, X1, X2)
str(mydata)
mydata

library(reshape2)
# Note do not use this method as it generates a duplicate column for each vector
md <- melt(mydata, id = c(ID, Time))
md

# User this method
md <- melt(mydata, id = c("ID", "Time"))
md

# Cast reshapes melted data into a new data frame 
# using a formula that you provide and an (optional) 
# function used to aggregate the data.

# Eg formula takes the form rowvar~colvar
# where rowvar defines set of variables that define the rows
# and colvar defines the set of variables that define columns

# Here, rowvar = ID (ID defines each row) and 
# colvar = variable (variable field shown in the columns)
dcast(md, ID ~ variable, mean)

#data identified by the Time variable, with 
# outputs aggregated to only show mean results
dcast(md, Time ~ variable, mean)

# data identified by ID, with columns aggregated by mean
dcast(md, ID ~ Time, mean)

# No function = no calculated mean etc. 
# Data organised by ID and then Time within
# each ID set of data. Variable field then shown in columns
dcast(md, ID + Time ~ variable)

# Data organised by ID and then variable within
# each ID set of data. Time field then shown in columns
dcast(md, ID + variable ~ Time)

# Data organised by ID 
# Variable and Time fields then shown in columns
dcast(md, ID ~ variable + Time)
