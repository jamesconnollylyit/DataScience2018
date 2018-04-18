# Example of a detailed web scrape from IMDB.com website
# See Data Input slides for additional information
# on Blackboard

install.packages("rvest")
library(rvest)

# Specifying url of website to be scrapped
url <- 'https://www.imdb.com/search/title?release_date=2017-01-01,2017-12-31&count=250'

# Reading the HTML code from the website
web_page <- read_html(url)

# Quick look at the contents of web_page
head(web_page)

str(web_page)

# Using HTML tag to scrape the rankings section - IMDB site is 
# examined for this information first
rank_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > h3 > span.lister-item-index.unbold.text-primary')
head(rank_data_html, 10)
# should have 250 records
length(rank_data_html)

# Converting the ranking data from HTML to text
rank_data <- html_text(rank_data_html)

# Let's have a look at the rankings data
head(rank_data, 10)

# Data-Preprocessing: Converting rankings from string to numeric
rank_data <- as.numeric(rank_data)

# Let's have another look at the rankings data
head(rank_data, 10)

# Scrape the title section using the HTML data
# copied from "Copy selector" option within "Inspect" in Google Chrome
title_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > h3 > a')

# Converting the title section data to text
title_data <- html_text(title_data_html)

# Let's have a look at the title
head(title_data, 10)

# Scrape the description section
description_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > p:nth-child(4)')

description_data <- html_text(description_data_html)
# Examine the description data
head(description_data, 10)

# Converting the description data to text
description_data <- html_text(description_data_html)
# Delete all text after "\n". Replaces all matches of a string
# with a string vector of same length
description_data <- gsub("\n *", "", description_data)

# Alternatively we can scrape all of the text from the description section
# and then use gsub to remove unneeded portions of text
# Ignore this section for now ---------------------------------------------

# Delete all text with punctuation included
# See http://www.endmemo.com/program/R/gsub.php for details
description_data <- gsub("[[:punct:]]", "", description_data)

# Delete all text with "Votes"
description_data <- gsub("Votes", "", description_data)

# Delete all text with "201*"
description_data <- gsub("201.*", "", description_data)

# Delete all text with "Gross"
description_data <- gsub("Gross", "", description_data)

# Delete all text with a number in it - eg PG12A
description_data <- gsub(".*[0-9]", "", description_data)

description_data <- gsub(".* min ", "", description_data)

#-------------------------------------------------------------------

# Let's have a look at the description data
head(description_data, 50)
length(description_data)
str(description_data)
summary(description_data)
class(description_data)


# ---------------- ignore for now -----------------------------------
# We can use the function below to replace missing values with 
# NA's as specified in the "missing_char_data" vector
# missing_char_data <- c(2, 6, 8)
# description_data <- replace_missing_data(description_data, missing_char_data)


replace_missing_data <- function(variable_to_examine, missing_vector) {
    for (record in missing_vector) {
        # Create a vector of size missing data -1
        # since the vector index starts at 0
        missing_records <- variable_to_examine[1:(record - 1)]
        append_data <- variable_to_examine[record:length(variable_to_examine)]
        variable_to_examine <- append(missing_records, list("NA"))
        variable_to_examine <- append(variable_to_examine, append_data)
        variable_to_examine <- as.character(variable_to_examine)
    }
    return(variable_to_examine)
}

# ---------------------------------------------------------------------

# Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > p:nth-child(2) > span.runtime')

# Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

# Let's have a look at the runtime
head(runtime_data, 10)

# "min" text inside running time values are going to be a problem
# Remove it using gsub function. And convert it using the as.numeric convertor
runtime_data <- gsub(" min", "", runtime_data)
runtime_data <- as.numeric(runtime_data)

# Let's have another look at the runtime data
head(runtime_data, 10)
length(runtime_data) # 1 value missing

# Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > p:nth-child(2) > span.genre')

# Converting the genre data to text
genre_data <- html_text(genre_data_html)

# Let's have a look at the genre data
head(genre_data, 10)

# We need to tidy the genre data to remove "\n" control character
# and to remove additional spaces after each ","
# Do this using gsub() function
#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

# Data-Preprocessing: removing excess spaces
# found at the end of the genre data
genre_data<-gsub(" ","",genre_data)

# Now let's examine the genre_data
head(genre_data, 10)

# There are multiple genres for each film.
# We only need to have the first genre so we can use gsub() and a wildcard to remove
# all text after the first comma
genre_data <- gsub(",.*", "", genre_data)
length(genre_data)
# Now let's examine the genre_data
head(genre_data, 10)

# Scraping the IMDB rating section
rating_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > div > div.inline-block.ratings-imdb-rating > strong')

# Converting the ratings data to text
rating_data <- html_text(rating_data_html)

# Let's have a look at the ratings data
head(rating_data)

# Data-Preprocessing: converting ratings to numerical values
rating_data <- as.numeric(rating_data)

# Let's have another look at the ratings data
head(rating_data, 10)
length(rating_data)

# Scraping the directors section
directors_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > p:nth-child(5)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data, 10)

#Data-Preprocessing: converting directors data into factors
directors_data <- as.factor(directors_data)
# Check the length of the directors data
length(directors_data)

# Scraping the actors section
actors_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > p:nth-child(5) > a:nth-child(3)')

# Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

# Let's have a look at the actors data
head(actors_data, 10)

# Data-Preprocessing: convert the actors data into factors
actors_data <- as.factor(actors_data)
length(actors_data)

# Scrape the metascore section
metascore_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > div > div.inline-block.ratings-metascore > span')

#Converting the data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore 
head(metascore_data, 10)
# Problem with the metascore data length
length(metascore_data)

# Data-Preprocessing: removing extra space in metascore data
metascore_data <- gsub(" ", "", metascore_data)
head(metascore_data, 100)

# Data should contain 250 values (50 movies)
length(metascore_data)
summary(metascore_data)
# There's lots of metascore data missing and it is not NA data
na_count <- is.na(metascore_data)
sum(na_count)

# Through analysis, this data is missing
missing_data <- c(1, 4, 6, 10, 16, 17, 23, 25, 29, 33, 37, 43, 47, 49, 50, 52, 57:59, 64:66, 73, 74, 77, 78, 83, 85, 87, 89:91, 94, 95, 97, 100, 103, 105112, 113, 115, 117, 118, 120:122, 130, 136:138, 141, 147, 149, 152, 158:160, 162, 164:166, 169, 170, 173, 178, 183, 184, 189, 193, 197:199, 205:207, 209:212, 220:222, 225, 227, 228, 231, 233, 235, 240, 241, 247, 249)

length(missing_data)

# This function is copied from above
replace_missing_data <- function(variable_to_examine, missing_vector) {
    for (record in missing_vector) {
        # Create a vector of size missing data -1
        # since the vector index starts at 0
        missing_records <- variable_to_examine[1:(record - 1)]
        append_data <- variable_to_examine[record:length(variable_to_examine)]
        variable_to_examine <- append(missing_records, list("NA"))
        variable_to_examine <- append(variable_to_examine, append_data)
        variable_to_examine <- as.character(variable_to_examine)
    }
    return(variable_to_examine)
}

metascore_data <- replace_missing_data(metascore_data, missing_data)
head(metascore_data, 10)
str(metascore_data)

# Scrape the gross revenue section
gross_data_html <- html_nodes(web_page, '#main > div > div > div.lister-list > div:nth-child(n) > div.lister-item-content > p.sort-num_votes-visible > span:nth-child(5)')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the gross data
head(gross_data, 10)
length(gross_data)

# Data-Preprocessing: removing 'M' sign
gross_data <- gsub("M", "", gross_data)
head(gross_data, 10)

# Data-Preprocessing: removing '$' sign with substring
gross_data <- substring(gross_data, 2, 7)

#Let's check the length of gross data
# Should be 50 - lots of data missing
length(gross_data)
summary(gross_data)

# Data-Preprocessing: converting gross to numerical
gross_data <- as.numeric(gross_data)

# Now lets combine all of the vectors into a data frame
movies <- data.frame(rank = rank_data, Title = title_data, Desc = description_data, Genre = genre_data, Runtime = runtime_data, Rating = rating_data )
head(movies, 60)
length(description_data)
summary(movies)
str(movies)
movies

# Show runtime versus rating for scraped movies
?plot
plot(movies$Runtime, movies$Rating, type = "p")
plot(movies$Genre, movies$Rating, type = "p")



# Web scrape
# General-purpose data wrangling
install.packages("tidyverse")
library(tidyverse)

# Parsing of HTML/XML files  
install.packages("rvest")
library(rvest)

# String manipulation
install.packages("stringr")
library(stringr)

# Verbose regular expressions
install.packages("rebus")
library(rebus)

# Eases DateTime manipulation
install.packages("lubridate")
library(lubridate)

url <- 'http://www.trustpilot.com/review/www.amazon.com'


get_last_page <- function(html) {

    pages_data <- html %>%
    # The '.' indicates the class
    html_nodes('.pagination-page') %>%
    # Extract the raw text as a list
    html_text()

    # The second to last of the buttons is the one
    pages_data[(length(pages_data) - 1)] %>%
    # Take the raw string
    unname() %>%
    # Convert to number
    as.numeric()
}

#first_page <- read_html(url)
#(latest_page_number <- get_last_page(first_page))

#list_of_pages <- str_c(url, '?page=', 1:latest_page_number)
#list_of_pages

get_reviews <- function(html) {
    html %>%
    # The relevant tag
    html_nodes('.review-info__body__text') %>%
        html_text() %>%
    # Trim additional white space
    str_trim() %>%
    # Convert the list into a vector
    unlist()
}

get_reviewer_names <- function(html) {
    html %>%
        html_nodes('.consumer-info__details__name') %>%
        html_text() %>%
        str_trim() %>%
        unlist()
}

get_review_dates <- function(html) {

    status <- html %>%
                  html_nodes('time') %>%
    # The status information is this time a tag attribute
    html_attrs() %>%
    # Extract the second element
    map(2) %>%
                  unlist()

    dates <- html %>%
                  html_nodes('time') %>%
                  html_attrs() %>%
                  map(1) %>%
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%
                  unlist()

    # Combine the status and the date information to filter one via the other
    return_dates <- tibble(status = status, dates = dates) %>%
    # Only these are actual reviews
    filter(status == 'ndate') %>%
    # Select and convert to vector
    pull(dates) %>%
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00')

    # The lengths still occasionally do not lign up. You then arbitrarily crop the dates to fit
    # This can cause data imperfections, however reviews on one page are generally close in time)

    length_reviews <- length(get_reviews(html))

    return_reviews <- if (length(return_dates) > length_reviews) {
        return_dates[1:length_reviews]
    } else {
        return_dates
    }
    return_reviews
}

get_review_dates <- function(html) {

    status <- html %>%
                  html_nodes('time') %>%
    # The status information is this time a tag attribute
    html_attrs() %>%
    # Extract the second element
    map(2) %>%
                  unlist()

    dates <- html %>%
                  html_nodes('time') %>%
                  html_attrs() %>%
                  map(1) %>%
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%
                  unlist()

    # Combine the status and the date information to filter one via the other
    return_dates <- tibble(status = status, dates = dates) %>%
    # Only these are actual reviews
    filter(status == 'ndate') %>%
    # Select and convert to vector
    pull(dates) %>%
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00')

    # The lengths still occasionally do not lign up. You then arbitrarily crop the dates to fit
    # This can cause data imperfections, however reviews on one page are generally close in time)

    length_reviews <- length(get_reviews(html))

    return_reviews <- if (length(return_dates) > length_reviews) {
        return_dates[1:length_reviews]
    } else {
        return_dates
    }
    return_reviews
}

get_star_rating <- function(html) {

    # The pattern you look for: the first digit after `count-`
    pattern = 'count-' %R% capture(DIGIT)

    ratings <- html %>%
        html_nodes('.star-rating') %>%
        html_attrs() %>%
    # Apply the pattern match to all attributes
    map(str_match, pattern = pattern) %>%
    # str_match[1] is the fully matched string, the second entry
    # is the part you extract with the capture in your pattern  
    map(2) %>%

        unlist()

    # Leave out the first instance, as it is not part of a review
    ratings[2:length(ratings)]
}

get_data_table <- function(html, company_name) {

    # Extract the Basic information from the HTML
    reviews <- get_reviews(html)
    reviewer_names <- get_reviewer_names(html)
    dates <- get_review_dates(html)
    ratings <- get_star_rating(html)

    # Combine into a tibble
    combined_data <- tibble(reviewer = reviewer_names,
                              date = dates,
                              rating = ratings,
                              review = reviews)

    # Tag the individual data with the company name
    combined_data %>%
        mutate(company = company_name) %>%
        select(company, reviewer, date, rating, review)
}

get_data_from_url <- function(url, company_name) {
    html <- read_html(url)
    get_data_table(html, company_name)
}

scrape_write_table <- function(url, company_name) {

    # Read first page
    first_page <- read_html(url)

    # Extract the number of pages that have to be queried
    latest_page_number <- get_last_page(first_page)

    # Generate the target URLs
    list_of_pages <- str_c(url, '?page=', 1:latest_page_number)

    # Apply the extraction and bind the individual results back into one table, 
    # which is then written as a tsv file into the working directory
    list_of_pages %>%
    # Apply to all URLs
    map(get_data_from_url, company_name) %>%
    # Combine the tibbles into one tibble
    bind_rows() %>%
    # Write a tab-separated file
    write_tsv(str_c(company_name, '.tsv'))
}
url
scrape_write_table(url, 'amazon')

amz_tbl <- read_tsv('amazon.tsv')
#tail(amz_tbl, 5)