# Example of a detailed web scrape from IMDB.com website
install.packages("rvest")
library(rvest)

# Specifying url of website to be scrapped
url <- 'https://www.imdb.com/search/title?release_date=2017-01-01.2017-12-31'

# Reading the HTML code from the website
web_page <- read_html(url)


# Quick look at the contents of web_page
head(web_page)
str(web_page)

# Using CSS selectors to scrap the rankings section - IMDB site is 
# examined for this information first
rank_data_html <- html_nodes(web_page, '.text-primary')
head(rank_data_html, 10)
# should have 50 records
length(rank_data_html)

# Converting the ranking data from HTML to text
rank_data <- html_text(rank_data_html)

# Let's have a look at the rankings data
head(rank_data, 10)

# Data-Preprocessing: Converting rankings from string to numeric
rank_data <- as.numeric(rank_data)

# Let's have another look at the rankings data
head(rank_data, 10)

# Using selected CSS tag to scrap the title section
title_data_html <- html_nodes(web_page, '.lister-item-header a')

# Converting the title section data to text
title_data <- html_text(title_data_html)

# Let's have a look at the title
head(title_data, 10)


# Scrape the description section
description_data_html <- html_nodes(web_page, '.ratings-bar+ .text-muted')
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

description_data <- description_data[description_data == ""] <- NA

#-------------------------------------------------------------------

# Let's have a look at the description data
head(description_data, 50)
str(description_data)
summary(description_data)
class(description_data)

missing_char_data <- c(2, 6, 8)
description_data <- replace_missing_data(description_data, missing_char_data)


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

# Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(web_page, '.text-muted .runtime')

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
missing_runtime_data <- c(6)
runtime_data <- replace_missing_data(runtime_data, missing_runtime_data)


# Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(web_page, '.genre')

# Converting the genre data to text
genre_data <- html_text(genre_data_html)

# Let's have a look at the genre data
head(genre_data)

# We need to tidy the genre data to remvoe "\n" control character
# and to remove additional spaces after each ","
# Do this using gsub() function
#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces

genre_data<-gsub(" ","",genre_data)

# Now let's examine the genre_data
head(genre_data, 10)

# There are multiple genres for each film.
# We only need to have the first genre so we can use gsub() and a wildcard to remove
# all text after the first comma
genre_data <- gsub(",.*", "", genre_data)

# Now let's examine the genre_data
head(genre_data, 10)

# Scraping the IMDB rating section
rating_data_html <- html_nodes(web_page, '.ratings-imdb-rating strong')

# Converting the ratings data to text
rating_data <- html_text(rating_data_html)

# Let's have a look at the ratings data
head(rating_data)

# Data-Preprocessing: converting ratings to numerical values
rating_data <- as.numeric(rating_data)

# Let's have another look at the ratings data
head(rating_data, 10)

# Scraping the directors section
directors_data_html <- html_nodes(web_page, '.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data, 10)

#Data-Preprocessing: converting directors data into factors
directors_data <- as.factor(directors_data)

# Scraping the actors section
actors_data_html <- html_nodes(web_page, '.lister-item-content .ghost+ a')

# Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

# Let's have a look at the actors data
head(actors_data)

# Data-Preprocessing: convert the actors data into factors
actors_data <- as.factor(actors_data)

# Scrape the metascore section
metascore_data_html <- html_nodes(web_page, '.ratings-metascore')

#Converting the data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore 
head(metascore_data, 10)

# Data-Preprocessing: removing extra space in metascore data
metascore_data <- gsub(" ", "", metascore_data)
head(metascore_data, 100)

# Data-Preprocessing: removing \n
metascore_data <- gsub("\n", "", metascore_data)

# Data pre-processing: remove "metascore" from data
metascore_data <- gsub("Metascore", "", metascore_data)

# Data should contain 50 values (50 movies)
length(metascore_data)
summary(metascore_data)
# There's lots of metascore data missing and it is not NA data
na_count <- is.na(metascore_data)
sum(na_count)

# Through analysis, this data is missing
missing_data <- c(2, 3, 6:9, 11:13, 17, 19, 20, 21:28, 31:33, 35:38, 41:44, 46:49)

metascore_data <- replace_missing_data(metascore_data, missing_data)
head(metascore_data, 10)
str(metascore_data)





# Scrape the gross revenue section
gross_data_html <- html_nodes(web_page, '.ghost+ .text-muted+ span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the gross data
head(gross_data, 10)

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
movies <- data.frame(rank = rank_data, Title = title_data, Desc = description_data, Genre = genre_data, Runtime = runtime_data )
head(movies, 60)
length(description_data)
summary(movies)
str(movies)
movies






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