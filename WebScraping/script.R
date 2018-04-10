# Copy data from URL's
install.packages("XML")
library(XML)
theURL <- "http://www.lyit.ie/organisation/staffdirectory/staffdirectory.aspx?departmentID=D202"
staff_list <- readHTMLTable(theURL, which = 1, header = TRUE, stringAsFactors = FALSE)
staff_list

# Example of a simpleweb scrape
library(rvest)
f1 <- read_html("http://www.bbc.com/sport/formula1/43702834")
class (f1)
f1

f1 %>% html_nodes('ul') %>% html_nodes('span')
f1 %>% html_nodes('.cmts-list')

# More detailed we bscrape
install.packages("rvest")
library(rvest)

#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2017,2017&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage, '.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data <- as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage, '.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)


#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)



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