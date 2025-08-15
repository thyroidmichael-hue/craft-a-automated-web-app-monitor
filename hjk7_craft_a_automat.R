# Loading required libraries
library(RCurl)
library(XML)
library/stringr)

# Defining the website to monitor
website_url <- "https://www.example.com"

# Function to check website availability
check_availability <- function(url) {
  tryCatch(
    expr = {
      webpage <- getURL(url)
      if (statusCode(webpage) == 200) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# Function to extract website title
extract_title <- function(url) {
  webpage <- getURL(url)
  doc <- htmlParse(webpage)
  xpath <- "//title"
  title_nodes <- getNodeSet(doc, xpath)
  title <- xmlValue(title_nodes[[1]])
  return(str_trim(title))
}

# Monitoring function
monitor_website <- function(url) {
  availability <- check_availability(url)
  title <- extract_title(url)
  if (!availability) {
    print(paste(" Website is down: ", url))
  } else {
    print(paste(" Website is up: ", url, " - Title: ", title))
  }
}

# Testing the monitor function
monitor_website(website_url)