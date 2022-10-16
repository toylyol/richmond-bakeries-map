# Scrape the web for Richmond Virginia-area bakery information


# Load packages ----

library(rvest)      # part of the tidyverse, but load it separately to ensure all functions are recognized
library(robotstxt)


# Define URL ----

url <- "https://richmondmagazine.com/restaurants-in-richmond/richmond-area-bakeries/"

robotstxt::paths_allowed(url) # check to see if scraping permitted (T/F)

html <- read_html(url)


# Retrieve bakeries' data ----

## The data are in paragraphs, not a table. So, some iteration makes it easier to wrangle the data.
## Use a for loop to create a list of all the CSS selectors needed for the paragraphs of bakery info.

element_list <- list()

for ( i in 1:61 ){
  
  num <- i + 2
  
  temp <- paste0( "#content > div > p:nth-child(",num,")" )
  
  element_list[[i]] <- temp
  
}


## Add the CSS selector for the first paragraph to the list.

element_list[[62]] <- "#content > div > p.lead"


## Scrape the webpage using a custom function and the list of CSS selectors, iterating with purrr.

scrapeAddress <- function(css_selector){
  
  html %>%
    html_element(css = css_selector) %>%
    html_text()
}

raw_bakery_data <- map(element_list, scrapeAddress)


## Name elements in the list to bind_rows() 

add_name <- function(x){ stringr::str_extract(x, "^[^\\:]+") }    # extract bakery name from beginning of each paragraph

bakery_data <- set_names(raw_bakery_data, add_name)


## Create tibble ----

bakery_table <- bind_rows(bakery_data)

bakery_table <- bakery_table %>%
  pivot_longer(everything(), names_to = "bakery_name", values_to = "info")


## Save copy of scraped data ----

write.csv( bakery_table, paste0(path, "/output/raw_bakery_data.csv") )


