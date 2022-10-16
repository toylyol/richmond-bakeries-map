# Install necessary packages ----

## This function is courtesy of a StackOverflow user, Matthew: 
## https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

usingPackages <-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}


# Scrape web for data ----

## These functions are used in bakery_web_scraping.R

scrapeAddress <- function(css_selector){
  
  html %>%
    html_element(css = css_selector) %>%
    html_text()
}


add_name <- function(x){ stringr::str_extract(x, "^[^\\:]+") }    # extract bakery name from beginning of each paragraph


# Clean bakery data ----

## These functions are used in bakery_data_cleaning.R

extractDescription <- function(x){
  
  x <- stringr::str_extract(x, "(?<=:\\s).*")   # extract string after colon and whitespace
  x <- sub(".[^.]*$", ".", x)                   # extract string before last period and substitute period
  return(x)
  
}


extractAddress <- function(x){
  
  x <- str_extract( x,"(?<=\\s)\\d(...).*," )
  x <- paste0(x," Richmond, VA")
  return(x)
  
}