# Untuk nyoba scraping data review pada website tripadvisor sebelum diterapkan pada shiny
# Penerapan pada shiny dapat dilihat di ../HotelReview/helper/scrapeHotelReview.R

# Import library
library(rvest)

# Ambil nama hotel dan url nta
baseUrl <- "https://www.tripadvisor.com"
hotelsUrl <- "/Restaurants-g14782503-Yogyakarta_Yogyakarta_Region_Java.html"
url <- paste(baseUrl, hotelsUrl, sep = "")
webpage <- read_html(url)

restaurantName <- webpage %>% html_nodes('.wQjYiB7z') %>% html_text()
restaurantURL <- webpage %>% html_nodes('._15_ydu6b') %>% html_attr('href')

restaurants <- data.frame(name = restaurantName, link = restaurantURL, stringsAsFactors = FALSE)

View(restaurants)

# set direktori untuk simpan data
setwd("D:/Document/Informatika/Tugas/Data Science/Project/TripAdvisor")
# simpan data
saveRDS(restaurants, "restaurants.rds")



# Cara ngambil semua review dari restaurant pertama
restaurants$name[1]
reviewUrl <- paste(baseUrl, restaurants$link[1], sep = "")
reviewPage <- read_html(reviewUrl)

review <- reviewPage %>%
  html_nodes('.entry .partial_entry') %>%
  html_text()

reviews <- character()
reviews <- c(reviews, review)

nextPage <- reviewPage %>%
  html_nodes('.next') %>%
  html_attr('href')

while (!is.na(nextPage)) {
  reviewUrl <- paste(baseUrl, nextPage, sep = "")
  reviewPage <- read_html(reviewUrl)
  
  review <- reviewPage %>%
    html_nodes('.entry .partial_entry') %>%
    html_text()
  
  reviews <- c(reviews, review)
  
  nextPage <- reviewPage %>%
    html_nodes('.next') %>%
    html_attr('href')
}

restReview <- data.frame(reviews, stringsAsFactors = FALSE)
