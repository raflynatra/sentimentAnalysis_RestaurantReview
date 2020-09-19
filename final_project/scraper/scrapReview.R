library(rvest)

# get_restaurant_reviews return data frame contains reviews and authors
baseUrl <- "https://www.tripadvisor.com"

get_resto_reviews <- function(restoUrl, incProgress = NULL) {
  
  withProgress(message = "Collecting data ", value = 0, {
    
    reviewPage <- read_html(paste(baseUrl, restoUrl, sep = ""))
    review <- reviewPage %>%
      html_nodes('.quote + .prw_reviews_text_summary_hsx') %>%
      html_text()
    reviewer <- reviewPage %>%
      html_nodes('.info_text>div') %>%
      html_text()
    
    reviews <- character()
    reviewers <- character()
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    
    nextPage <- reviewPage %>%
      html_nodes('.mobile-more .next') %>%
      html_attr('href')
    
    while (!is.na(nextPage) && length(reviews) < 500) {
      incProgress(10/length(reviews), detail = paste(length(reviews), " data"))
      print(paste(length(reviews), "data", "collected"))
      
      reviewUrl <- paste(baseUrl, nextPage, sep = "")
      reviewPage <- read_html(reviewUrl)
      
      review <- reviewPage %>%
        html_nodes('.quote + .prw_reviews_text_summary_hsx') %>%
        html_text()
      
      reviewer <- reviewPage %>%
        html_nodes('.info_text>div') %>%
        html_text()
      
      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
      
      nextPage <- reviewPage %>%
        html_nodes('.mobile-more .next') %>%
        html_attr('href')
    }
    
    totalReviews <- length(reviews)
    
    print(paste(length(reviews), "data", "collected"))
    
  })
  
  return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE))
}
