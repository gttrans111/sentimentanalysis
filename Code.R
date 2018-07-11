### Authored and compiled by Kshitiz Goel (PGDM-DCP-170201062)
##for any issues please mail: dcp17kshitizgoel@imt.ac.in

######WEB SCRAPING

library(tidyverse)     # General purpose data wrangling
library(rvest)         # Parsing of html/xml files
library(stringr)       # String manipulation
library(rebus)         # Verbose regular expressions
library(lubridate)     # Eases datetime manipulation


get_last_page <- function(html){
  
  pages_data <- html %>% 
    html_nodes('.pagination-page') %>% # The '.' indicates the class
    html_text()                        # Extracts the raw text as a list
  
  pages_data[(length(pages_data)-1)] %>%             # The second to last of the buttons is the one
    unname() %>%                                     # Take the raw string
    as.numeric()                                     # Convert to number
}

get_reviews <- function(html){
  html %>% 
    html_nodes('.review-info__body__text') %>%       # The relevant tag
    html_text() %>% 
    str_trim() %>%                       # Trims additional white space
    unlist()                             # Converts the list into a vector
}

get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.consumer-info__details__name') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}

get_review_dates <- function(html){
  
  status <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>%              # The status information is this time a tag attribute
    map(2) %>%                    # Extracts the second element
    unlist() 
  
  dates <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>% 
    map(1) %>% 
    ymd_hms() %>%                 # Using a lubridate function 
    # to parse the string into a datetime object
    unlist()
  
  return_dates <- tibble(status = status, dates = dates) %>%   # You combine the status and the date 
    # information to filter one via the other
    filter(status == 'ndate') %>%              # Only these are actual reviews
    pull(dates) %>%                            # Selects and converts to vector
    as.POSIXct(origin = '1970-01-01 00:00:00') # Converts datetimes to POSIX objects
  
  # The lengths still occasionally do not lign up. You then arbitrarily crop the dates to fit
  # This can cause data imperfections, however reviews on one page are generally close in time)
  
  length_reviews <- length(get_reviews(html))
  
  return_reviews <- if (length(return_dates)> length_reviews){
    return_dates[1:length_reviews]
  } else{
    return_dates
  }
  return_reviews
}

get_star_rating <- function(html){
  
  
  # The pattern we look for
  pattern = 'star-rating-'%R% capture(DIGIT)  
  
  vector <- html %>% 
    html_nodes('.star-rating') %>% 
    html_attr('class') %>% 
    str_match(pattern) %>% 
    map(1) %>% 
    as.numeric()  
  vector <-  vector[!is.na(vector)]
  vector
}

get_data_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  dates <- get_review_dates(html)
  ratings <- get_star_rating(html)
  
  # Minimum length
  min_length <- min(length(reviews), length(reviewer_names), length(dates), length(ratings))
  
  # Combine into a tibble
  combined_data <- tibble(reviewer = reviewer_names[1:min_length],
                          date = dates[1:min_length],
                          rating = ratings[1:min_length],
                          review = reviews[1:min_length]) 
  
  # Tag the individual data with the company name
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company, reviewer, date, rating, review)
}

get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html, company_name)
}

scrape_write_table <- function(url, company_name){
  
  # Read first page
  first_page <- read_html(url)
  
  # Extract the number of pages that have to be queried
  latest_page_number <- get_last_page(first_page)
  
  # Generate the target URLs
  list_of_pages <- str_c(url, '?page=', 1:latest_page_number)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    map(get_data_from_url, company_name) %>%  # Apply to all URLs
    bind_rows() %>%                           # Combines the tibbles into one tibble
    write_tsv(str_c(company_name,'.tsv'))     # Writes a tab separated file
}



url <-'https://www.trustpilot.com/review/www.uber.com'

scrape_write_table(url, 'uber')

lyft_tbl <- read_tsv('uber.tsv')
tail(lyft_tbl, 5)
table(lyft_tbl$rating)

library(stringr)
library(ggplot2)
library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(dplyr)
#library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(wordcloud)
library(plotly)

rm(tweet_df)
clean.text = function(x)
{
  
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

cleanText <- clean.text(lyft_tbl$review)
vector <- paste(cleanText,collapse=" ")


########wordcloud
wordcloud(vector, scale=c(6,0.7), max.words=150, 
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

##########simple Sentiment

#load both files to home directory
pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

neg.words = c(neg.words, 'wtf', 'fail')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sentiment.scores= score.sentiment(cleanText, pos.words, neg.words, .progress='none')
score <- sentiment.scores$score



p <- plot_ly(x = ~score, type = "histogram")
p
topic.negative = subset(sentiment.scores, score < 0)
topic.positive = subset(sentiment.scores, score > 0)
topic.neutral = subset(sentiment.scores, score = 0)

Negative = nrow(topic.negative)
Positive = nrow(topic.positive)
Netural = nrow(topic.neutral)

dftemp=data.frame(topic=c("Negative", "Positive","Netural"), 
                  number=c(Negative,Positive,Netural))

p <- plot_ly(data=dftemp, labels = ~topic, values = ~number, type = 'pie') %>%
  layout(title = 'Pie Chart of reviews of lyft',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p


####################Prediction Model
head(lyft_tbl)
lyft_tbl$sentiment=ifelse(lyft_tbl$rating==1,'Negative',
                          ifelse(lyft_tbl$rating==2,'Negative',
                                 ifelse(lyft_tbl$rating==3,'Positive',
                                        ifelse(lyft_tbl$rating==4,'Positive',
                                               ifelse(lyft_tbl$rating==5,'Positive',
                                                      ifelse(lyft_tbl$rating==0,'Negative','NA'))))))

# 
dim(lyft_tbl)
prop.table(table(lyft_tbl$sentiment))

text = lyft_tbl[,c("rating", "review")]
head(text)

colnames(text)
library(caret)
set.seed(14)
train = createDataPartition(y=text$rating, p = 0.70, list = FALSE)
train_d = text[train,]
test_d =text[-train,]


##########################################################################################################################################
text = lyft_tbl[,c("sentiment", "review")]
head(text)

colnames(text)
library(caret)
set.seed(14)
train = createDataPartition(y=text$sentiment, p = 0.70, list = FALSE)
train_d = text[train,]
test_d =text[-train,]

train_corp = Corpus(VectorSource(train_d$review))

library(tm)
St1 = tm_map(train_corp, tolower)
St2 = tm_map(St1, removeNumbers)
St3 = tm_map(St2, removeWords, stopwords())
St4 = tm_map(St3, removePunctuation)
St5 = tm_map(St4, stripWhitespace)
St6 = tm_map(St5,stemDocument, language="english")

dtm_train = DocumentTermMatrix(St6)
dim(dtm_train)

dtm_train_spte= removeSparseTerms(dtm_train, 0.95) # the most 30 words used
dim(dtm_train_spte)                               

# Average frequency of the top 30 words - selecting in desenting oder
meantrain = sort(colMeans(as.matrix(dtm_train_spte)), decreasing= T)
meantrain[1:30]

mean30 = mean(meantrain[1:30])
mean30

barplot(meantrain[1:30], border=NA, las=3, xlab="top 30 words", ylab="Frequency", ylim=c(0, 0.5))
#another method with stopwords
mystopwords=c('lyft','driver')

train_corp2 = tm_map(train_corp, removeWords, mystopwords)
dtm_train2 = DocumentTermMatrix(train_corp2, control = list(tolower  =t, removeNumbers=T, removePunctuation=T, stopwords=T, stripWhitespace=T, stemming=T))

dim(dtm_train2)
dtm_train_spte2 = removeSparseTerms(dtm_train2, 0.95)
dim(dtm_train_spte2) 


#train and test
train_freq_bagw = as.matrix(dtm_train_spte2)
train_data_m = data.frame(y = train_d$sentiment, x = train_freq_bagw) # combining the review level with Score
dim(train_data_m)
head(train_data_m)

train_m_bagw = findFreqTerms(dtm_train_spte2)
length(train_m_bagw)
head(train_m_bagw)

test1_corp = Corpus(VectorSource(test_d$review))
test1_m_bagw = DocumentTermMatrix(test1_corp, 
                                  control = list(tolower  = T, 
                                                 removeNumbers=T, 
                                                 removePunctuation=T, 
                                                 stopwords=T, 
                                                 stripWhitespace=T, 
                                                 stemming=T, 
                                                 dictionary=train_m_bagw)) # dictionary calculates the frequency of the words from the dictionary 117

test1_freq_bagw = as.matrix(test1_m_bagw) # document transformed into matrix from a doc term matrix
#head(test1_freq_bagw)
test1_d_m = data.frame(y=test_d$sentiment, x =test1_freq_bagw)
dim(test1_d_m)

##############Descision Tree
library(party)
bagw_ctree = ctree (y ~., data = train_data_m)
bagw_ctree
library(partykit)
plot(bagw_ctree, gp=gpar(fontsize=6))
library(rminer)
test1pdction = predict(bagw_ctree, newdata=test1_d_m)

mmetric(test1pdction, test1_d_m[,1], c("ACC", "TPR", "PRECISION", "F1"))
###Naive bayes Algo
library(e1071)
bagw_nb = naiveBayes(y ~., data= train_data_m,laplace = 1)  
test1prdNB = predict(bagw_nb, newdata=test1_d_m)

str(test1prdNB)
mmetric(test1prdNB, test1_d_m[,1], c("ACC", "TPR", "PRECISION", "F1"))

table("Predictions"= test1prdNB,  "Actual" = test_d$sentiment )

