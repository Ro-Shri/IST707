library(tidyverse)
library(tidytext)
library(textstem)
library(qdap)
library(caret)
library(scales)
library(wordcloud)
library(SnowballC)
library(arules)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext)
library(plyr)
library(ggplot2)
library(tm)
library(text2vec)
library(rpart)
require(rpart.plot)
library(e1071)

install.packages("e1071")
library(superml)

train_df <- read_csv("C:\\Users\\shriv\\Downloads\\spooky-author-identification\\train\\train.csv")
test_df <- read_csv("C:\\Users\\shriv\\Downloads\\spooky-author-identification\\test\\test.csv")
train_df
train_df$author <- factor(train_df$author)
str(train_df$author)
table(train_df$author)
train_label <- train_df$author
test_label <- test_df$author

train_corp <- VCorpus(VectorSource(train_df$text))

train_corp_clean <- tm_map(train_corp, content_transformer(tolower))
train_corp_clean <- tm_map(train_corp_clean, removeNumbers)
train_corp_clean <- tm_map(train_corp_clean, removeWords, stopwords())
train_corp_clean <- tm_map(train_corp_clean, removePunctuation)

train_dtm <- DocumentTermMatrix(train_corp_clean)

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

smp_size <- floor(0.75 * nrow(train_df))

smp_size

sms_dtm_train <- train_dtm[1:14684, ]
sms_dtm_test <- train_dtm[14685:19568, ]

sms_train_labels <- train_dtm[1:14684, ]$author
sms_test_labels <- train_dtm[14685:19568,]$author

freq_train <- findFreqTerms(sms_dtm_train, 5)
str(freq_train)

#freq_train_words <- train_dtm[ , freq_train]

dtm_freq_train <- sms_dtm_train[ , freq_train]
dtm_freq_test <- sms_dtm_test[ , freq_train]

sms_train <- apply(dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(dtm_freq_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(sms_train, train_label)

#just the text in the train dataset and dropping the ID column for the other
text_train_df <- train_df$text
noID_train <- subset(train_df, select=-c(id))
summary(train_df)

#count of rows per author
count(train_df$author)

author_count <- ggplot(data=train_df, aes(x=author))+geom_bar(stat="count", fill = "blue")

author_count

#countvectorize the train dataset
#cv<-CountVectorizer$new(remove_stopwords = TRUE)
#cv_train<-cv$fit_transform(train)

##Make Train and Test sets
smp_size <- floor(0.25 * nrow(train_df))

set.seed(11) # Set Seed so that same sample can be reproduced in future also

train_ind <- sample(seq_len(nrow(train_df)), size = smp_size)

train <- train_df[train_ind, ]

#using vectorization
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)
test <- noID_train[-train_ind, ]

vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)

vocab = create_vocabulary(it_train, stopwords = "english")
vocab

TrainTree <- rpart(id ~ ., data = dtm_train, method="class", control=rpart.control(cp=0, minsplit = 5, maxdepth = 10))
summary(TrainTree)
rsq.rpart(TrainTree)


vocab %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds