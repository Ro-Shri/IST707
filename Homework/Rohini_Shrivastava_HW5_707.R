
library(wordcloud)
library(tm)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext)
library(plyr)
library(ggplot2)
library(factoextra)
#library(mclust)
library(dplyr)
library(rpart)
require(rpart.plot)

install.packages("rattle")
library(rattle)


install.packages("factoextra")
install.packages("SnowballC")
install.packages("rpart")
setwd("C:/Users/shriv/Downloads")
FedCorp <- Corpus(DirSource("FederalPapers"))
(summary(FedCorp))

(getTransformations())

(FedCorpLen<-length(FedCorp))
FedCorpDF <- DocumentTermMatrix(FedCorp,
                                 control = list(
                                   stopwords = TRUE,
                                   wordLengths=c(3, 15),
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   remove_separators = T
                                 ))


WordFreq <- colSums(as.matrix(FedCorpDF))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])
barplot(WordFreq[tail(ord)])
Freq_Words<-findFreqTerms(FedCorpDF)
(Row_Sum_Per_doc <- rowSums((as.matrix(FedCorpDF))))

FedCorpMatrix <- as.matrix(FedCorpDF)
Papers_N1 <- apply(FedCorpMatrix, 1, function(i) round(i/sum(i),3))
Papers_Norm <- t(Papers_N1)

PaperDF <- as.data.frame(as.matrix(Papers_Norm))
PaperDF1<- PaperDF%>%add_rownames()

names(PaperDF1)[1]<-"Author"
PaperDF1[1:11,1]="Disputed"
PaperDF1[12:62,1]="Hamilton"
PaperDF1[63:65,1]="HM"
PaperDF1[66:70,1]="Jay"
PaperDF1[71:85,1]="Madison"

PaperNoDisp <- subset(PaperDF1, Author != "Disputed")
DisputedOnly <- subset(PaperDF1, Author == "Disputed")

smp_size <- floor(0.8 * nrow(PaperNoDisp))
##Make Train and Test sets

set.seed(11) # Set Seed so that same sample can be reproduced in future also

train_ind <- sample(seq_len(nrow(PaperNoDisp)), size = smp_size)

train <- PaperNoDisp[train_ind, ]
test <- PaperNoDisp[-train_ind, ]
# train / test ratio
length(sample)/nrow(PaperNoDisp)


DT1 <- rpart(Author ~ ., data = train, method="class")
summary(DT1)
DT2 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 5, maxdepth = 10))
summary(DT2)
DT3 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(minbucket = 1, minsplit = 7, maxdepth = 8))
summary(DT3)


predicted1 <- predict(DT1, test, type="class")
predicted2<- predict(DT2, test, type="class")
predicted3 <- predict(DT3, test, type="class")

fancyRpartPlot(DT1, cex=0.6, caption="Training Tree 1")
fancyRpartPlot(DT2, cex=0.6, caption="Training Tree 2")
fancyRpartPlot(DT3, cex=0.6, caption="Training Tree 3")

#confusion matrix to find correct and incorrect predictions
table1<-table(Authorship=predicted1, true=test$Author)
table2<-table(Authorship=predicted2, true=test$Author)
table3<-table(Authorship=predicted3, true=test$Author)

accuracy1 <- sum(diag(table1)) / sum(table1)
accuracy2 <- sum(diag(table2)) / sum(table2)
accuracy3 <- sum(diag(table3)) / sum(table3)

print(paste("Accuracy for Decision Tree 1:",accuracy1))
print(paste("Accuracy for Decision Tree 2:",accuracy2))
print(paste("Accuracy for Decision Tree 3:",accuracy3))

Validation1 <- predict(DT3, newdata=DisputedOnly)
Validation1


Validation2 <- predict(DT2, newdata=DisputedOnly)
Validation2