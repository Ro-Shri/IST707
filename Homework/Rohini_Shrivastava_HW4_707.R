
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

install.packages("factoextra")
install.packages("SnowballC")

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
FedCorpMatrix <- as.matrix(FedCorpDF)
(FedCorpMatrix[1:11,1:10])

Disputed <- FedCorpMatrix[1:11,]
Hamilton <- FedCorpMatrix[12:62,]
HM <- FedCorpMatrix[63:65,]
Jay <- FedCorpMatrix[66:70,]
Madison <- FedCorpMatrix[70:85,]

WordFreq <- colSums(as.matrix(FedCorpDF))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])
barplot(WordFreq[tail(ord)])
Freq_Words<-findFreqTerms(FedCorpDF)
(Row_Sum_Per_doc <- rowSums((as.matrix(FedCorpDF))))
FedMatrix <- as.matrix(FedCorpDF)
FedMatrix_N1 <- apply(FedMatrix, 1, function(i) round(i/sum(i),3))
FedMatrix_Norm <- t(FedMatrix_N1)

(FedMatrix[c(1:11),c(1000:1010)])
(FedMatrix_Norm[c(1:11),c(1000:1010)])
(FedMatrix_Norm[c(1:11),c(4620:4630)])
Fed_Paper_Matrix = as.matrix(FedCorpDF)
str(Fed_Paper_Matrix)
(Fed_Paper_Matrix[c(1:11),c(2:10)])

Papers_DF <- as.data.frame(as.matrix(Papers_DTM))
str(Papers_DF)
(Papers_DF$abolit)
(nrow(Papers_DF)) ## Each row is Paper

m <- Fed_Paper_Matrix
m_norm <- FedMatrixatrix_Norm
(m_norm[c(1:11),c(4626:4628)])

dist_mat <- dist(m, method="euclidean")

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')


km.res <- kmeans(Fed_Paper_Matrix, , nstart = 50)
km.res


aggregate(Fed_Paper_Matrix, by=list(cluster=km.res$cluster), mean)

km.res$centers
clusters_k <- data.frame(Fed_Paper_Matrix, km.res$cluster)
View(clusters_k)
clusters_k

summary(km.res)

km.res$cluster