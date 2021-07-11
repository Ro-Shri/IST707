################################
#Rohini Shrivastava
#IST707
#HW 3
################################
install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxml")
library(readxl)
install.packages("dplyr")
library(dplyr)


df <- read_excel("C:/Users/shriv/Downloads/Unit1PEPData.xlsx")
df <- subset(df, select= -c(id))
str(df)
na.omit(df)
df<- unique(df)
td <- as(df, "transactions")
td

summary(td)

itemLabels(td)
associations.rules <- apriori(td, parameter = list(supp=0.001, conf=0.8,minlen=3,maxlen=10),appearance = list(rhs = c("pep=NO", "pep=YES")))
summary(associations.rules)

inspect(associations.rules[1:5])
