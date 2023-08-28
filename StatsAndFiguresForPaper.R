library(MASS)
library(tidyverse)

#clean up earlier runs
rm(list=ls())

####run settings ####

#Update the path as needed
setwd("C:/Users/Margret/Dropbox/rannsoknir/cocaine/stat_aggrigation/ForGitHub")

my_data<-read.csv("CocaineSimulated.csv",header=T)
summary(my_data)
nrow(my_data)

###clean up potency
my_data <- my_data %>% filter(Potency>0)
  
### create Figure 1
subset<-my_data %>% filter(Nt.Wt>=1 & Nt.Wt<1000 & PricePerCocGram<1000)
subset$PricePerCocGram=as.numeric(subset$PricePerCocGram)
ggplot(subset, aes(x=Nt.Wt, y=PricePerCocGram)) + geom_point() +
  labs(title="",x="Purchase weight (g)", y="Purity-adjusted price") 
  
###Create summary stats for Table 1

my_data %>% summarize(AvgPot=mean(Potency), AvgWt=mean(Nt.Wt), AvgPr=mean(Price), AvgPrPerGram = mean(as.numeric(PricePerCocGram)))
my_data %>% filter(Nt.Wt>=1 & Nt.Wt<1000 ) %>% summarize(AvgPot=mean(Potency), AvgWt=mean(Nt.Wt), AvgPr=mean(Price), AvgPrPerGram = mean(as.numeric(PricePerCocGram)))


## Create Figure 3a
my_data<-read.csv("ExampleOutputFromStep3a.csv",header=T)
summary(my_data)

ggplot(my_data, aes(x=Iterations.Anecdotal)) +
  ggtitle("Variation in Explanatory Power of the Inferred Dataset", subtitle="Measure Used: % of Anecdotal Links also in Inferred Dataset
") +
  geom_line(mapping= aes(y=Percentage.of.All.Anecdotal.Links.Identified.by.Inferred.Links)) + # Divide by 10 to get the same range than the temperature
  geom_point(mapping = aes(x = Iterations.Anecdotal, y = Percentage.of.All.Anecdotal.Links.Identified.by.Inferred.Links), size = 3, shape = 21, fill = "black") + 
  scale_x_continuous(name = "Degree of anecdotal links")+
  scale_y_continuous(
    # Features of the first axis
    name = "Percent of anecdotal links identified"
  )


##figure 3b
my_data<-read.csv("ExampleOutputFromStep3b.csv",header=T)
summary(my_data)

ggplot(my_data, aes(x=Iterations.Anecdotal)) +
  ggtitle("Variation in Explanatory Power of the Inferred Dataset", subtitle="Measure Used: Concordance Rate") +
  geom_line(mapping= aes(y=Concordance.Rate.between.Anecdotal.and.Inferred.Dataset)) + # Divide by 10 to get the same range than the temperature
  geom_point(mapping = aes(x = Iterations.Anecdotal, y = Concordance.Rate.between.Anecdotal.and.Inferred.Dataset), size = 3, shape = 21, fill = "black") + 
  scale_x_continuous(name = "Degree of anecdotal links")+
  scale_y_continuous(
    # Features of the first axis
    name = "Concordance rate"
  )



