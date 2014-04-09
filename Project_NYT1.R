#Final R Script - Project 1 EDA in R #
#Aravindhan Thanigachalam 50098345 , Srinivasan Rengarajan - 50097996#
#input the data of each day in order to calculate the metrics and store them in a 
#data frame #
nyt1$age_group[nyt1$Age < 18] <- "<18";
nyt1$age_group[nyt1$Age >= 18 & nyt1$Age <= 24] <- "18-24";
nyt1$age_group[nyt1$Age >= 24 & nyt1$Age <= 34] <- "24-34";
nyt1$age_group[nyt1$Age >= 34 & nyt1$Age <= 44] <- "34-44";
nyt1$age_group[nyt1$Age >= 44 & nyt1$Age <= 54] <- "44-54";
nyt1$age_group[nyt1$Age >= 54 & nyt1$Age <= 64] <- "54-64";
nyt1$age_group[nyt1$Age >= 65] <- "65+";
#Question 2#
summary(nyt1);
install.packages("doBy");
library("doBy");
siterange <- function(x){c(length(x), min(x), mean(x), max(x))};
summaryBy(Age~age_group, data =nyt1, FUN=siterange);
# so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group,data =nyt1);
install.packages("ggplot2")
library(ggplot2)
ggplot(nyt1, aes(x=Impressions, fill=age_group))+geom_histogram(binwidth=1);
ggplot(nyt1, aes(x=age_group, y=Impressions, fill=age_group)) +geom_boxplot();
# create click thru rate
# we don't care about clicks if there are no impressions
# if there are clicks with no imps my assumptions about
# this data are wrong
nyt1$hasimps <-cut(nyt1$Impressions,c(-Inf,0,Inf));
summaryBy(Clicks~hasimps, data =nyt1, FUN=siterange)
ggplot(subset(nyt1, Impressions>0), aes(x=Clicks/Impressions,colour=age_group)) + geom_density();
ggplot(subset(nyt1, Clicks>0), aes(x=Clicks/Impressions,colour=age_group)) + geom_density();
ggplot(subset(nyt1, Clicks>0), aes(x=age_group, y=Clicks,fill=age_group)) + geom_boxplot();
# Without subset- Categorize Impressions for the Entire Dataset.
ggplot(nyt1, aes(x=age_group, y=Impressions, fill=age_group)) + geom_boxplot();
ggplot(subset(nyt1, Clicks>0 & Impressions > 0), aes(x=age_group,y=Clicks, fill = age_group)) + geom_boxplot();
#Plot the distributions of number impressions and click through-rate (CTR=# clicks/# impressions) for these six age categories.
ggplot(subset(nyt1, Impressions>0), aes(x=Clicks/Impressions,colour=age_group)) + geom_density();
#Plot Clicks Impressions versus Clicks for different Age Groups #
ggplot(nyt1, aes(x=Clicks, y=Impressions, fill=age_group)) + geom_boxplot();
#Categorizes based on the number of Clicks - Question 3#
nyt1$ClicksGroup[nyt1$Clicks == 0] <- "No clicks";
nyt1$ClicksGroup[nyt1$Clicks == 1] <- "One click";
nyt1$ClicksGroup[nyt1$Clicks == 2] <- "Two clicks";
nyt1$ClicksGroup[nyt1$Clicks == 3] <- "Three clicks";
nyt1$ClicksGroup[nyt1$Clicks == 4] <- "Four clicks";
nyt1$Category[nyt1$Gender == 1 & nyt1$Age < 18] <- "<18 Males";
nyt1$Category[nyt1$Gender == 0 & nyt1$Age < 18] <- "<18 Females";

#To estimate the density function #
ggplot(subset(nyt1,Clicks > 1), aes(x=Clicks/Impressions, colour=age_group)) + geom_density();
#Plotting impression counts (histogram)
ggplot(nyt1, aes(x=Impressions, fill=age_group)) + geom_histogram(binwidth=1);
# create categories #
nyt1$scode[nyt1$Impressions==0] <- "NoImps"
nyt$scode[nyt1$Impressions >0] <- "Imps"
nyt1$scode[nyt1$Clicks >0] <- "Clicks"



























