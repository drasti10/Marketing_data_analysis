#importing and loading the libraries
library(rstatix)
library(tidyverse)
library(ggplot2)

#reading the csv file
mds <- read.csv(file = 'marketing_data.csv')
#converting character datatype of income to numeric datatype
Income_new <- gsub("[$,]", "", mds$Income)
mds$Income <- as.numeric(Income_new)
#creating a dataframe
mdf <- data.frame(mds)
#finding total na
sum(is.na(mdf))
#removing the na values and storing it in the dataframe
mdf <- na.omit(mdf)
#identifying the outliers in various columns of the data frame and plotting the variable in case of an extreme outlier.
identify_outliers(mdf, NumWebVisitsMonth)
identify_outliers(mdf, NumStorePurchases)
identify_outliers(mdf, NumCatalogPurchases)
identify_outliers(mdf, NumWebPurchases)
identify_outliers(mdf, NumDealsPurchases)
identify_outliers(mdf, MntGoldProds)
plot(mdf$MntGoldProds, main = "Plotting the values of Gold Products to identify extreme outliers")
identify_outliers(mdf, MntSweetProducts)
plot(mdf$MntSweetProducts, main = "Plotting the values of Sweet Products to identify extreme outliers")
identify_outliers(mdf, MntFishProducts)
plot(mdf$MntFishProducts)
identify_outliers(mdf, MntMeatProducts)
plot(mdf$MntMeatProducts, main = "Plotting the values of Meat Products to identify extreme outliers")
identify_outliers(mdf, MntFruits)
plot(mdf$MntFruits)
identify_outliers(mdf, MntWines)

#summing up similar columns to move ahead with the analysis
mdf<-mdf%>% 
  mutate(TotalMntSpent=MntWines + MntFruits + MntMeatProducts +MntFishProducts + MntSweetProducts + MntGoldProds)%>%
  mutate(TotalAcceptedCmp = AcceptedCmp1+ AcceptedCmp2 +AcceptedCmp3+ AcceptedCmp4+ AcceptedCmp5)%>%
  mutate(TotalNumPurchases=NumDealsPurchases + NumWebPurchases + NumCatalogPurchases+ NumStorePurchases)

#Education, Marital Status and Country are the three categorical variables on the basis of which data will be analyzed.
#data aggregation by Education
ByEducation<-aggregate(cbind(Income,TotalMntSpent,TotalNumPurchases,TotalAcceptedCmp) ~ Education, data = mdf, sum)
ByEducation <- ByEducation[order(ByEducation$Income,decreasing=T),]
ByEducation

#data aggregation by Marital Status
ByMaritalStatus <- aggregate(cbind(Income,TotalMntSpent,TotalNumPurchases,TotalAcceptedCmp) ~ Marital_Status, data = mdf, sum)
ByMaritalStatus <- ByMaritalStatus[order(ByMaritalStatus$Income,decreasing=T),]
ByMaritalStatus

#data aggregation by country
ByCountry <- aggregate(cbind(Income,TotalMntSpent,TotalNumPurchases,TotalAcceptedCmp) ~ Country, data = mdf, sum)
ByCountry <- ByCountry[order(ByCountry$Income,decreasing=T),]
ByCountry

#are people who buy gold more conservative?
if (mdf$MntGoldProds > mean(mdf$MntGoldProds)){
  print(mdf$NumStorePurchases)
}
#plotting the graphs

#1
#Success of marketing campaign by Marital Status
Campaign <- gather(mdf, key="measure", value="value", c("AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1","AcceptedCmp2"))
ggplot(Campaign, aes(x=factor(Marital_Status), y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure) + ggtitle("Plot Of campaign success By Marital Status")  + 
  theme(axis.text.x = element_text(angle = 90)
        ,axis.title.x = element_blank(),
        axis.title.y = element_blank(),panel.spacing = unit(2, "lines"))

#Success of marketing campaign by Education
Campaign <- gather(mdf, key="measure", value="value", c("AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1","AcceptedCmp2"))
ggplot(Campaign, aes(x=factor(Education), y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure) + ggtitle("Plot Of campaign success By Education")  + 
  theme(axis.text.x = element_text(angle = 90)
        ,axis.title.x = element_blank(),
        axis.title.y = element_blank(),panel.spacing = unit(2, "lines"))

#Success of marketing campaign by Country
Campaign <- gather(mdf, key="measure", value="value", c("AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1","AcceptedCmp2"))
ggplot(Campaign, aes(x=factor(Country), y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure) + ggtitle("Plot Of campaign success By Country")  + 
  theme(axis.text.x = element_text(angle = 90)
        ,axis.title.x = element_blank(),
        axis.title.y = element_blank(),panel.spacing = unit(2, "lines"))

#2
#Product performance by Marital Status
Product_perf <- gather(mdf, key="measure", value="value", c("MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds"))
ggplot(Product_perf, aes(x=factor(Marital_Status), y=value))+
  geom_bar(stat='identity', fill="Magenta")+
  facet_wrap(~measure) + theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
                               axis.title.y = element_blank(),panel.spacing = unit(2, "lines")) + ggtitle("Plot of Different Products by Marital Status")

#Product performance by Education
Product_perf <- gather(mdf, key="measure", value="value", c("MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds"))
ggplot(Product_perf, aes(x=factor(Education), y=value))+
  geom_bar(stat='identity', fill="Magenta")+
  facet_wrap(~measure) + theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
                               axis.title.y = element_blank(),panel.spacing = unit(2, "lines")) + ggtitle("Plot of Different Products by Education")

#Product performance by country
Product_perf <- gather(mdf, key="measure", value="value", c("MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds"))
ggplot(Product_perf, aes(x=factor(Country), y=value))+
  geom_bar(stat='identity', fill="Magenta")+
  facet_wrap(~measure) + theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank(),
                               axis.title.y = element_blank(),panel.spacing = unit(2, "lines")) + ggtitle("Plot of Different Products by Country")

#3
#Channel performance by Marital Status
Ch_perf <- gather(mdf, key="measure", value="value", c("NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth"))
ggplot(Ch_perf, aes(x=factor(Marital_Status), y=value))+
  geom_bar(stat='identity', fill="Black")+
  facet_wrap(~measure) + ggtitle("Plot Of Channel performance By Marital Status")  + 
  theme(axis.text.x = element_text(angle = 90)
        ,axis.title.x = element_blank(),
        axis.title.y = element_blank(),panel.spacing = unit(2, "lines"))

#Channel performance by Education
Ch_perf <- gather(mdf, key="measure", value="value", c("NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth"))
ggplot(Ch_perf, aes(x=factor(Education), y=value))+
  geom_bar(stat='identity', fill="Black")+
  facet_wrap(~measure) + ggtitle("Plot Of Channel performance By Education")  + 
  theme(axis.text.x = element_text(angle = 90)
        ,axis.title.x = element_blank(),
        axis.title.y = element_blank(),panel.spacing = unit(2, "lines"))

#Channel performance by Country
Ch_perf <- gather(mdf, key="measure", value="value", c("NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth"))
ggplot(Ch_perf, aes(x=factor(Country), y=value))+
  geom_bar(stat='identity', fill="Black")+
  facet_wrap(~measure) + ggtitle("Plot Of Channel performance By Country")  + 
  theme(axis.text.x = element_text(angle = 90)
        ,axis.title.x = element_blank(),
        axis.title.y = element_blank(),panel.spacing = unit(2, "lines"))


