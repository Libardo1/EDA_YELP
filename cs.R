#Real Direct Case Study #
install.packages("ggplot2");
require("ggplot2");
library("ggplot2")
install.packages("gdata");

library("gdata")
install.pac
bk.homes = read.xls(file.choose(),pattern="BOROUGH");
bk.homes$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk.homes$SALE.PRICE));
bk.homes$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk.homes$SALE.PRICE));
names(bk.homes) <- tolower(names(bk.homes));
bk.homes$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk.homes$gross.square.feet));bk.homes$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk.homes$land.square.feet));
bk.homes$sale.date <- as.Date(bk.homes$sale.date);
bk.homes$year.built <- as.numeric(as.character(bk.homes$year.built));
attach(bk.homes);
hist(sale.price.n);
hist(sale.price.n[sale.price.n>0]);
hist(gross.sqft[sale.price.n==0]);
detach(bk.homes);
bk.homes.sale <- bk.homes[bk.homes$sale.price.n!=0,];
plot(bk.homes.sale$gross.sqft,bk.homes.sale$sale.price.n);
plot(log(bk.homes.sale$gross.sqft),log(bk.homes.sale$sale.price.n));
#perform exploratory EDA on the Read DirectCase Study bk.homesset#
plot(bk.homes$neighborhood,bk.homes$year.built);
install.packages("ggplot2");
library(ggplot2);
#plirs the density of the building categories#
ggplot(bk.homes, aes(x=bk.homes$building.class.category, colour=building.class.category)) + geom_density();






































