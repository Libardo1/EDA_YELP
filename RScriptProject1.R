library("rjson")
library("rJava")
library("raster")
library("rgdal")
library("OpenStreetMap")

#load business_data from file
business_data <- fromJSON(file = "C:/Users/Aravindhan/Desktop/DIC Project1/DataSet/yelp_academic_dataset_business.json",unexpected.escape = "error")

# collect the latitude and longitude data
for(i in 1:length(business_data))
  lon[i] <- business_data[[i]]$longitude
  lat[i] <-business_data[[i]]$latitude

#ordinary scatter plot
plot(xlab="Longitude",ylab="Latitude",lon,lat,pch=20,col="blue",main="Scatterplot of local businesses' geo coords");

#Display the map of phoenix city using open street maps
cloudmadeMap <- openmap(c(lat=34.2,lon=-113),c(lat=32.8,lon=-111),type="cloudmade-7",minNumTiles=12);
osmMap <- openmap(c(lat=34.2,lon=-113),c(lat=32.8,lon=-111),type="osm",minNumTiles=12)


#Convert the map into plottable map where we can plot points
plottableCloudmadeMap = openproj(cloudmadeMap)
plottableOsmMap = openproj(osmMap)

#scatterplot of latlong of all businesses on maps
plot(plottableCloudmadeMap);
for(i in 1:length(business_data))
  points(lon[i],lat[i],col="red",cex=0.6,pch=20)

#mMydata Frame has data for businesses and their ratings and price
mydf <- NULL;

#collect all restaurants from the business_data along with their lat long
restaurantCount = 0;
for(i in 1:length(business_data)) {
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants"){
        restaurants[restaurantCount] <- business_data[[i]]$business_id;
        restaurantLat[restaurantCount] <- business_data[[i]]$latitude;
        restaurantLong[restaurantCount] <- business_data[[i]]$longitude;
        restaurantRating[restaurantCount] <- floor(business_data[[i]]$stars);
        if(length(business_data[[i]]$attributes$"Price Range")!=0){
        restaurantPrice[restaurantCount] <- business_data[[i]]$attributes$"Price Range";
        } else {
          restaurantPrice[restaurantCount] <- 0;
        }
        restaurantCount = restaurantCount + 1;
        rbind(mydf,c(restaurantRating[restaurantCount],"Restaurant",restaurantPrice[restaurantCount])) ->mydf
      }
  }
  }
}

#collect all spas from the business_data along with their lat long
spaCount = 0;
for(i in 1:length(business_data)) {
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Beauty & Spas"){
        spas[spaCount] <- business_data[[i]]$business_id;
        spaLat[spaCount] <- business_data[[i]]$latitude;
        spaLong[spaCount] <- business_data[[i]]$longitude;
        spaRating[spaCount] <- floor(business_data[[i]]$stars);
        if(length(business_data[[i]]$attributes$"Price Range")!=0){
          spaPrice[spaCount] <- business_data[[i]]$attributes$"Price Range";
        } else {
          spaPrice[spaCount] <- 0;
        }
        rbind(mydf,c(spaRating[spaCount],"Beauty & Spas",spaPrice[spaCount])) ->mydf
        spaCount = spaCount + 1;
      }
    }
  }
}

#collect all healths from the business_data along with their lat long
healthCount = 0;
for(i in 1:length(business_data)) {
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Health & Medical"){
        healths[healthCount] <- business_data[[i]]$business_id;
        healthLat[healthCount] <- business_data[[i]]$latitude;
        healthLong[healthCount] <- business_data[[i]]$longitude;
        healthRating[healthCount] <- floor(business_data[[i]]$stars);
        if(length(business_data[[i]]$attributes$"Price Range")!=0){
          healthPrice[healthCount] <- business_data[[i]]$attributes$"Price Range";
        } else {
          healthPrice[healthCount] <- 0;
        }
        rbind(mydf,c(healthRating[healthCount],"Health & Medical",healthPrice[healthCount])) ->mydf
        healthCount = healthCount + 1;
      }
    }
  }
}

#collect all fashions from the business_data along with their lat long
fashionCount = 0;
for(i in 1:length(business_data)) {
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Fashion"){
        fashions[fashionCount] <- business_data[[i]]$business_id;
        fashionLat[fashionCount] <- business_data[[i]]$latitude;
        fashionLong[fashionCount] <- business_data[[i]]$longitude;
        fashionRating[fashionCount] <- floor(business_data[[i]]$stars);
        if(length(business_data[[i]]$attributes$"Price Range")!=0){
          fashionPrice[fashionCount] <- business_data[[i]]$attributes$"Price Range";
        } else {
          fashionPrice[fashionCount] <- 0;
        }
        rbind(mydf,c(fashionRating[fashionCount],"Fashion",restaurantPrice[fashionCount])) ->mydf
        fashionCount = fashionCount + 1;
      }
    }
  }
}

#collect all autos from the business_data along with their lat long
autoCount = 0;
for(i in 1:length(business_data)) {
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Automotive"){
        autos[autoCount] <- business_data[[i]]$business_id;
        autoLat[autoCount] <- business_data[[i]]$latitude;
        autoLong[autoCount] <- business_data[[i]]$longitude;
        autoRating[autoCount] <- floor(business_data[[i]]$stars);
        if(length(business_data[[i]]$attributes$"Price Range")!=0){
          autoPrice[autoCount] <- business_data[[i]]$attributes$"Price Range";
        } else {
          autoPrice[autoCount] <- 0;
        }
        rbind(mydf,c(autoRating[autoCount],"Automotive",autoPrice[autoCount])) ->mydf
        autoCount = autoCount + 1;
      }
    }
  }
}


#scatterplot of all restaurants based on rating
plot(plottableOsmMap);
for(i in 1:length(restaurants)) {
  if(restaurantRating[i] == 1)
  points(restaurantLong[i],restaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(restaurantRating[i] == 2)
    points(restaurantLong[i],restaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(restaurantRating[i] == 3)
    points(restaurantLong[i],restaurantLat[i],col="orange",cex=0.6,pch=20)
  else if(restaurantRating[i] == 4)
    points(restaurantLong[i],restaurantLat[i],col="green",cex=0.8,pch=20)
  else if(restaurantRating[i] == 5)
    points(restaurantLong[i],restaurantLat[i],col="red",cex=1,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4","5"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("yellow","blue","orange","green","red"),title="Rating");


#scatterplot of all restaurants based on price range
plot(plottableOsmMap);
for(i in 1:length(restaurants)) {
  if(restaurantPrice[i] == 1)
    points(restaurantLong[i],restaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(restaurantPrice[i] == 2)
    points(restaurantLong[i],restaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(restaurantPrice[i] == 3)
    points(restaurantLong[i],restaurantLat[i],col="green",cex=0.8,pch=20)
  else if(restaurantPrice[i] == 4)
    points(restaurantLong[i],restaurantLat[i],col="red",cex=1.0,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8),col=c("yellow","blue","green","red"),title="Price Range");

#collect all mexican  restaurants from the business_data along with their lat long
mexicanRestaurantCount = 0;
for(i in 1:length(business_data)) {
  mexicanFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="Mexican")
        mexicanFlag = 1;
    }
    if(mexicanFlag == 1 && restaurantFlag == 1){
      mexicanRestaurants[mexicanRestaurantCount] <- business_data[[i]]$business_id;
      mexicanRestaurantLat[mexicanRestaurantCount] <- business_data[[i]]$latitude;
      mexicanRestaurantLong[mexicanRestaurantCount] <- business_data[[i]]$longitude;
      mexicanRestaurantRating[mexicanRestaurantCount] <- floor(business_data[[i]]$stars);
      if(length(business_data[[i]]$attributes$"Price Range")!=0){
        mexicanRestaurantPrice[mexicanRestaurantCount] <- business_data[[i]]$attributes$"Price Range";
      } else {
        mexicanRestaurantPrice[mexicanRestaurantCount] <- 0;
      }
      mexicanRestaurantCount = mexicanRestaurantCount + 1;
    }
  }
}



#scatterplot of all mexican mexicanRestaurants based on rating
plot(plottableOsmMap);
for(i in 1:length(mexicanRestaurants)) {
  if(mexicanRestaurantRating[i] == 1)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(mexicanRestaurantRating[i] == 2)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(mexicanRestaurantRating[i] == 3)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="orange",cex=0.6,pch=20)
  else if(mexicanRestaurantRating[i] == 4)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="green",cex=0.8,pch=20)
  else if(mexicanRestaurantRating[i] == 5)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="red",cex=1,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4","5"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("yellow","blue","orange","green","red"),title="Rating");

#scatterplot of all mexican restaurants based on price range
plot(plottableOsmMap);
for(i in 1:length(mexicanRestaurants)) {
  if(mexicanRestaurantPrice[i] == 1)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(mexicanRestaurantPrice[i] == 2)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(mexicanRestaurantPrice[i] == 3)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="green",cex=0.8,pch=20)
  else if(mexicanRestaurantPrice[i] == 4)
    points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="red",cex=1.0,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4"),pch=c(20,20,20,20),cex=c(0.8,0.8,0.8,0.8),col=c("yellow","blue","green","red"),title="Price Range");

#collect all indian  restaurants from the business_data along with their lat long
indianRestaurantCount = 0;
for(i in 1:length(business_data)) {
  indianFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="Indian")
        indianFlag = 1;
    }
    if(indianFlag == 1 && restaurantFlag == 1){
      indianRestaurants[indianRestaurantCount] <- business_data[[i]]$business_id;
      indianRestaurantLat[indianRestaurantCount] <- business_data[[i]]$latitude;
      indianRestaurantLong[indianRestaurantCount] <- business_data[[i]]$longitude;
      indianRestaurantRating[indianRestaurantCount] <- floor(business_data[[i]]$stars);
      if(length(business_data[[i]]$attributes$"Price Range")!=0){
        indianRestaurantPrice[indianRestaurantCount] <- business_data[[i]]$attributes$"Price Range";
      } else {
        indianRestaurantPrice[indianRestaurantCount] <- 0;
      }
      indianRestaurantCount = indianRestaurantCount + 1;
    }
  }
}



#scatterplot of all indian indianRestaurants based on rating
plot(plottableCloudmadeMap);
for(i in 1:length(indianRestaurants)) {
  if(indianRestaurantRating[i] == 1)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(indianRestaurantRating[i] == 2)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(indianRestaurantRating[i] == 3)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="orange",cex=0.6,pch=20)
  else if(indianRestaurantRating[i] == 4)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(indianRestaurantRating[i] == 5)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="red",cex=1,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4","5"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("yellow","blue","brown","green","red"),title="Rating");


#scatterplot of all indian restaurants based on price range
plot(plottableCloudmadeMap);
for(i in 1:length(indianRestaurants)) {
  if(indianRestaurantPrice[i] == 1)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(indianRestaurantPrice[i] == 2)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(indianRestaurantPrice[i] == 3)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(indianRestaurantPrice[i] == 4)
    points(indianRestaurantLong[i],indianRestaurantLat[i],col="red",cex=1.0,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4"),pch=c(20,20,20,20),cex=c(0.8,0.8,0.8,0.8),col=c("yellow","blue","brown","red"),title="Price Range");

#collect all chinese  restaurants from the business_data along with their lat long
chineseRestaurantCount = 0;
for(i in 1:length(business_data)) {
  chineseFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="Chinese")
        chineseFlag = 1;
    }
    if(chineseFlag == 1 && restaurantFlag == 1){
      chineseRestaurants[chineseRestaurantCount] <- business_data[[i]]$business_id;
      chineseRestaurantLat[chineseRestaurantCount] <- business_data[[i]]$latitude;
      chineseRestaurantLong[chineseRestaurantCount] <- business_data[[i]]$longitude;
      chineseRestaurantRating[chineseRestaurantCount] <- floor(business_data[[i]]$stars);
      if(length(business_data[[i]]$attributes$"Price Range")!=0){
        chineseRestaurantPrice[chineseRestaurantCount] <- business_data[[i]]$attributes$"Price Range";
      } else {
        chineseRestaurantPrice[chineseRestaurantCount] <- 0;
      }
      chineseRestaurantCount = chineseRestaurantCount + 1;
    }
  }
}



#scatterplot of all chinese chineseRestaurants based on rating
plot(plottableCloudmadeMap);
for(i in 1:length(chineseRestaurants)) {
  if(chineseRestaurantRating[i] == 1)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(chineseRestaurantRating[i] == 2)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(chineseRestaurantRating[i] == 3)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="orange",cex=0.6,pch=20)
  else if(chineseRestaurantRating[i] == 4)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(chineseRestaurantRating[i] == 5)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="red",cex=1,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4","5"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("yellow","blue","orange","brown","red"),title="Rating");

#scatterplot of all chinese restaurants based on price range
plot(plottableCloudmadeMap);
for(i in 1:length(chineseRestaurants)) {
  if(chineseRestaurantPrice[i] == 1)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(chineseRestaurantPrice[i] == 2)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(chineseRestaurantPrice[i] == 3)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(chineseRestaurantPrice[i] == 4)
    points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="red",cex=1.0,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4"),pch=c(20,20,20,20),cex=c(0.8,0.8,0.8,0.8),col=c("yellow","blue","brown","red"),title="Price Range");


#collect all italian  restaurants from the business_data along with their lat long
italianRestaurantCount = 0;
for(i in 1:length(business_data)) {
  italianFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="Italian")
        italianFlag = 1;
    }
    if(italianFlag == 1 && restaurantFlag == 1){
      italianRestaurants[italianRestaurantCount] <- business_data[[i]]$business_id;
      italianRestaurantLat[italianRestaurantCount] <- business_data[[i]]$latitude;
      italianRestaurantLong[italianRestaurantCount] <- business_data[[i]]$longitude;
      italianRestaurantRating[italianRestaurantCount] <- floor(business_data[[i]]$stars);
      if(length(business_data[[i]]$attributes$"Price Range")!=0){
        italianRestaurantPrice[italianRestaurantCount] <- business_data[[i]]$attributes$"Price Range";
      } else {
        italianRestaurantPrice[italianRestaurantCount] <- 0;
      }
      italianRestaurantCount = italianRestaurantCount + 1;
    }
  }
}



#scatterplot of all italian italianRestaurants based on rating
plot(plottableCloudmadeMap);
for(i in 1:length(italianRestaurants)) {
  if(italianRestaurantRating[i] == 1)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(italianRestaurantRating[i] == 2)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(italianRestaurantRating[i] == 3)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="orange",cex=0.6,pch=20)
  else if(italianRestaurantRating[i] == 4)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(italianRestaurantRating[i] == 5)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="red",cex=1,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4","5"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("yellow","blue","orange","brown","red"),title="Rating");


#scatterplot of all italian restaurants based on price range
plot(plottableCloudmadeMap);
for(i in 1:length(italianRestaurants)) {
  if(italianRestaurantPrice[i] == 1)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(italianRestaurantPrice[i] == 2)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(italianRestaurantPrice[i] == 3)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(italianRestaurantPrice[i] == 4)
    points(italianRestaurantLong[i],italianRestaurantLat[i],col="red",cex=1.0,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4"),pch=c(20,20,20,20),cex=c(0.8,0.8,0.8,0.8),col=c("yellow","blue","brown","red"),title="Price Range");



#collect all french  restaurants from the business_data along with their lat long
frenchRestaurantCount = 0;
for(i in 1:length(business_data)) {
  frenchFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="French")
        frenchFlag = 1;
    }
    if(frenchFlag == 1 && restaurantFlag == 1){
      frenchRestaurants[frenchRestaurantCount] <- business_data[[i]]$business_id;
      frenchRestaurantLat[frenchRestaurantCount] <- business_data[[i]]$latitude;
      frenchRestaurantLong[frenchRestaurantCount] <- business_data[[i]]$longitude;
      frenchRestaurantRating[frenchRestaurantCount] <- floor(business_data[[i]]$stars);
      if(length(business_data[[i]]$attributes$"Price Range")!=0){
        frenchRestaurantPrice[frenchRestaurantCount] <- business_data[[i]]$attributes$"Price Range";
      } else {
        frenchRestaurantPrice[frenchRestaurantCount] <- 0;
      }
      frenchRestaurantCount = frenchRestaurantCount + 1;
    }
  }
}



#scatterplot of all french frenchRestaurants based on rating
plot(plottableCloudmadeMap);
for(i in 1:length(frenchRestaurants)) {
  if(frenchRestaurantRating[i] == 1)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(frenchRestaurantRating[i] == 2)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(frenchRestaurantRating[i] == 3)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="orange",cex=0.6,pch=20)
  else if(frenchRestaurantRating[i] == 4)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(frenchRestaurantRating[i] == 5)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="red",cex=1,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4","5"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("yellow","blue","orange","brown","red"),title="Rating");


#scatterplot of all french restaurants based on price range
plot(plottableCloudmadeMap);
for(i in 1:length(frenchRestaurants)) {
  if(frenchRestaurantPrice[i] == 1)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="yellow",cex=0.6,pch=20)
  else if(frenchRestaurantPrice[i] == 2)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="blue",cex=0.6,pch=20)
  else if(frenchRestaurantPrice[i] == 3)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="brown",cex=0.8,pch=20)
  else if(frenchRestaurantPrice[i] == 4)
    points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="red",cex=1.0,pch=20)
}
legend(-111.2,34.1,c("1","2","3","4"),pch=c(20,20,20,20),cex=c(0.8,0.8,0.8,0.8),col=c("yellow","blue","brown","red"),title="Price Range");



#collect all korean  restaurants from the business_data along with their lat long
koreanRestaurantCount = 0;
for(i in 1:length(business_data)) {
  koreanFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="korean")
        koreanFlag = 1;
    }
    if(koreanFlag == 1 && restaurantFlag == 1){
      koreanRestaurantCount = koreanRestaurantCount + 1;
    }
  }
}


japaneseRestaurantCount = 0;
for(i in 1:length(business_data)) {
  japaneseFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="Japanese")
        japaneseFlag = 1;
    }
    if(japaneseFlag == 1 && restaurantFlag == 1){
      japaneseRestaurantCount = japaneseRestaurantCount + 1;
    }
  }
}

#collect all americannew  restaurants from the business_data along with their lat long
americannewRestaurantCount = 0;
for(i in 1:length(business_data)) {
  americannewFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="American (New)")
        americannewFlag = 1;
    }
    if(americannewFlag == 1 && restaurantFlag == 1){
      americannewRestaurantCount = americannewRestaurantCount + 1;
    }
  }
}

#collect all americantrad  restaurants from the business_data along with their lat long
americantradRestaurantCount = 0;
for(i in 1:length(business_data)) {
  americantradFlag = 0;
  restaurantFlag = 0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants")
        restaurantFlag = 1;
      if(categories[j]=="American (Traditional)")
        americantradFlag = 1;
    }
    if(americantradFlag == 1 && restaurantFlag == 1){
      americantradRestaurantCount = americantradRestaurantCount + 1;
    }
  }
}



#scatterplot of restaurants based on cuisines
plot(plottableOsmMap);
for(i in 1:length(italianRestaurants)) {
  points(italianRestaurantLong[i],italianRestaurantLat[i],col="green",cex=0.6,pch=20)
}
for(i in 1:length(chineseRestaurants)) {
  points(chineseRestaurantLong[i],chineseRestaurantLat[i],col="blue",cex=0.6,pch=20)
}
for(i in 1:length(indianRestaurants)) {
  points(indianRestaurantLong[i],indianRestaurantLat[i],col="yellow",cex=0.6,pch=20)
}
for(i in 1:length(mexicanRestaurants)) {
  points(mexicanRestaurantLong[i],mexicanRestaurantLat[i],col="red",cex=0.6,pch=20)
}
for(i in 1:length(frenchRestaurants)) {
  points(frenchRestaurantLong[i],frenchRestaurantLat[i],col="orange",cex=0.6,pch=20)
}
legend(-111.2,34.1,c("Italian","Chinese","Indian","Meixcan","French"),pch=c(20,20,20,20,20),cex=c(0.8,0.8,0.8,0.8,0.8),col=c("green","blue","yellow","red","orange"),title="Cuisine");

#plot of no of restaurants per cuisine
cuisines <- c("Mexican", "Italian","Indian","Chinese","French","Japanese","American (New)","American (Traditional)");
cuisineCount <- c(mexicanRestaurantCount,italianRestaurantCount,indianRestaurantCount,chineseRestaurantCount,frenchRestaurantCount,japaneseRestaurantCount,americannewRestaurantCount,americantradRestaurantCount);
qplot(x = cuisines, y = cuisineCount, geom="histogram") + xlab("Cuisines") + ylab("Count");

#pie chart for no of restaurants as per cuisine
library("plotrix");
pie3D(cuisineCount, labels = cuisines, main="Pie Chart of Restaurants",col=rainbow(length(cuisines)));

#collect data frame for restaurant rating and cuisine type 
ratingTypeDF <- NULL;
for(i in 1:length(business_data)) {
  restaurantFlag=0;
  typeFlag=0;
  categories <- business_data[[i]]$categories;
  if(length(categories) != 0){ 
    for(j in 1:length(categories)) {
      if(categories[j]=="Restaurants"){
        rating = business_data[[i]]$stars;
        if(length(business_data[[i]]$attributes$"Price Range")!=0){
          price <- business_data[[i]]$attributes$"Price Range";
        } else {
          price <- 0;
        }
        restaurantFlag = 1;
      } 
      if(categories[j]=="Mexican"){
        type = "Mexican";
        typeFlag=1;
      }
      if(categories[j]=="Italian"){
        type = "Italian";
        typeFlag=1;
      }
      if(categories[j]=="Chinese"){
        type = "Chinese";
        typeFlag=1;
      }
      if(categories[j]=="Indian"){
        type = "Indian";
        typeFlag=1;
      }
      if(categories[j]=="French"){
        type = "French";
        typeFlag=1;
      }
      if(categories[j]=="Japanese"){
        type = "Japanese";
        typeFlag=1;
      }
      if(categories[j]=="Korean"){
        type = "Korean";
        typeFlag=1;
      }
      if(categories[j]=="American (New)"){
        type = "American (New)";
        typeFlag=1;
      }
      if(categories[j]=="American (Traditional)"){
        type = "American (Traditional)";
        typeFlag=1;
    }
    }
    if(restaurantFlag == 1 && typeFlag==1) {
      rbind(ratingTypeDF,c(rating,type,price))->ratingTypeDF;
    }
  }
}

#distribution of restaurant ratings with respect to each cuisine type
install.packages("reshape");
library("plyr");
library("reshape");
properratingTypeDF = as.data.frame(ratingTypeDF);
properratingTypeDF <- rename(properratingTypeDF, c(V1="Rating", V2="Cuisine",V3="Price"));
ggplot(properratingTypeDF,aes(x=Rating)) + geom_density(aes(col=Cuisine),size=0.7);
ggplot(properratingTypeDF,aes(x=Rating)) + geom_histogram(aes(fill=Cuisine));
ggplot(properratingTypeDF,aes(x=Rating)) + geom_bar(aes(fill=Cuisine));


#distribution of restaurant price range with respect to each cuisine type
ggplot(subset(properratingTypeDF,Price !=0),aes(x=Price)) + geom_density(aes(col=Cuisine),size=0.7);
ggplot(subset(properratingTypeDF,Price !=0),aes(x=Price)) + geom_histogram(aes(fill=Cuisine));
ggplot(subset(properratingTypeDF,Price !=0),aes(x=Price)) + geom_bar(aes(col=Cuisine));


#collect user data from the json file
user_data <- fromJSON(file = "C:/Users/Aravindhan/Desktop/DIC Project1/DataSet/yelp_academic_dataset_user.json",unexpected.escape = "error")

#prepare data frame from user_data
userReviewDF <-NULL
userCount = 0;
kmeansDF <-NULL
for(i in 1:length(user_data)){
  user = userCount;
  year = substr(user_data[[i]]$"yelping_since",1,4);
  review_count = user_data[[i]]$"review_count";
  rbind(userReviewDF,c(user,year,review_count)) -> userReviewDF;
  rbind(kmeansDF,c(user,review_count)) ->kmeansDF;
  userCount = userCount + 1;
}

#plot user review vs yelping since ggplots
properuserReviewDF = as.data.frame(userReviewDF);
properuserReviewDF <- rename(properuserReviewDF, c(V1="User", V2="Yelping_Since",V3="No_of_Reviews"));
ggplot(properuserReviewDF,aes(x=as.integer(No_of_Reviews))) + geom_density(aes(col=Yelping_Since),size=0.7) + xlab("No of Reviews");
ggplot(properuserReviewDF,aes(x=as.integer(No_of_Reviews))) + geom_histogram(aes(fill=Yelping_Since)) + xlab("No of Reviews");


#k means clustering 
properkmeansDF <- as.data.frame(kmeansDF);
properkmeansDF <- rename(properkmeansDF,c(V1="User",V2="No_of_Reviews"));
ds <- kmeans(properkmeansDF,10);
plot(properkmeansDF, col= ds$cluster);
plot(properkmeansDF, col= ds$center);
plot(ds$center);
plot(ds$cluster);

#barplot of various businesses and their count
businesses <- c("Restaurants", "Beauty & Spas","Health & Medical","Automotive","Fashion");
bCount <- c(restaurantCount,spaCount,healthCount,autoCount,fashionCount);
qplot(x = businesses, y = bCount, geom="histogram") + xlab("Businesses") + ylab("Count");


#distribution of business vs rating
propermydf = as.data.frame(mydf);
propermydf <- rename(propermydf, c(V2="Business_Category", V1="Rating",V3="Price_Range"));
ggplot(propermydf,aes(x=as.integer(Rating))) + geom_density(aes(col=Business_Category),size=0.7) + xlab("Rating") + coord_cartesian(xlim = c(1, 5));
ggplot(propermydf,aes(x=as.integer(Rating))) + geom_histogram(aes(fill=Business_Category)) + xlab("Rating") + coord_cartesian(xlim = c(0, 6));


ggplot(subset(propermydf),aes(x=as.integer(Price_Range))) + geom_density(aes(col=Business_Category),size=0.7) +xlab("Price Range") +  coord_cartesian(xlim = c(1, 5));
ggplot(subset(propermydf),aes(x=as.integer(Price_Range))) + geom_histogram(aes(fill=Business_Category)) + xlab("Price_Range") + coord_cartesian(xlim = c(0, 6));
