library(data.table)
library(pacman)
library(geosphere)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(corrplot)
library(leaflet)
library(stringr)
library(rgdal)
library(animation)

train = read_csv("C:\\Users\\pulkit\\Desktop\\new york city taxi\\train.csv")
test = read_csv("C:\\Users\\pulkit\\Desktop\\new york city taxi\\test.csv")

//Missing Values
//Missing data is the nemesis of every data scientist, especially if they are new in the field. It is important to check whether 
//we have data missing in the train and the test set.

sum(is.na(train))
sum(is.na(test))

//Pickup and Dropoff date are two of the few variables that are provided. This information could play an important role in predicting the 
//total duration time that is why it needs some attention. I will extract different time features such as hour, week, month and so on.

train$pickup_hour <- hour(train$pickup_datetime)
train$pickup_week <- week(train$pickup_datetime)
train$pickup_month <- month(train$pickup_datetime)
train$pickup_weekdays <- weekdays(train$pickup_datetime)
train$pickup_weekend <- ifelse(train$pickup_weekdays==1 | train$pickup_weekdays==7,"Weekend","not-Weekend")

train = as.data.table(train)

train[,pickup_datetime:=as.Date(pickup_datetime)]
train[,dropoff_datetime:=as.Date(dropoff_datetime)]

train[,":="(
   pickup_yday=yday(pickup_datetime)
  ,pickup_mday=mday(pickup_datetime)
  )]
  
//analysis on Trip Duration.

train %>% 
  ggplot(aes(x=trip_duration)) + 
  geom_histogram(bins=40000, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=12),axis.text = element_text(size=12))+
  ylab("Density")+coord_cartesian(x=c(0,6000))
  
//Using Kmeans to show the dropoff and pickup. To know the most common pickup na drop.

data2=train[1:250,c(6,8)]
plot(data2,main="dropoff_longitude Vs pickup longitude",pch=20,cex=2)
ki=kmeans.ani(data2,2)
ki=kmeans(data2,3)
#kmeans
ki
plot(data2,col=ki$cluster,main="% dropoff_longitude Vs pickup longitude ",pch=20,cex=2)

//Lets see the distribution of the passenger_count and vendor_id. We can observe that the majority of the rides are singel ride. 
//But still a substantial number is more than one ride. Probably rides with more than one ride are more often people who are visiting NY.

plot1 = train %>% 
  group_by(passenger_count) %>% 
  count() %>% 
  ggplot(aes(x=passenger_count,y=n, fill=passenger_count))+
  geom_col()+
  theme(legend.position = "none")

plot2 = train %>% 
  group_by(vendor_id) %>% 
  count() %>% 
  ggplot(aes(x=vendor_id,y=n, fill=vendor_id))+
  geom_col()+
  theme(legend.position = "none")

grid.arrange(plot1, plot2, ncol =2)

//using base plotting system to show mean, median

boxplot(train$passenger_count,col ="pink", ylab = "passenger_count", main= "passenger_count,mean(magenta),median(red)")
abline(h = mean(train$passenger_count),col= "magenta")
abline(h = median(train$passenger_count, col = "red", lwd = 2))

//we see there is dip in the frequency around ending january and starting of february

ggplot(data = train, mapping =aes(x = pickup_datetime ) )+
   geom_histogram(fill = "blue", bins = 120)
   
//Speed of the Ride
//The feature speed is not available for the test data set. But it can help us to see what kind of pattern is there in the traffic.

train[,speed:=(distance_km)/(trip_duration/3600)]
train %>% 
  ggplot(aes(x=speed)) + 
  geom_histogram(bins=4000, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=11),axis.text = element_text(size=8))+
  ylab("Density")+coord_cartesian(x=c(0,50))

//Distance in kilometers between pickup and dropoff location
//From the coordinates of the pickup and dropoff points we can calculate the distance between the two points. To compute these 
//distances we are using the distHaversine function of the geosphere package. This method gives us the shortest distance between two 
//points on a spherical earth.

train <- train[,distance_km := 
                     distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),
                     matrix(c(dropoff_longitude,dropoff_latitude), ncol = 2))/1000
              ]
train %>% 
  ggplot(aes(x=distance_km)) + 
  geom_histogram(bins=4000, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=11),axis.text = element_text(size=8))+
  ylab("Density")+coord_cartesian(x=c(0,25))
