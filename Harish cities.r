
# Project Title:  Room rent analysis for hotels
# NAME: Harish Ravi
# EMAIL: hharish001@gmail.com
# COLLEGE / COMPANY: SSN college of Engineering



# tasks 1 and 2
cities42.df<-read.csv(paste("Cities42.csv",sep = ""))


View(cities42.df)

summary(cities42.df)

# data cleaning
#Removing the repeated dates 

cities42.df$Date<-gsub("18-Dec-16", "Dec 18 2016", cities42.df$Date)
cities42.df$Date<-gsub("21-Dec-16", "Dec 21 2016", cities42.df$Date)
cities42.df$Date<-gsub("24-Dec-16", "Dec 24 2016", cities42.df$Date)
cities42.df$Date<-gsub("25-Dec-16", "Dec 25 2016", cities42.df$Date)
cities42.df$Date<-gsub("28-Dec-16", "Dec 28 2016", cities42.df$Date)
cities42.df$Date<-gsub("31-Dec-16", "Dec 31 2016", cities42.df$Date)
cities42.df$Date<-gsub("4-Jan-17", "Jan 04 2017", cities42.df$Date)
cities42.df$Date<-gsub("4-Jan-16", "Jan 04 2017", cities42.df$Date)
cities42.df$Date<-gsub("8-Jan-16", "Jan 08 2017", cities42.df$Date)
cities42.df$Date<-gsub("8-Jan-17", "Jan 08 2017", cities42.df$Date)
cities42.df$Date<-gsub("Jan 4 2017", "Jan 04 2017", cities42.df$Date)
cities42.df$Date<-gsub("Jan 8 2017", "Jan 08 2017", cities42.df$Date)

#checking dates
table(cities42.df$Date)


#dates to factors for labelling 
cities42.df$Date<-factor(cities42.df$Date)
is.factor(cities42.df$Date)

#Checking the labelling
levels(cities42.df$Date)

#Analyzing the summary of the data and describing the variables
################################
#Task 3:y(room rent) =F(star rating , capacity , swimming pool , date , hotel name , city)
#Task 4: Dependent variable = roomrent;
##############################
library(psych)
describe(cities42.df)

summary(cities42.df)

#Taking Y = RoomRent, identifying the most relevent predictor variables by corrgram


#Corrgram

library(corrgram)

corrgram(cities42.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel  data")

#########################
#TASK 5 : Three most important predictors
  ##from the corrgram it is understood that HasSwimming, StarRating, HotelCapital are very well correlated to RoomRent
  ##therefore we take it as predictors
#############################

## TASK 6:Visualizing data for Y as Room rent and X1,X2,X3 as HasSwimmingPool, StarRating and HotelCapacity respectively

  #Table for HasSwimmingPool
    table(cities42.df$HasSwimmingPool)
   
    
  #Table for StarRating
    table(cities42.df$StarRating)
    
  
  #BoxPlot for HotelCapacity
    boxplot(cities42.df$HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE)

#TASK 7 ::Scatterplot for predictor variable
    
    library(car)
    #StarRating Vs RoomRent
    
    scatterplot(cities42.df$StarRating,cities42.df$RoomRent,main="RoomRent of Hotels  with StarRating",ylab = "RoomRent in INR", xlab="Star rating out of 5",cex=1.1)
    
    #RoomRent Vs HotelCapacity
    
    scatterplot(cities42.df$RoomRent,cities42.df$HotelCapacity,main="RoomRent of Hotels  with Hotel capacity",ylab = "Hotel Capacity in rooms", xlab="RoomRent in INR",cex=1.1)
    
    #RoomRent Vs HasSwimmingPool
    
    plot(jitter(cities42.df$RoomRent),jitter(cities42.df$HasSwimmingPool),main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent",cex=1.1)
    library(lattice)
    bwplot(HasSwimmingPool~RoomRent, data = cities42.df,main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent" )
    
    #Scatterplot matrix
    
    scatterplotMatrix(
      cities42.df[
        ,c("RoomRent","HasSwimmingPool","StarRating", "HotelCapacity")], 
      spread=FALSE, smoother.args=list(lty=2),
      main="Scatter Plot Matrix", diagonal = "histogram")
        
    
    #TASK 8::Corrgram of Y, x1, x2, x3
    
    library(corrgram)
    
    xyz<-data.frame(cities42.df$RoomRent, cities42.df$HasSwimmingPool, cities42.df$HotelCapacity, cities42.df$StarRating)
    corrgram(xyz, order=TRUE, lower.panel=panel.shade,
             upper.panel=panel.pie, text.panel=panel.txt,
             main="Corrgram of Hotel Prices In India")
    
    #TASK 9:Variance-Covariance Matrix for Y, x1, x2, x3

    x<-cities42.df[,c("HasSwimmingPool","StarRating", "HotelCapacity")]
    y<-cities42.df[,c("RoomRent")]
  #Variance and covariance matrix
    cov(x,y)
    var(x,y)
	cor(x,y)
	

   #Comparing other factors with roomrent
   
   #Analyzing IsWeekend with RoomRent
   table(cities42.df$IsWeekend)
   
   #Effect of Isweekend on RoomRent
   aggregate(RoomRent ~ IsWeekend, data=cities42.df,mean)
   boxplot(RoomRent~IsWeekend,data=cities42.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)
   
   #Comapring RoomRent with different dates
   table(cities42.df$Date)
   
   library(lattice)
   histogram(~Date, data = cities42.df, main="Distribution of Dates", xlab = "Differnt of Dates", col="Blue")
   
   
   #Effect of different dates on RoomRent
   
   aggregate(RoomRent ~ Date, data = cities42.df,mean)
   scatterplot(d$Date,d$RoomRent, main="Scatterplot between Date and RoomRent", xlab="Date", ylab = "Room Rent in Rupees")
   
   
   boxplot(RoomRent~Date,data=cities42.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))
  
   
   #Analyzing IsMetroCity with RoomRent
   table(cities42.df$IsMetroCity)
  
   #Effect of IsMetroCity on RoomRent
   aggregate(RoomRent ~ IsMetroCity, data = cities42.df, mean) 
   boxplot(RoomRent~IsMetroCity,data=cities42.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

   
   
   #Analyzing IsTouristDestination with RoomRent
   table(cities42.df$IsTouristDestination)
  
   #Effect of IsTouristDestination on RoomRent
   aggregate(RoomRent ~ IsTouristDestination, data = cities42.df, mean)
   boxplot(RoomRent~IsTouristDestination,data=cities42.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
 
   
   #Analyzing FreeWifi Vs RoomRent
   table(cities42.df$FreeWifi)
  
   #Effect of FreeWifi on RoomRent
   aggregate(RoomRent ~ FreeWifi, data = cities42.df, mean)
   boxplot(RoomRent~FreeWifi,data=cities42.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
  
   
   #Analyzing FreeBreakfast Vs RoomRent
   table(cities42.df$FreeWifi)
  
   
   #Effect of FreeBreakfast on RoomRent
   aggregate(RoomRent ~ FreeBreakfast, data =cities42.df, mean)
   boxplot(RoomRent~FreeBreakfast,data=cities42.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
   
   
   
   #Analyzing Airport distance from hotel 
   summary(cities42.df$Airport)
   boxplot(cities42.df$Airport, main="Boxplot of Airport",xlab= "Distance of airport from hotel(Km)" ,col="green",horizontal = TRUE)
   
   #Effect of Airport distance on RoomRent
   
   scatterplot(cities42.df$Airport,cities42.df$RoomRent, main="Room rent vs. Airport distance", xlab="Airport distance(km)", ylab="Room Rent in rupees ",cex=1.1)
   
##Hypothesis
   
   #1.The prices of rooms at hotels with swimming pool are higher than the hotels without swimming pool.
   t.test(RoomRent~HasSwimmingPool,data = cities42.df, alternative="less")
   
   #2.Average RoomRent in hotels with high star rating is high as compared to one which has less star rating.
   t.test(cities42.df$RoomRent,cities42.df$StarRating)
   
   #3.Average RoomRent in hotels providing Free Breakfast is more than that which don't provide.
   t.test(RoomRent~FreeBreakfast, data = cities42.df, alternative="less")
   
   #4.Average RoomRent in metro cities hotels is more than that of non metro cities.
   t.test(RoomRent~IsMetroCity, data = cities42.df, alternative="less")
   
   #5.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.
   t.test(cities42.df$RoomRent,cities42.df$HotelCapacity)
   
   #6.The prices of rooms at hotels located at tourist destination are higher than those located at places without tourist destination.  
    t.test(hotel.df$RoomRent[hotel.df$IsTouristDestination==1],hotel.df$RoomRent[hotel.df$IsTouristDestination==0])
   
   #7.Average RoomRent in hotels providing Free wifi is more than that which don't provide.
   t.test(RoomRent~FreeWifi, data = cities42.df, alternative="less")
 
   #chi sq test
   chisq.test(RoomRent,HotelCapacity)
   chisq.test(RoomRent,StarRating)
   chisq.test(RoomRent,HasSwimmingPool)
   
   
   fit <- lm(RoomRent ~ StarR, data = women)
   summary(fit)

   fitted_variables <- data.frame(cities42.df$RoomRent,cities42.df$StarRating,cities42.df$HotelCapacity,cities42.df$HasSwimmingPool)

