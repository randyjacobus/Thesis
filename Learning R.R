
# Load the important libraries
library(plyr)
library(ggplot2)

# Reading in Data
setwd("/Users/rolandjacobus/Desktop/Coursera/Coursera - Reproducible Data/PA2")
stormdata<-read.csv("StormData.csv")
storm<-data.frame(as.character(stormdata$EVTYPE),stormdata$FATALITIES,stormdata$INJURIES,stormdata$PROPDMG,as.character(stormdata$PROPDMGEXP),
stormdata$CROPDMG,as.character(stormdata$CROPDMGEXP))
names(storm)<-c("EVTYPE","FATAL","INJ","PROPDMG","PEXP","CROPDMG","CEXP")
head(storm)
sapply(storm,class)

i <- sapply(storm, is.factor)
storm[i] <- lapply(storm[i], as.character)
sapply(storm,class)

j <- sapply(storm, is.null)
storm[j] <- lapply(storm[j], 0)

m <- sapply(storm, storm[=="M",]
storm[m] <- lapply(storm[m], 1000000)
storm$PEXP

# How to change factor data to charactar data.
# When reading in data, how do you insure the data are not factors?

# How to write a function
myfunction <- function(arg1, arg2, ... ){
  statements
  return(object)
}
damage<-function(x){
  if(x="k") {
  cost = storm$PROPDMEXP * 1000
  } else {
  cost = storm$PROPDMEXP * 10000
  }
  return(cost) 
}

df$new_column <- df$column1 * df$column2
replace(storm,storm$CROPDMGEXP==k,1000)

replace(storm,is.null(storm),0)

j <- sapply(storm, is.null)
storm[j] <- lapply(storm[j], 0)

storm$CEXP <- replace(storm$CEXP, storm$CEXP== "K", as.numeric(1000))
storm$PEXP <- replace(storm$PEXP, storm$PEXP== "K", as.numeric(1000))
storm$CEXP <- replace(storm$CEXP, storm$CEXP== "M", as.numeric(1000000))
storm$PEXP <- replace(storm$PEXP, storm$PEXP== "M", as.numeric(1000000))
storm$CEXP <- replace(storm$CEXP, storm$CEXP== "B", as.numeric(1000000000))
storm$PEXP <- replace(storm$PEXP, storm$PEXP== "B", as.numeric(1000000000))
storm$CEXP <- ifelse(storm$CEXP == "", 0, storm$CEXP)
storm$PEXP <- ifelse(storm$PEXP == "", 0, storm$PEXP)
storm$PDamage<-as.numeric(storm$PROPDMG) * as.numeric(storm$PEXP)
storm$CDamage<-as.numeric(storm$CROPDMG) * as.numeric(storm$CEXP)
storm$Economic<-storm$PDamage + storm$CDamage
storm$Health<-storm$INJ + storm$FATAL
stormdamage<-data.frame(storm$EVTYPE,storm$Health,storm$Economic)
names(stormdamage)<-c("Type","Health","Economic")
healthdamage<- ddply(stormdamage,c("Type"),summarize, Healthdamage = sum(Health))
newhealthdamage<-healthdamage[order(Healthdamage),]
head(newhealthdamage,10)
# sort by mpg
newdata <- mtcars[order(mpg),] 

x<- healthdamage[with(healthdamage, order(-Healthdamage)), ]
plot<-head(x,10)
hist(is.numeric(plot))

sort(healthdamage,Healthdamage)
head(healthdamage)
tope10health<-ddply(healthdamage,c("Type"),subset, Healthdamage==sort(Healthdamage,TRUE)[10])

hist(as.numeric(healthdamage), main = "Healthdamage",xlab="Number of Fatalities or Injuries", col="red")

df.max1 <- ddply(df, c("z"), subset, x==sort(x, TRUE)[1])

health<-ggplot(data=plot,mapping=aes(x=Type, y=Healthdamage)) + geom_bar(stat="identity") + 
  labs(title = "Top 10 Events That Cause the Most in Fatalities and Injuries") +
  theme(axis.text.x  = element_text(angle=90))
print(health)


bp + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
           axis.text.x  = element_text(angle=90, vjust=0.5, size=16))

head(stormdamage)


storm$CEXP <- ifelse(storm$CEXP == "", 0, storm$CEXP)
storm$PEXP <- ifelse(storm$PEXP == "", 0, storm$PEXP)

storm$CEXP
storm$CEXP <- replace(storm$CEXP, storm$CEXP== "K", 1000)aggregate(stormdamage,"Type",sum)
storm$PEXP <- replace(storm$PEXP, storm$PEXP== "K", 1000)
head(storm)

aggregate(stormdamage,"Type",sum)
library(plyr)
library(ggplot2)
library(stringr)

## Create summary table of of the annual emmisions from motor vehicle sources in Baltimore
## Find motor vehicle sources.
Onroadfinder<-as.character(SCC$Data.Category)
Onroadpos<-str_detect(Onroadfinder, "Onroad")
Onroadscc<-SCC[Onroadpos,1]

## Subset Motor Vehicle and Baltimore (fips =="24510")
Onroad<-subset(NEI,NEI$SCC %in% Onroadscc & NEI$fips == "24510")

## Find the total annual emmisions from each source
sources<- ddply(Onroad,c("year"),summarize, Annual_Emissions = sum(Emissions))

## Use ggplot2 to graph the annual emmissions
plot5<-ggplot(data=sources,mapping=aes(x=year, y=Annual_Emissions)) + geom_point() + geom_line() + labs(title = "Annual Emissions from On Road MV Sources in Baltimore")
print(plot5)


Steps<-data.frame(steps$steps,as.Date(steps$date),steps$interval)
names(Steps)<-c("steps","date","interval")



subdata<-ddply(data,"EVTYPE",summarize,sumdeath=sum(FATALITIES)+sum(INJURIES));
subdata<- subdata[with(subdata, order(-sumdeath, EVTYPE)), ][1:50,];
g<-ggplot(subdata,aes(EVTYPE,sumdeath,col = EVTYPE));
g<-g+geom_point(size = 5);
g<-g+labs(x = "Events", y = "CASUALTIES") + coord_flip();
print(g);


# Adjust property and crop damage costs by myltiplying by quantifier
# K=1,000; M=1,000,000; B=1,000,000,000; also create column cmputng overall
# costs

reduced_data$property_dcosts <- rep(0, nrow(reduced_data))
reduced_data$property_dcosts[reduced_data$PROPDMGEXP == "K"] <- reduced_data$PROPDMG[reduced_data$PROPDMGEXP == 
                                                                                       "K"] * 1000
reduced_data$property_dcosts[reduced_data$PROPDMGEXP == "M"] <- reduced_data$PROPDMG[reduced_data$PROPDMGEXP == 
                                                                                       "M"] * 1e+06
reduced_data$property_dcosts[reduced_data$PROPDMGEXP == "B"] <- reduced_data$PROPDMG[reduced_data$PROPDMGEXP == 
                                                                                       "B"] * 1e+09

reduced_data$crop_dcosts <- rep(0, nrow(reduced_data))
reduced_data$crop_dcosts[reduced_data$CROPDMGEXP == "K"] <- reduced_data$PROPDMG[reduced_data$CROPDMGEXP == 
                                                                                   "K"] * 1000
reduced_data$crop_dcosts[reduced_data$CROPDMGEXP == "M"] <- reduced_data$PROPDMG[reduced_data$CROPDMGEXP == 
                                                                                   "M"] * 1e+06
reduced_data$crop_dcosts[reduced_data$CROPDMGEXP == "B"] <- reduced_data$PROPDMG[reduced_data$CROPDMGEXP == 
                                                                                   "B"] * 1e+09

reduced_data$all_damage_billion <- (reduced_data$property_dcosts + reduced_data$crop_dcosts)/1e+09
reduced_data$deaths_injuries <- reduced_data$FATALITIES + reduced_data$INJURIES
reduced_data$property_dcosts <- (reduced_data$property_dcosts)/1e+09
reduced_data$crop_dcosts <- (reduced_data$crop_dcosts)/1e+09

library(ggplot2)
myplot<- ggplot(data, aes(x = xcoord, y = ycoord, colour = group)) +
  geom_point()
print(myplot)





# Create a new variable to determine weekday or weekend
data$day <- "weekday"
data$day[weekdays(data$date) %in% c("Saturday", "Sunday")] <- "weekend"

# Create 2 new dataframe consisting of either weekday or weekend
weekday <- subset(data, day == "weekday")
weekend <- subset(data, day == "weekend")

# Average number of steps for each internval for the two dataframes
avg_weekday <- tapply(weekday$steps, weekday$interval, mean, na.rm= TRUE)
avg_weekend <- tapply(weekend$steps, weekend$interval, mean, na.rm= TRUE)

# Plot results
par(mfrow=c(2,1))
plot(avg_weekday, type="l")
plot(avg_weekend, type="l")

# Replace all NA with average number of steps for that day
sum(is.na(data$steps))
## [1] 2304
data1 <- data
temp <- tapply(data$steps, data$date, mean, simplify=T)
temp <- subset(temp, !is.na(temp))
# If still NA, then replace with average number of steps overall
mean_list <- mean(!is.na(data1$steps))
data1[is.na(data1$steps),1] <- mean_list

# Plot histogram on the number of steps with imputed values
hist(data1$steps)

dataset <- read.csv("activity/activity.csv")

StepsByInterval <- aggregate(steps ~ interval, data = dataset, FUN = mean)

plot(StepsByInterval$interval, StepsByInterval$steps, type = "l")

results2[results2$steps_taken == max(results2$steps_taken), "interval"]




+#read raw data
  
  
  
  +raw.data<-read.table("./household_power_consumption.txt",head=TRUE,sep=";",na.strings="?")



+
  
  
  
  +#merge date & time
  
  
  
  +raw.data$tmp_merge_date_time<-paste(raw.data$Date,raw.data$Time,sep=" ")



+
  
  
  
  +#change variable class into POSIXlt
  
  
  
  +raw.data$merge_date_time<-as.POSIXlt(strptime(raw.data$tmp_merge_date_time, "%d/%m/%Y %H:%M:%S"))



+
  
  
  
  +#subset of Feb 1st & 2nd
  
  
  
  +data<-subset(raw.data,as.Date(raw.data$merge_date_time)==as.Date("2007-02-01","%Y-%m-%d")|
                  
                  
                  
                  +               as.Date(raw.data$merge_date_time)==as.Date("2007-02-02","%Y-%m-%d"))



+
  
  
  
  +#output png file
  
  
  
  +png(filename = "plot4.png",width = 480, height = 480, bg = "transparent")



+
  
  
  
  +##plot
  
  
  
  +par(mfcol=c(2,2))



+#first
  
  
  
  +plot(data$merge_date_time,data$Global_active_power,type="l",col = "black",xlab="",ylab="Global Active Power")



+##second
  
  
  
  +plot(data$merge_date_time,data$Sub_metering_1,type="l",col = "black",xlab="",ylab="Energy sub metering")



+lines(data$merge_date_time,data$Sub_metering_2,col="red")



+lines(data$merge_date_time,data$Sub_metering_3,col="blue")



+legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
        
        
        
        +       ,xjust=0, 
        
        
        
        +       ,lty=1,col=c("black","red","blue"),bg="transparent",box.col=0)



+##third
  
  
  
  +plot(data$merge_date_time,data$Voltage,type="l",col = "black",xlab="datetime",ylab="Voltage")



+
  
  
  
  +##fouth
  
  
  
  +plot(data$merge_date_time,data$Global_reactive_power,type="l",
        
        
        
        +     col = "black",xlab="datetime",ylab="Global_reactive_power")



+
  
  
  
  +
  
  
  
  +#close the graphic device
  
  
  
  +dev.off()


## Random Numbers
mu<-1:10
sigma<-10:1
sample.size<-100
norm.mat<-mapply(function(x,y){rnorm(x,y,n=sample.size)},x=mu,y=sigma)

mean<-1:4
std<-c(.5,.5,.5,.5)
size<-25
set.seed(1)
x<-mapply(function(x,y){rnorm(x,y,n=size)}, x=mean,y=std)
y<-mapply(function(x,y){rnorm(x,y,n=size)}, x=mean,y=std)
data<-cbind(x[,1],y[,1])




groups<-1:4
elements<-25
xmean<-c(1,1,2,2)
ymean<-c(1,2,1,2)
xstd<-.5
ystd<-.5

data<-data.frame()
for  (i in 1:length(groups)) {
  group<-rep(groups[i],elements)
  set.seed(1)
  xcoord<-rnorm(xmean[i],xstd,n=elements)
  ycoord<-rnorm(ymean[i],ystd,n=elements)
  temp<-data.frame(cbind(group,xcoord,ycoord))
  data<-data.frame(rbind(data,temp)) 
}
