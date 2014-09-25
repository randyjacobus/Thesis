setwd("Users/rolandjacobus/Documents/Thesis")
## Read in Data
yields<-read.csv("Futures.csv")

## set up yield data
names(yields)<-c("DATE","T25","T15")
yields$Curve<-yields[,2]-yields[,3]
Diff<-diff(yields$Curve,lag=1)*100
yields<-yields[-1,]
yields$diff<-Diff

## what does data look like
summary(yields$diff)
hist(yields$diff)

## set trigger for trade
trigger<-3*sd(yields$diff)

## define variables
results<-as.data.frame(results)
class(results)
colnames(results)<-c('dates','curve1','diff',"curve2","PL","Record")
results[,1:5]<-sapply(results[,1:5],as.numeric)

days<-1
end<-length(yields$diff)

k=1 ## counter for how many times we hit trigger

## Loop to find triggers
for (i in 1:end) {
  if(abs(yields[i,5])>trigger) {
    results[k,1]<-yields[i,1] ## set date
    results[k,2]<-yields[i,4] ## set curve
    results[k,3]<-yields[i,5] ## set diff
    results[k,4]<-yields[i+days,4] ## next day curve
    results[k,5]<-if(results[k,3]>0) {  ## determine pl.
      
      (yields[i,4]-yields[i+days,4])*100
    } else {
      (yields[i+days,4]-yields[i,4])*100
    }
    results[k,6]<-if(results[k,5]>0) {
      'W'
    } else {
      "L"
    }

    k=k+1  ## counter for results data frame  
  }  
}

sapply(results,mode)
head(results)
hist(results$PL)
