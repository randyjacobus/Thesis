setwd("Users/rolandjacobus/Documents/Thesis")
yields<-read.csv("Futures.csv")
names(yields)<-c("DATE","T25","T15")
yields$Curve<-yields[,2]-yields[,3]
Diff<-diff(yields$Curve,lag=1)*100
yields<-yields[-1,]
yields$diff<-Diff

summary(yields$diff)
hist(yields$diff)

## Initialize Counters and Vectors
dates=NULL
curve=NULL
diff=NULL
end<-length(yields$diff)
trigger<-2*sd(yields$diff)
k=1

## Loop to find triggers
for (i in 1:end) {
  if(abs(yields[i,5])>trigger) {
    
    dates[k]=yields[i,1]
    curve[k]=yields[i,4]
    diff[k]=yields[i,5] 
    k=k+1
    
  }

}
results<-cbind(dates,curve,diff)
head(results)