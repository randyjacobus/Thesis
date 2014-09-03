## Load libraries
library(ggplot2)
library(rpud)
## Generate 3 sets of random numbers with different centers and name them
groups<-1:3
elements<-100
concentration<-50
xmean<-c(1,1.5,2)
ymean<-c(1,2,1)
xstd<-.25
ystd<-.25
xstdconc<-.05
ystdconc<-.05

data<-data.frame()
for  (i in 1:length(groups)) {
     group<-rep(groups[i],elements)
     groupconc<-rep(groups[i],concentration)
     
     set.seed(1)
     xcoord<-rnorm(xmean[i],xstd,n=elements)
     ycoord<-rnorm(ymean[i],ystd,n=elements)
     temp<-data.frame(cbind(group,xcoord,ycoord))
     
     xcoord<-rnorm(xmean[i],xstdconc,n=concentration)
     ycoord<-rnorm(ymean[i],ystdconc,n=concentration)
     temp2<-data.frame(cbind(group=groupconc,xcoord,ycoord)) 
     
     temp3<-data.frame(rbind(temp,temp2))
     
     data<-data.frame(rbind(data,temp3)) 
 }

## Graph this dataframe using a different colors for each group.
myplot<- ggplot(data, aes(x = xcoord, y = ycoord, colour = group)) +
  geom_point()
  print(myplot)

## Construct a heatmap-- need to fix this
myheat<- ggplot(data, aes(x = xcoord, y = ycoord, colour = group)) +
  geom_tile()
print(myheat)

## Find a distance matrix
as.matrix(dist(data))

## How to rank distances

## Find a measurement of the distances and choose a threshold that defines a cluster?

## How do you define a cluster?


# get rid of legend

# nearest neighbor

# kmeans with min threshold


## Run Kmeans