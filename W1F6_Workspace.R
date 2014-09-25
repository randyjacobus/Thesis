#W1F6session 
#If you source() this file you will get errors. You can probably figure out why.
#It's beacause the code shows what happens when you use a name that is hidden.
#Go for file/ open script    or  use copy paste into R console

getwd()
#setwd() 
dir()
ls()
pi
pi <- 12
pi
ls()
rm(pi)
ls()
data()
head(infert)
names(infert)
education
attach(infert)
require()
education
education <- 3
detach()
head(infert$education)
save(infert, file="infertprocessed.rda")
dir()
# load()
rm(list=ls())
search()
install.packages("maps")
#select CRAN mirror
search()
library("maps")
map("world")
points(18,59)
points(18,59,pch=19, col="blue")
history()  #doesn't work in MAC
save.image() #same as clicing  save workspace
save.image(file="lifesciencedataproject.RData")
load("lifesciencedataproject.RData")
