getwd()
df<-read.csv("Lectures/MKSdata_cleaned.csv", sep=",")

# lecture 7
colnames(df)
columns<- colnames(df)[c(4:6,21)]
cormat<-cor(df[,columns], use="pairwise.complete.obs")
library(reshape2)
melted_cormat <- melt(round(cormat,2), na.rm = TRUE)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

library(MASS)
lda1<-lda(Android~Sales_rank_week1+List_price_week1+Num_reviews_week1, df)
dff<-na.omit(df[,columns])
table(dff$Android,predict(lda1)$class)


mydata<-df[,columns[c(3,2,1,4)]]
colnames(mydata)<-c("reviews","price","sales","Android")
mydata<-na.omit(mydata)

# generate data
N<-100
sales  <- seq(min(dff[,columns[1]]), max(dff[,columns[1]]), length=N)
price  <- seq(min(dff[,columns[2]]), max(dff[,columns[2]]), length=N)
reviews  <- seq(min(dff[,columns[3]]), max(dff[,columns[3]]), length=N)

# select two variables
newdata <- expand.grid(x=reviews,y=price)
colnames(newdata)<-c("reviews","price")

# predict values for new data & perform the lda on these two variables
mylda1 <- lda(Android~reviews+price , mydata)
prd    <- predict(mylda1, newdata=newdata)$class

# plot the data for two variables reviews and price
plot(mydata[,c(1,2)], col = factor(mydata$Android))
points(mylda1$means, pch = "+", cex = 3, col = c("black","red"))
contour(x = reviews, y = price, z = matrix(prd,N,N), 
        levels = c(0,1), add = TRUE, drawlabels = TRUE)

### do the same for qda
myqda1 <- qda(Android~reviews+price, mydata)
prd    <- predict(myqda1, newdata=newdata)$class

plot(mydata[,c(1,2)], col = factor(mydata$Android))
points(myqda1$means, pch = "+", cex = 3, col = c("black","red"))
contour(x = reviews, y = price, z = matrix(prd,N,N), 
        levels = c(0,1), add = TRUE, drawlabels = TRUE)


## test normality
apply(dff[dff$Android==1,columns[1:3]], 2 , shapiro.test)
apply(dff[dff$Android==0,columns[1:3]], 2 , shapiro.test)

install.packages("MVN")
library(MVN)

mvn(dff)
#######################################################################
## now with multiple classes:
columns<- colnames(df)[c(4:6,18)]
mydata<-df[,columns[c(3,2,1,4)]] # to reorder the columns
colnames(mydata)<-c("reviews","price","sales","Operating_SystemD")
mydata<-na.omit(mydata)

# make numeric for contour
mydata$OS <- 1
mydata$OS <- ifelse(mydata$Operating_SystemD=="Apple",2, mydata$OS)
mydata$OS <- ifelse(mydata$Operating_SystemD=="Windows",3, mydata$OS)

# generate data
N<-100
sales  <- seq(min(dff[,columns[1]]), max(dff[,columns[1]]), length=N)
price  <- seq(min(dff[,columns[2]]), max(dff[,columns[2]]), length=N)
reviews  <- seq(min(dff[,columns[3]]), max(dff[,columns[3]]), length=N)

# select two variables
newdata <- expand.grid(x=reviews,y=price)
colnames(newdata)<-c("reviews","price")

# predict values for new data & perform the lda on these two variables
mylda1 <- lda(OS~reviews+price , mydata) 
#mylda1 <- lda(Operating_SystemD~reviews+price, mydata)# same results if character
prd    <- predict(mylda1, newdata=newdata)$class

# plot the data for two variables reviews and price
plot(mydata[,c(1,2)], col = mydata$OS)
points(mylda1$means, pch = "+", cex = 3, col = c("black","red","green")) # for some reason windows is green in points
contour(x = reviews, y = price, z = matrix(prd,N,N), 
        levels = c(1,2,3), add = TRUE, drawlabels = TRUE)

### do the same for qda
myqda1 <- qda(OS~reviews+price, mydata)
prd    <- predict(myqda1, newdata=newdata)$class

plot(mydata[,c(1,2)], col = factor(mydata$OS))
points(myqda1$means, pch = "+", cex = 3, col = c("black","red"))
contour(x = reviews, y = price, z = matrix(prd,N,N), 
        levels = c(1,2,3), add = TRUE, drawlabels = TRUE)

#######################################################################
## lecture 8

apply(df[,c("reviews","accommodates")],1,mean, na.rm=T)

mutate(df[,c("reviews","accommodates")], (reviews+accommodates)/2)

df %>% 
  select(c("reviews","accommodates")) %>% 
  mutate((reviews+accommodates)/2)

apply(df[,c("reviews","accommodates")],2,mean, na.rm=T)
mutate(df[,c("reviews","accommodates")], funs(mean, na.rm=T))

summarytab<-df %>% 
  summarise(Mean_reviews = mean(reviews), 
            Mean_accommodates = mean(accommodates, na.rm=T))
