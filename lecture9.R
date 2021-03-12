# lecture 10: pca

df<-read.csv("Lectures/MKSdata_cleaned.csv", sep=",")
num_vars<-c("Sales_rank_week1","Sales_rank_week6","Sales_rank_week12","Sales_rank_week18","Sales_rank_week24",
            "Price_week1","Price_week6","Price_week12","Price_week18","Price_week24" )

pca1<-princomp(na.omit(df[,num_vars]), cor=T)
summary(pca1)
pca1$loadings
plot(pca1$scores[,1],pca1$scores[,2])
legend("topright",paste("correlation = ",cor(pca1$scores[,1],pca1$scores[,2])))

plot(pca1$scores[,1],pca1$scores[,3])
legend("topright",paste("correlation = ",cor(pca1$scores[,1],pca1$scores[,3])))
pairs(pca1$scores)
## Faces example

#You will analyze faces. Each line in the dataset is a face, each number is a pixel. Each face has 50*50 pixels. Use `read.table`, find the dataset faces on canvas.
getwd()
faces <- read.table("Lectures/faces.txt", sep=";", header=F, skip=1)
faces <- faces[,2:ncol(faces)] # delete column with rownames
dim(faces)
faces[1:3,1:10]

# Each line contains 2500 numbers that represents the darkness/lightness of a pixel
# You can create the image from the data using `image`
# as input for `image` you will need to rearrange the 2500 numbers as a matrix
# You will have to set the `par` margins to zero
# for some unknown reason, R stores this datafile as a list, and you will need to use unlist 
# before you can perform calculations. Unlist only works for one line at one time


par(mfrow=c(5,5), mar=c(0, 0, 0, 0))
for(i in seq(1,50,2)){
  m <- matrix(unlist(faces[i,]),50,50, byrow=F)
  image(m, useRaster=TRUE, axes=FALSE)
} 

# Analyze the faces using pca on entire dataset
# The number components is equal to the number of faces
# Each component is a eigenvector of 2500 values
# The eigenvalue shows how important the component is. Use `prcomp`

faces.pca <- prcomp(t(faces))
summary(faces.pca)
dim(faces.pca$x)
faces.pca$x[1:10,1:5] # first five components for first ten faces

# Reproduce the faces using the three largest components
# Can you see what the components measure?

par(mfrow=c(5,5), mar=c(0, 0, 0, 0))
for(i in 25:1){
  m <- matrix(faces.pca$x[,i],50,50, byrow=F)
  image(m, useRaster=TRUE, axes=FALSE)
}


# In a subsequent analysis, you could use weights (similar to factor loadings) to give more 
# importance to some components than others

dim(faces.pca$rotation)
