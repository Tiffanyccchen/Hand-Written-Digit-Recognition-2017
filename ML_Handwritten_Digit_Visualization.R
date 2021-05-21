## Load and Check the Data

#{r load data, message=FALSE}
library(readr)
library(caret)

dir<- getwd()
train <- read_csv(dir + "/train.csv")
test <- read_csv(dir + "/test.csv")


#Let's check the data in train
#{r check data}
dim(train)
table(as.factor(train$label))
ggplot(train,aes(x=as.factor(label),fill=label))+
  geom_bar(stat="count",color="white")+
  scale_fill_gradient(low="lightblue",high="pink",guide=FALSE)+
  labs(title="Digits in Train Data",x="Digits")

#There are `r dim(train)[1]` rows and `r dim(train)[2]` columns in the train data. It seems that the number of digits has little difference. 
#Here we know the labels are handwritten digits, what do they look like? Let's just have a look. I randomly sample 50 rows from train data set and turn them to images:

#{r images}
sample <- sample(1:nrow(train),50)
var <- t(train[sample,-1])
summary(var)
var_matrix <- lapply(1:50,function(x) matrix(var[,x],ncol=28))
opar <- par(no.readonly = T)

par(mfrow=c(5,10),mar=c(.1,.1,.1,.1))

for(i in 1:50) {
  for(j in 1:28) {
    var_matrix[[i]][j,] <- rev(var_matrix[[i]][j,])
  }
  image(var_matrix[[i]],col=grey.colors(225),axes=F)
}
par(opar)

var_matrix

#Quick look at the Digits data
#Intro
#Plotting
#Mean
#Median
#Standard Deviation
#Intro
#Hi! This is a short script that separates out the digits by label and 
# plots the pixel-wise mean, median and standard deviation of the digits data for each of the ten labels.

# Load libraries
library(readr)

# Read in data

#First we will separate out the digits into separate lists according to which label they have
#split training data into list by label number
digit_groups <- split(train, train$label)

#Next we remove the label column from every image because we do not want it to affect 
#our calculations involving the raw pixels. We also define a function called pixel-wise. 
#This is the function we will use to compute our pixel based statistics.

# Remove label column in each
dg2 <- lapply(digit_groups, function(x) { x["label"] <- NULL; x })
summary(dg2)
gs1 = grey(c(0:128)/128)


# Pixel-wise function
pixel_wise <- function(df, func) {
  output <- apply(t(df),1, mean)
  u <- matrix(output, ncol = 28)
  v <- u
  for(i in 1:28)  {
    v[i,] <- rev(u[i,])
  }
  image(v, col = gs1, axes = FALSE) 
}

#Plotting

#Mean
#First we will visualise the mean of each digit that we previously separated into their own lists.
# Plot each digits mean
par(mfrow=c(4,3))
plot <- lapply(dg2, pixel_wise, func = mean)
title("Pixel-wise Mean", outer=TRUE, line = -2)

#Median
#Next we will do the same for each digits median.
# Plot each digits median
par(mfrow=c(4,3))
plot <- lapply(dg2, pixel_wise, func = median)
title("Pixel-wise Median", outer=TRUE, line = -2)

#Standard Deviation
#Finally we look at each digits standard deviation, this will give us an idea of how each digit varies in their respective samples.
# Plot each digits standard deviation
par(mfrow=c(4,3))
plot <- lapply(dg2, pixel_wise, func = sd)
title("Pixel-wise Standard Deviation", outer=TRUE, line = -2)