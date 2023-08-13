# CUSTOMER SEGMENTATION

#     It is a process of division of consumer base into several groups of individuals
# that shares a similarity in different ways that are relevant to marketing.


customer_data=read.csv("D:/R PROGRAM/Mall_Customers.csv")

str(customer_data)
names(customer_data)
head(customer_data)

summary(customer_data$Age)
sd(customer_data$Age)

summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)

summary(customer_data$Spending.Score..1.100.)
sd(customer_data$Spending.Score..1.100.)


###################### GENDER COMAPRISON ( BAR PLOT,PIE CHART) #########################################################

library(ggplot2)
a=table(customer_data$Genre)
barplot(a,main="Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(3),
        xlim=c(0,3),
        ylim=c(0,200),
        legend=rownames(a))

########################## PIE CHART ###################################################################################

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")



####################################### CUSTOMER AGE ( HISTOGRAM,BOXPLOT) ###################################################

summary(customer_data$Age)

hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col='green',
        main="Boxplot for Descriptive Analysis of Age")


###################################### ANNUAL INCOME (HISTOGRAM,PLOT DENSITY) ########################################

summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")

polygon(density(customer_data$Annual.Income..k..),col="pink")


####################### SPENDING SCORE ( HISTOGRAM,BOXPLOT) ########################

summary(customer_data$Spending.Score..1.100.)

hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)

boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")

##################################### ELBOW METHOD ( NO OF CLUSTERS) ###############################################


library(cluster)
library(purrr)
set.seed(123)

# Total intra-cluster sum of square 

iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")




################################# GAP STATIC METHOD ###################################################

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)


library(NbClust)
library(factoextra)

############################# PRINCIPAL COMPONENT #######################################################

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]


############################ VISUALIZE CLUSTERS #######################################

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")




ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
