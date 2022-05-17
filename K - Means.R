#import required dataset
data = read.csv("Walmart Sale DataSet.csv")
# Summary Stats
summary(data)
# activate psych package to run describe function
library(psych)
describe(data)
head(data);tail(data)
# Remove unnecessary columns - "date" is not unnecessary 
Data = subset (data, select = -2)
head(Data)
# Check for Missing values - Count
p <- function(x) {sum(is.na(x))}
apply(Data,2,p)
# From the above result its clear that our data do not have any missing values
# Build K Means Methodology on Walmart data and find patterns
#step 1: First find the optimal number of clusters
# here, for this we will use elbow methods to find the number of cluster using total within sum of squares.
# import required libraries to build K Means clusterting
library(factoextra);library(cluster)
# we use Fviz_ncluster() function to find the optimal k value
fviz_nbclust(Data,kmeans,method="wss")
# optional - if you are not clear with  finding out the optimal K values using elbow method use gap stats method to make sure we are using correct k value.
gap_stat=clusGap(Data, FUN = kmeans,nstart=25, K.max=10, B= 50)
fviz_gap_stat(gap_stat)
#step 2: Perform K means clustering using optimal K
set.seed(1)
km=kmeans(Data,centers = 4,nstart=25)
km
# Plot the cluster using fviz cluster function
fviz_cluster(km, data= Data)
aggregate(Data, by=list(cluster=km$cluster), mean)
final_result=cbind(Data,cluster=km$cluster)
head(final_result)
write.csv(final_result,"Kmeans_Result.csv")
