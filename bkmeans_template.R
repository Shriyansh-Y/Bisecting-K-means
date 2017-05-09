######
# Bisecting K-Means
#####
rm(list = ls(all = T))

library(RColorBrewer)
library(cluster)
library(fpc)

# set seed to ensure consistent results
set.seed(100)

# Read data file
# When	submitting, ensure that the path to this file is just "hw2-data.csv" 
# and doesn't point	to a path on your machine 
data.df <- read.csv('hw2-data.csv')
sse<-array()
fit<-kmeans(data.df,1)
result<-array()
# TODO: Implement bisecting k means.
# Input:
# data.df: data frame based on hw2-data.csv
# trials.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Be mindful that there might be duplicates in the data.
# terminating condition: when k clusters have been found

bisectingkmeans <- function(data.df, trials.max, k){
 # start your implementation here
#STEP 1:INITIALIZE THE CLUSTER WITH ALL POINTS
  cluster_count=1
  
  sse[1]<-fit$withinss
  total_sse<-fit$totss
  
  result<-fit$cluster
  fit2<-array()
  
#STEP 2:  REPEAT
  while(cluster_count<k){
#STEP 3:  REMOVE A CLUSTER FROM THE LIST OF CLUSTER  
    cluster_selected<-which.max(sse)
    for(i in 1:length(sse)){
      #REMOVING THE PROBLEM OF OVERLAP FOR WITHINSS
      if(i>cluster_selected){
        sse[i+1]<-sse[i]
      }
    }
    for(i in 1:300){
      #REMOVING THE PROBLEM OF OVERLAP
      if(result[i]>cluster_selected){
        result[i]<-result[i]+1
      }
    }
    #GETTING THE IDS OF THE CLUSTERS TO BE UPDATED
    ids<-data.df[result==cluster_selected,]
    x<-c(ids[,1])
    
#STEP 4:  FOR I=1 TO TRIALS.MAX 
    kdata<-data.df[result==cluster_selected,]
    for(i in 1:trials.max){
#STEP 5:  BISECT THE SELECTED CLUSTER
      #RANDOM CENTERS
      trial_fit<-kmeans(kdata,2)
      trial_fit$cluster
      current<-trial_fit$totss
      if(current<=total_sse){
#STEP 6:  SELECT THE LOWEST TOTAL SSE CLUSTER BISECTION 
        total_sse<-current
        #AND UPDATE THE SSE LIST
        sse[cluster_selected]<-trial_fit$withinss[1]
        sse[cluster_selected+1]<-trial_fit$withinss[2]
      
#STEP 7: ADD THE CLUSTER TO THE CLUSTER LIST
    #UPDATING THE FINAL CLUSTER LIST
    for(i in 1:nrow(ids)){
      if(trial_fit$cluster[i]==1){
        result[ids[i,1]]=cluster_selected
      }
      else{
        result[ids[i,1]]=cluster_selected+1
      }
    }
   }
  }
    cluster_count=cluster_count+1
}
#STEP 8:  RETURN "output" 
  plotcluster(data.df,result)
  output<-list()
  output[[1]]<-result
  output[[2]]<-sse
  
  return(output)
}

# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
  c = data.df[c(210, 247, 265, 278, 288),1:3]
  c1<-kmeans(data.df,centers = c)
  plotcluster(data.df,c1$cluster)
  
}

# Don't edit anything beyond this line
# Please note, TA will test with different configurations of trials.max and k.
# NOTE/UPDATE(02/11/17): Fixed typo:Change made to make second variable as trials.max in bisectingkmeans.
k=5
result <- bisectingkmeans(data.df, trials.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)

