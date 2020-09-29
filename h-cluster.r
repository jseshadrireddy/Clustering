##loding packages
library(readr)
#importing data
crime_data=read.csv(file.choose())
View(crime_data)
attach(crime_data)
summary(crime_data)
str(crime_data)
sum(is.na(crime_data))
crime_data1=crime_data[, 2:5]
View(crime_data1)
#Normalizing the data
normalized_data=scale(crime_data1)
normalized_data
#distance matrix
d=dist(normalized_data, method="euclidean")
#Heirarchical clustering using complete linkage method
fit=hclust(d, method="complete")
fit
#dendrogram
plot(fit)
plot(fit, hang=-1)
rect.hclust(fit,plot(fit, hang=-1), k=4, border="green")
#using Cutree
groups=cutree(fit, k=4)
membership=as.matrix(groups)
final=data.frame(crime_data, membership)
final1=final[,c(ncol(final),1:(ncol(final)-1))]
final1
agg=aggregate(crime_data[,-1],by=list(final$membership),mean)
agg
#group 2 have high crime rata