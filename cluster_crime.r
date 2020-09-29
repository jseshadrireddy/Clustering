crime=read.csv(file.choose())
View(crime)
#scator plot
plot(Murder~Assault,crime)
with(crime,text(Murder~Assault,labels=X,pos=4,cex=1))#the pos value should be <=4 only
#normalization process
z=crime[,c(2,3,4,5)]
View(z)
m=apply(z,2,mean)#if we will give the 2 then the mean will showes(or)indicates columns
m
m=apply(z,1,mean)#if we will give the 1 then the mean will showes(or)indicates rows
m
#calculation of euclidian distance
d=dist(z)
print(d,digits = 3)
#calculate dendrograms
hc=hclust(d)
plot(hc)
plot(hc,labels=crime$X)
plot(hc,hang = -1)
#cluster dendrograms with avg linkage
hc.a=hclust(d,method = 'complete')
plot(hc.a,hang = -1)
#cluster membarship
member.c=cutree(hc,3)
member.c
plot(member.c)
member.a=cutree(hc.a,3)
member.a
plot(member.a)
table(member.c,member.a)
#cluster means
aggregate(z,list(member.c),mean)#use in this formate or 
aggregate(crime[,c(2,3,4,5)],list(member.c),mean)#use in this formate also we should get same result
#we can use member.c or member.a then we get same result
aggregate(z,list(member.a),mean)#use in this formate or 
aggregate(crime[,c(2,3,4,5)],list(member.a),mean)#use in this formate also we should get same result
##silhoutte plot
library(cluster)
plot(silhouette(cutree(hc,3),d))
##scree plot
wss=(nrow(z)-1)*sum(apply(z,2,var))
for(i in 2:20)wss[i]=sum(kmeans(z,centers = i)$withinss
plot(1:20,wss,type ='b', xlab='number of clusters',ylab = 'within groupss')
#k-means cluster
kc=kmeans(z,3)
kc
plot(Murder~Assault,crime,col=kc$cluster)
