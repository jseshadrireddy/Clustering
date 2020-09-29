ewair=read.csv(file.choose())
View(ewair)
#scator plot
plot(Balance~Days_since_enroll,ewair)
with(ewair,text(Balance~Days_since_enroll,labels=ID.,pos=4,cex=1))#the pos value should be <=4 only
#normalization process
z=ewair[,-1]
View(z)
m=apply(z,2,mean)
m
#calculation of euclidean distance
d=dist(z,method ='euclidean')
print(d)
str(d)
#calculate dendrograms
hc=hclust(d)
plot(hc)
plot(hc,labels=ewair$ID.)
plot(hc,hang = -1)
#cluster dendrograms with avg linkage
hc.a=hclust(d,method = 'average')
plot(hc.a,hang = -1)
rect.hclust(hc.a,plot(hc.a, hang=-1), k=10, border="red")
#cluster membarship
member.c=cutree(hc,3)
member.c
plot(member.c)
member.a=cutree(hc.a,3)
member.a
plot(member.a)
table(member.c,member.a)
#cluster means
aggregate(z,list(member.c),mean)
##silhoutte plot
library(cluster)
plot(silhouette(cutree(hc,3),d))
##scree plot
wss=(nrow(z)-1)*sum(apply(z,2,var))
for(i in 2:10){wss[i]=sum(kmeans(z,centers = i)$withinss)}
plot(1:10,wss,type ='b', xlab='number of clusters',ylab = 'within groupss')
#k-means cluster
kc=kmeans(z,5)
kc
str(kc)
plot(Balance~Days_since_enroll,ewair,col=kc$cluster,frame=TRUE)
aggregate(z[,-1], by=list(member.c), FUN = mean)                         
