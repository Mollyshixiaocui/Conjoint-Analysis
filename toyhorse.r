library(ggplot2)
library(reshape)
library(cluster)
library(fpc)

load("~/conjoint Data.Rdata")
DFdesmat=data.frame(desmat)
names(DFdesmat)=c("price","size","motion","style")
fullDB=data.frame(DFdesmat,ratings,ageD,genderD,ID)

#priori gender segment

reg.gender.0=lm(ratings~price+size+motion+style,fullDB,subset = genderD==0)
summary(reg.gender.0)
reg.gender.1=lm(ratings~price+size+motion+style,fullDB,subset = genderD==1)
gender0=reg.gender.0$coefficients
gender1=reg.gender.1$coefficients
gender=data.frame(gender1,gender0)
#check significant
reg.gender=lm(ratings~(price+size+motion+style)*genderD,fullDB)
summary(reg.gender)
#priori age segment
reg.age.0=lm(ratings~price+size+motion+style,fullDB,subset = ageD==0)
summary(reg.age.0)
reg.age.1=lm(ratings~price+size+motion+style,fullDB,subset = ageD==1)
age0=reg.age.0$coefficients
age1=reg.age.1$coefficients
age=data.frame(age1,age0)
#check significant
reg.age=lm(ratings~(price+size+motion+style)*ageD,fullDB)
summary(reg.age)

#find the partworths of each individual
partworths = matrix(nrow=sampsize,ncol = 5)
for (i in 1:sampsize) partworths[i,]=lm(ratings~price+size+motion+style,fullDB,subset = ID==i)$coef

#post hoc segmentation by cluster
#find the optimal cluster number
pm1=pamk(partworths,scaling = TRUE)
pm1$nc # three clusters, we should try 4 clusters too
wss=c()
for (i in 2:15) {
  wss[i-1]=sum(kmeans(partworths,i)$withinss)
}
plot(2:15,wss,type = "b",main="Within group sum of squared VS Number of Clusters")#best cluster 3

#visualize the clusters
km3=kmeans(partworths,3,iter.max = 200, nstart = 3)
km4=kmeans(partworths,4,iter.max = 200, nstart = 3)
clusplot(partworths,km3$cluster,main="3 clusters",shade=TRUE,color=TRUE,line=0)
clusplot(partworths,km4$cluster,main="4 clusters",shade=TRUE,color=TRUE,line=0)
#3 clusters is better

#part-utility mean of each segment
names(km3)
summary(km3)
km3
km4

#segment size of each segments
prop.table(table(km3$cluster))
pie(prop.table(table(km3$cluster)))
prop.table(table(km4$cluster))

#predict customers' utility
partworthsfull=matrix(rep(partworths,each=16),ncol = 5)
dim(partworthsfull)
constant=rep(1,nrow(desmat))
desmatf=cbind(constant,desmat)
predictRT=rowSums(desmatf*partworthsfull)
#combine predicttions and actual ratings
ratingsfull=ifelse(is.na(ratings),predictRT,ratings)
#convert the rating vector into matrix
ratingsmatrix=matrix(ratingsfull,nrow = 16)#every customer in a column, matrix() is column-wise by default.

#Market simulation
#define function to calculate market share
MS=function(rating,scen){
  n=length(scen)#number of configurations
  rating=rating[scen,]
  Max=apply(rating,2,max)
  is.max=(rating==rep(Max,each=n)) #boolean matrix, "=="operato is column wise
  tie=matrix(rep(colSums(is.max),each=n),nrow=n) 
  indivitualMS=is.max/tie
  return(rowMeans(indivitualMS))
}

# 16 configurations
desmat[1:16,]
ms.4.13=MS(ratingsmatrix,c(4,13))
ms.4.13
ms.13.5.7=MS(ratingsmatrix,c(13,5,7))
ms.13.5.7
ms.11.7=MS(ratingsmatrix,c(11,7))
# all possible products for us
product=c(1,2,3,4,5,6,9,10,11,12,13,14,15,16)
# price for our products
price=rep(c(111.99,95.99),8)
length(price)
#variable cost for our products
vc=rep(rep(c(21,29,33,41),each=2),2)
length(vc)
#one product
name=c()
p.noreact.one=c()
p.comp.noreac.one=c()
p.react.one=c()
p.comp.react.one=c()
for(i in product){
  name[i]=paste("ms",i,"7",sep=".")
  ms.one=MS(ratingsmatrix,c(i,7))
  p.noreact.one[i]=price[i]*4000*ms.one[1]-(4000*ms.one[1]*vc[i]+20000)
  p.comp.noreac.one[i]=111.99*4000*ms.one[2]-(4000*ms.one[2]*41+20000)
  ms.one.react=MS(ratingsmatrix,c(i,8))
  p.react.one[i]=price[i]*4000*ms.one.react[1]-(4000*ms.one.react[1]*vc[i]+20000)
  p.comp.react.one[i]=95.99*4000*ms.one.react[2]-(4000*ms.one.react[2]*41+20000)
}
result_one=cbind(name,p.noreact.one,p.comp.noreac.one,p.react.one,p.comp.react.one)
result_one=na.omit(result_one)
write.csv(result_one,file = "result_one.csv")

# two products
noreaction_result=data.frame()
reaction_result=data.frame()
senario_noreaction=c()
senario_reaction=c()
profit.comp.noreact=c()
profit.comp.react=c()
profit.noreact=c()
profit.react=c()
reaction=c()
for (i in c(1,2,3,4,5,6,9,10,11,12,13,14,15,16)) {
  for (j in c(1,2,3,4,5,6,9,10,11,12,13,14,15,16)){
    if (i==j) next #不是同一个产品
    if ((i%%2==1)&(j==i+1)) next #同一个配置不同价格
    if ((i%%2==0)&(j==i-1)) next
    for (k in product){
      if (k==j) next
      if ((j%%2==1)&(k==j+1)) next
      if ((j%%2==0)&(k==j-1)) next
      name1=paste("ms",i,j,k,"7", sep=".") #product combination/senario name, no reaction
      ms.no=MS(ratingsmatrix,c(i,j,k,7)) #market share if no reaction
      #profit of competitor if no reaction
      p.comp.noreact=111.99*4000*ms.no[4]-(4000*ms.no[4]*41+20000)
      #market share if competitor react
      ms.re=MS(ratingsmatrix,c(i,j,k,8))
      name2=paste("ms",i,j,k,"8", sep=".")  #product combination/senario name, have reaction
      p.comp.react=95.99*4000*ms.re[4]-(4000*ms.re[4]*41+20000)
      #compare the profit of reaction and no reaction for competitor
      if (p.comp.react>p.comp.noreact) {reaction[(i-1)*14+(j-1)*14+k]="yes"}
      else if (p.comp.react==p.comp.noreact) { reaction[(i-1)*14+(j-1)*14+k]="tie" }
      else if (p.comp.react<p.comp.noreact) {reaction[(i-1)*14+(j-1)*14+k]="no"}
      #calculate our profit
      p.noreact=price[i]*4000*ms.no[1]+price[j]*4000*ms.no[2]-
        (4000*ms.no[1]*vc[i]+4000*ms.no[2]*vc[j]+20000*2)
      p.react=price[i]*4000*ms.re[1]+price[j]*4000*ms.re[2]+price[k]*4000*ms.re[3]-
        (4000*ms.re[1]*vc[i]+4000*ms.re[2]*vc[j]+4000*ms.re[3]*vc[k]+20000*3)
      #store the result
      senario_noreaction[(i-1)*14+(j-1)*14+k]=name1
      senario_reaction[(i-1)*14+(j-1)*14+k]=name2
      profit.noreact[(i-1)*14+j]=p.noreact
      profit.react[(i-1)*14+(j-1)*14+k]=p.react
      profit.comp.noreact[(i-1)*14+j]=p.comp.noreact
      profit.comp.react[(i-1)*14+(j-1)*14+k]=p.comp.react
      
    }
    
    }
}
#noreaction_result=cbind(senario_noreaction,profit.noreact,profit.comp.noreact,reaction)
reaction_result=cbind(senario_reaction,profit.react,profit.comp.react)
#noreaction_result=na.omit(noreaction_result)
reaction_result=na.omit(reaction_result)
write.csv(reaction_result,file = "reaction_result.csv")

# 3 products
ms.2.15.14.8=MS(ratingsmatrix,c(2,15,14,8))
p.2.15.14.8=price[2]*4000*ms.2.15.14.8[1]+price[15]*4000*ms.2.15.14.8[2]+
  price[14]*4000*ms.2.15.14.8[3]-(4000*ms.2.15.14.8[1]*vc[2]+4000*ms.2.15.14.8[2]*vc[15]+
  4000*ms.2.15.14.8[3]*vc[14]+20000*3)
ms.1.11.13.7=MS(ratingsmatrix,c(1,11,13,7))
p.c.1.11.13.7=111.9*4000*ms.1.11.13.7[4]-(4000*ms.1.11.13.7[4]*41+20000)
ms.1.11.13.8=MS(ratingsmatrix,c(1,11,13,8))
p.c.1.11.13.8=95.99*4000*ms.1.11.13.8[4]-(4000*ms.1.11.13.8[4]*41+20000)
ms.4.6.16.8=MS(ratingsmatrix,c(4,6,16,8))  
    p.4.6.16.8=price[4]*4000*ms.4.6.16.8[1]+price[6]*4000*ms.4.6.16.8[2]+
  price[16]*4000*ms.4.6.16.8[3]-(4000*ms.4.6.16.8[1]*vc[4]+4000*ms.4.6.16.8[2]*vc[6]+
                                    4000*ms.4.6.16.8[3]*vc[16]+20000*3)
