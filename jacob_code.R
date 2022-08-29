
set.seed(5150)
 






##This chunk of code was used to determine if this feature
#was worth keeping. it was found that most of it was the same observation,
#so it was deleted
thing<-mushrooms$gill.attachment


counter<-0
for(i in 1:length(thing)){
  if(thing[i]=='f'){
    counter<-counter+1
  }
  
}
counter/length(thing)



#used to visualize the content of the features
#of the dataset
table(mushrooms$class)
table(mushrooms$cap.shape)
table(mushrooms$cap.surface)
table(mushrooms$cap.color)
table(mushrooms$bruises)
table(mushrooms$odor)
table(mushrooms$gill.attachment)
table(mushrooms$gill.spacing)
table(mushrooms$gill.size)
table(mushrooms$gill.color)
table(mushrooms$stalk.shape)
table(mushrooms$stalk.root)
table(mushrooms$stalk.surface.above.ring)
table(mushrooms$stalk.surface.below.ring)
table(mushrooms$stalk.color.above.ring)
table(mushrooms$stalk.color.below.ring)
table(mushrooms$veil.type)
table(mushrooms$veil.color)
table(mushrooms$ring.number)
table(mushrooms$ring.type)
table(mushrooms$spore.print.color)
table(mushrooms$population)
table(mushrooms$habitat)



#used to determine the similarity between this feature as well as 
#slalk.color.above and below the ring
slt_surface_above<-mushrooms$stalk.surface.above.ring
slt_surface_below<-mushrooms$stalk.surface.below.ring

thing<-rep(FALSE,length(slt_surface_below))
for(i in 1:length(slt_surface_below)){
  if(slt_surface_below[i] == slt_surface_above[i]){thing[i]<-TRUE}
}

sum(thing)/length(thing)


stl_color_above<-mushrooms$stalk.color.above.ring
stl_color_below<-mushrooms$stalk.color.below.ring


thing<-rep(FALSE,length(stl_color_below))
for(i in 1:length(stl_color_below)){
  if(stl_color_below[i] == stl_color_above[i]){thing[i]<-TRUE}
}

sum(thing)/length(thing)


#used to determine how much data was missing in 
#this feature
thing<-mushrooms$stalk.root
length(thing[thing == '?'])/length(thing)



####We see that gil.attatchment should be deleted outright as it 
####has nearly all the same values (97%), as well as viel.type not being 
####worth keeping. So we will amend the dataset in the following way:
my_mushrooms<-subset(mushrooms,select=-c(veil.type,stalk.surface.below.ring,
                                         gill.attachment,
                                         stalk.color.above.ring))


#turns the variables into factors
my_mushrooms$class<-as.factor(my_mushrooms$class)
my_mushrooms$cap.color<-as.factor(my_mushrooms$cap.color)
my_mushrooms$gill.spacing<-as.factor(my_mushrooms$gill.spacing)
my_mushrooms$stalk.shape<-as.factor(my_mushrooms$stalk.shape)
my_mushrooms$stalk.surface.below.ring<-as.factor(my_mushrooms$stalk.surface.below.ring)
my_mushrooms$spore.print.color<-as.factor(my_mushrooms$spore.print.color)
my_mushrooms$cap.shape<-as.factor(my_mushrooms$cap.shape)
my_mushrooms$bruises<-as.factor(my_mushrooms$bruises)
my_mushrooms$gill.size<-as.factor(my_mushrooms$gill.size)
my_mushrooms$stalk.root<-as.factor(my_mushrooms$stalk.root)
my_mushrooms$stalk.color.above.ring<-as.factor(my_mushrooms$stalk.color.above.ring)
my_mushrooms$population<-as.factor(my_mushrooms$population)
my_mushrooms$stalk.color.above.ring<-as.factor(my_mushrooms$stalk.color.above.ring)
my_mushrooms$cap.surface<-as.factor(my_mushrooms$cap.surface)
my_mushrooms$odor<-as.factor(my_mushrooms$odor)
my_mushrooms$gill.color<-as.factor(my_mushrooms$gill.color)
my_mushrooms$stalk.surface.above.ring<-as.factor(my_mushrooms$stalk.surface.above.ring)
my_mushrooms$ring.type<-as.factor(my_mushrooms$ring.type)
my_mushrooms$stalk.color.below.ring<-as.factor(my_mushrooms$stalk.color.below.ring)
my_mushrooms$habitat<-as.factor(my_mushrooms$habitat)
my_mushrooms$veil.color<-as.factor(my_mushrooms$veil.color)


#get rid of stalk root
analyzed_mushrooms<-subset(my_mushrooms,select=-c(stalk.root))












##our goal is too impute the missing values but some of the variables have too
##much colinearity between them. Use MCA to determine the associated variables

library(FactoMineR)
library(ggplot2)

##mca on full dataset
my_mca<-MCA(mushrooms,graph=T)

##mca on trimmed dataset
my_mca<-MCA(analyzed_mushrooms,graph=T)


##subset to analyze poisonous mushrooms
poison<-analyzed_mushrooms[my_mushrooms$class == 'e',]


##get rid of the class as its no longer useful
poison<-subset(poison,select=-class)


#MCA of the poison data
my_MCA<-MCA(poison,graph=TRUE)


##this plot reveals that the data clusters into several very distincy groups. 
##the only thing we need to do is determine the optimal clustering algorithm.




###for plotting and visualization
my_dim1<-my_MCA$ind$coord[,1]
my_dim2<-my_MCA$ind$coord[,2]

my_x_matrix<-cbind(my_dim1,my_dim2)




########################################################
##clustering poisonous mushrooms


library(cluster)



##for GMM clustering
library(mixture)

gmm.fit<-gpcm(as.matrix(my_x_matrix),G=10,mnames='VVV')



plot(my_x_matrix,col=gmm.fit$map,pch=gmm.fit$map,ylab='Dim 2',xlab='Dim 1',main='GMM Clustering Result')


my_sill<-silhouette(x=gmm.fit$map,dist=dist(my_x_matrix))
plot(my_sill)


#my_sill<-silhouette(x=my_kmeans$cluster,dist=dist(my_x_matrix))


##does not stabalize to any consistant shape, many different patterns 
##that arise in our resulting graph. Definately not the ideal way to 
##categorize these observations








##for PAM: (k-mediods)
library(cluster)
pam_poison<-pam(x=poison,k=10)


osb<-pam_poison$clustering

##now to plot it along the factor map
plot(my_x_matrix,col=osb,pch=osb,xlab='Dim 1',ylab='Dim 2',main='K-Modes-Result')
##stablizes very nicely to this result!


my_sill<-silhouette(osb,dist=dist(poison))

#my_sill<-silhouette(x=my_kmeans$cluster,dist=dist(my_x_matrix))




##now to summarize the results
summary(pam_poison$medoids)

cluster_label<-pam_poison$clustering

poison_plus<-cbind(poison,cluster_label)

poison_plus$cluster_label<-as.factor(poison_plus$cluster_label)

table(poison_plus$cluster_label)








##looking at the aspects of the different clusters

summary(poison_plus[poison_plus$cluster_label==1,])
##stalk.color.below is always white!!!! viel color always white

summary(poison_plus[poison_plus$cluster_label==2,])
#stalk color above is always white, ring type is evanescent always 
#habitat always grassy. viel color always white

summary(poison_plus[poison_plus$cluster_label==3,])
#viel color always white. stalk color below ring always white
#ring type always pendant
#stalk.surface.above.ring always smoothe
#always bruises

summary(poison_plus[poison_plus$cluster_label==4,])
##ring.type of pendant always
##always no odor

summary(poison_plus[poison_plus$cluster_label==5,])
#talk.surface.above.ring 99% smoothe

summary(poison_plus[poison_plus$cluster_label==6,])
##ring type always pendant
##stalk.surface.above.ring always smoothe


summary(poison_plus[poison_plus$cluster_label==7,])
##stalk.surface.above.ring always smoothe
##gill spacing always close

summary(poison_plus[poison_plus$cluster_label==8,])
##viel color always white
#stalk color below always white
#always no odor

summary(poison_plus[poison_plus$cluster_label==9,])
#always bruises
#always no odor
#gil spacing always close
#gil size is always broad
#gil color is red
#stalk shape is enlarging
#stalk.surface.above.ring is smoothe
#viel color is always white
#habitat is always waste
#spore print color is white
#population is clustered



summary(poison_plus[poison_plus$cluster_label==10,])
##stalk shape is always enlarging