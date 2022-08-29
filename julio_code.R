
library(tree)
mushrooms <- read.csv("/Users/JTPD_/Desktop/School.Military/Spring2022/Math 151/project/mushrooms.csv",stringsAsFactors = TRUE)
View(mushrooms)
tree.mush <- tree(class ~ . , mushrooms) # decision tree with whole data set
plot(tree.mush)
text(tree.mush,pretty = 0)

#subset the dataset
my_mushrooms<-subset(mushrooms,select=-c(veil.type,stalk.surface.below.ring,
                                         gill.attachment,
                                         stalk.color.above.ring))


#training data set at 65 percent of data
sample.number = sample(1:nrow(my_mushrooms), nrow(my_mushrooms)*.65)
train.mush = my_mushrooms[sample.number,] 
test.mush= my_mushrooms[-sample.number,]

tree.mush <- tree(class ~ . , train.mush)#decision tree with 65 percent of data and subset
plot(tree.mush)
text(tree.mush,pretty = 0)

cv.mush <- cv.tree(tree.mush, FUN = prune.tree, K=5)
###
plot(cv.mush$size, cv.mush$dev, type = "b") #will find out how to prune
#
cv.mush # we will use 3 

prune.mush <- prune.tree(tree.mush, best =3 ) #pruned tree
plot(prune.mush)
text(prune.mush, pretty = 0)
tree.pred <- predict(prune.mush, test.mush)

mush = ifelse((tree.pred[,1] > 0.5),'e','p') # function to convert number to p or e
table(mush,test.mush$class) # table to display our algorithm

my_mushrooms.odor<-subset(mushrooms,select=-c(veil.type,stalk.surface.below.ring,
                                         gill.attachment,
                                         stalk.color.above.ring,odor))
#training data set at 65 percent of data
sample.number = sample(1:nrow(my_mushrooms.odor), nrow(my_mushrooms.odor)*.65)
train.musho = my_mushrooms.odor[sample.number,] 
test.musho= my_mushrooms.odor[-sample.number,]

tree.musho <- tree(class ~ . , train.musho)#decision tree with 65 percent of data and subset
plot(tree.musho)
text(tree.musho,pretty = 0)

cv.musho <- cv.tree(tree.musho, FUN = prune.tree, K=5)

plot(cv.musho$size, cv.musho$dev, type = "b") #will find out how to prune
#
cv.musho # we will use 4 
prune.musho <- prune.tree(tree.musho, best =4 )
plot(prune.musho)
text(prune.musho, pretty = 0)
tree.predo <- predict(prune.musho, test.musho)

musho = ifelse((tree.predo[,1] > 0.5),'e','p') # function to convert number to p or e
table(musho,test.musho$class) # table to display our algorithm

