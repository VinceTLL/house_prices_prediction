#price prediction Exploratory data analysis 
library(tidyverse)
library(reshape)
library(ggthemes)
library(corrplot)
library(stats) # for k means
library(cluster)# for PAM and CLARA
library(dplyr)
library(caret)
library(NbClust)
library(lubridate)
library(factoextra)
library(cluster)
library(VIM)# for visualizing missing values 
library(gridExtra)
library(seriation)# for diss plot visualization 
library(MASS) # mca function
library(impute)
library(rlist)
library(FactoMineR)
library(klaR)
library(e1071) # for skweness 
# load the data 

train_df_or<- read.csv("train_house.csv")
train_df<- train_df_or %>% dplyr:: select(-SalePrice)
sale_price<- train_df_or %>% dplyr::select(SalePrice)

test_df<- read.csv("test_house.csv")

lapply(list(train_df,test_df), dim)

#train df dimanisons 1460 rows and 81 columns
#test df dimensions 1459 ros and 80 columns

#column names
names(train_df)

str(train_df)

apply(train_df,2, function(x){length(unique(x))} )

numerical_features<- names(train_df[,grepl('(.)+Area', names(train_df)) | grepl('SF', names(train_df)) | names(train_df) %in% c('LotArea',"LotFrontage","ScreenPorch","X3SsnPorch","MiscVal", "EnclosedPorch")  ])

train_df_categorical<- train_df[,!names(train_df) %in% (numerical_features)]

train_df_numerical<- train_df[,numerical_features]

# Converting categorical features in categorical types

train_df_categorical<- data.frame(apply(train_df_categorical,2, as.factor))
# checking missing data

sort(colSums(is.na(train_df)))/nrow(train_df) # there are several variables with a lot of missing data


# lets impute 

head(train_df_num_V2)
train_df_num_imputed<-impute.knn(as.matrix(train_df_num_V2) )
colSums(is.na(train_df_num_imputed$data))
train_df_num_V2<-train_df_num_imputed$data

# lets investgate 
# according the documnetation houses with no pool  are set to NA
# same for Alley, MiscFeature, Fence, FirePlace, BSMTCond, Bsmtexposure, and  so on lets see if we 
# can identify patterns 

# detour to missing value tutorial in data camp

# FIRST QUESTION TO ASK IS  in which variables observation are missing and how many 

aggr(train_df_categorical, numbers = TRUE, prop = c(TRUE, FALSE))
# it shows the cobination of missing values for the variables and the proportion
# lets coerce  the same information into a single visualization

aggr(train_df_categorical,combined = TRUE, numbers = TRUE)

aggr(biopics,combined = TRUE, numbers = TRUE)

# SPINOGRAM AND SPINEPLOT to have better insight 
# They allow us to study the precentage f missing values in one variable for different other
#variables.

spineMiss(train_df[,c("Fence","Alley")])

spineMiss(train_df[,c("Alley","Fence")])
# If we have a numerical variables we will  use a spinogram, otherwise if categorical we use  spineplot

spineMiss(train_df[,c("WoodDeckSF","Fence")] )

# Mosaic plots
mosaicMiss(train_df[,c('PavedDrive','CentralAir','Alley')],highlight = 3,
           plotvars = 1:2, miss.labels = FALSE)
# ize of the tile corrispond to the occourcence frequencies of a gien combination
# in the data set. For example House with pived drive and central air condition are the most frequent

#Parallel box plot
#The idea behind it is to split the dataset in two subsets, one with only observed values of incomplete variables
# and the other with only its missing values. For both subsets, a boxplot of a chosen numeric variable is produced.
#This allow as to check if the ditribution of the chosen variable is affected by missing  values the slplitting value

pbox(train_df[,c('WoodDeckSF','FireplaceQu')])
#pbox(biopics[, c("log_earnings", "sub_race")])
#The white box on the left-hand side shows the overall distribution of log_earnings, 
#while the blue and the red one show its distribution for the subsets of observed and missing values in sub_race, respectively. 
#The relative width of the boxes reflects the sizes of the subsets on which they are based: the wider blue box means there are more observed than missing values in sub_race.
#Other than that, the two boxes look similar to each other and also the overall white box. 
#This suggests that missing race information has no impact on the distribution of earnings.

pbox(train_df[,c('GarageArea','FireplaceQu')])


# PARALLEL COORDINATE PLOT 
#We have looked at single variables and their interactions. 
#Let us now turn to analyzing all the variables in the dataset at once.

# Lets create a dataset with to be readable

df<- train_df[,c("RoofMatl","OverallQual","FireplaceQu","CentralAir", "LotArea")]
parcoordMiss(df, highlight = 'FireplaceQu') # It can show how the distribution of missing calues for Fireplaces changes dependingt
# the vaiable with what is interacting.
?parcoordMiss
df$logLotArea<- log(df$LotArea)
pbox(df[,c("logLotArea","FireplaceQu")])
cutoff<-quantile(df$LotArea)
df$lotsequence<-cut(df$LotArea, breaks = cutoff)
spineMiss(df[,c("lotsequence","FireplaceQu")])
mosaicMiss(df[,c('lotsequence','CentralAir','FireplaceQu')],highlight = 3,
           plotvars = 1:2, miss.labels = FALSE)
# AS we can see the number of missing values for frieplace changes as the  lot area increase  especially for houses with central condition  air


# MATRIX PLOT 

#It visualizes all cells of the data matrix by rectangles. 
#Observed data are shown in a continuous grey-black color scheme (the darker the color, the higher the value), 
#while missing values are highlighted in red. It's a good practice to sort the data by one of the incomplete variables - it makes the plot easier to interpret.

matrixplot(df, sortby = c('FireplaceQu'))
# We saw that the missing values in our data set are not random, it dependes on other variables
# removing observations with missing values, would most probabily intorduce bias to our inference.
# Since we know that categorical variables with missing values means that houses do not have that specific property,
# we will impute the missing values with a value " not present".

#Lets examine numerical variables
 head(train_df_numerical)
# finding low freqency variables
 
lowFrequency<- nearZeroVar(train_df_numerical)
lowFrequency
train_df_num_V2<- train_df_numerical[,-lowFrequency]
head(train_df_numerical[,lowFrequency])
head(train_df_num_V2)

# lets check if there are missing values in numerical vairables

colSums(is.na(train_df_num_V2))/nrow(train_df_num_V2)
# there are 17% missing alues in the first columns and 0.0054794 missing in the third variable
# lets analyze 

aggr(train_df_num_V2,combined = TRUE, numbers = TRUE)
spineMiss(train_df[,c("LotFrontage","MasVnrArea")])
spineMiss(train_df[,c("MasVnrArea","LotFrontage")])
# the majority of missing values is for MaVnrArea 0.

parcoordMiss(train_df_num_V2, highlight = 'LotFrontage')


train_df_num_V2$logMasVnrArea<- NULL
pbox(train_df_num_V2[,c('OpenPorchSF','LotFrontage')])
matrixplot(train_df_num_V2, sortby = c('LotFrontage'))

# lets impute the variables with missing values




# correlation matrix

cormatrix<- cor(train_df_num_V2)

corrplot(cormatrix,method = 'square',order = 'hclust')

set.seed(245)
index<- sample(1:nrow(train_df_num_V2),100)
dist_matrix<- dist(scale(train_df_num_V2[index,]))
fviz_dist(dist_matrix )
# lets check if our data is clusterable 
res<- get_clust_tendency(scale(train_df_num_V2), graph = FALSE, n = nrow(train_df_num_V2)-1)
#seems that our data is clustarbles 

# do pca to see any meaningful pattern 

pca<- prcomp(train_df_num_V2,center = TRUE, scale. = TRUE)
# etracting the eigenvalues 
get_eigenvalue(pca)
# the first two compnent can explain 47%of the total variance 
# Lets plot 
library(ggbiplot)
ggbiplot(pca, alpha = 0.5) + theme_minimal()
#lets test what type of cluster would be useful

install.packages("clValid")
library(clValid)
clmethod<- c('hierarchical','kmeans','pam')
intern<-clValid(scale(train_df_num_V2), nClust = 2:6, clMethods =clmethod, validation = 'internal')
# acorinng this 
intern_2<-clValid(scale(train_df_num_V2), nClust = 2:6, clMethods =clmethod, validation = 'stability')


# Lets try hairaircal clustering with two clusters 

train_df_num_v2_clust<- dist(scale(train_df_num_V2))

res.hc2<- hclust(train_df_num_v2_clust, method = 'average')
plot(res.hc2)
res.coph2<- cophenetic(res.hc2)
cor(train_df_num_v2_clust,res.coph2)
# correlation is 0.85 better then before 
# lets try another one 
res.hc3<- hclust(train_df_num_v2_clust, method = 'centroid')
plot(res.hc3)
res.coph3<- cophenetic(res.hc3)
cor(train_df_num_v2_clust,res.coph3)
# 0.80 is low

res.hc4<- hclust(train_df_num_v2_clust, method = 'median')
plot(res.hc4)
res.coph4<- cophenetic(res.hc4)
cor(train_df_num_v2_clust,res.coph4)
#0.81

res.hc5<- hclust(train_df_num_v2_clust, method = 'complete')
plot(res.hc5)
res.coph5<- cophenetic(res.hc5)
cor(train_df_num_v2_clust,res.coph5)
#0.68
res.hc6<- hclust(train_df_num_v2_clust, method = 'single')
plot(res.hc6)
res.coph6<- cophenetic(res.hc6)
cor(train_df_num_v2_clust,res.coph6)
#0.82

res.hc7<- hclust(train_df_num_v2_clust, method = 'ward.D2')
plot(res.hc7)
res.coph7<- cophenetic(res.hc7)
cor(train_df_num_v2_clust,res.coph7)
#0.42

# lets cut the tree at two cluster
grp<- cutree(res.hc2, k = 2)
table(grp) # there is only one 1 value in the second group not a good cluster.

# lets use density clusters 
install.packages("dbscan")
library(dbscan)
db<-dbscan(scale(train_df_num_V2), eps = 500, minPts = 13)
fviz_cluster(db,data = scale(train_df_num_V2), stand = FALSE, geom = "point", ggtheme = theme_classic())
# no cluster found 

kNNdistplot(scale(train_df_num_V2),k = 13)

# lets visualize the data to find possible clusters

df_dist<- dist(scale(train_df_num_V2))
dissplot(df_dist)# no plot in this numeriv variables 

head(train_df_categorical)

train_df_categorical_v2<- train_df_categorical


names_null<-names(which(sort(colSums(is.na(train_df_categorical))) >0))

unique(train_df_categorical[,names_null[1]])

for(i in names_null){
  
  level<-levels(train_df_categorical[,i])
  level_2<- list.append(level,'none')
  levels(train_df_categorical[,i])<- level_2
 train_df_categorical[is.na(train_df_categorical[,i]) ,i] <- 'none'
}



sort(colSums(is.na(train_df_categorical)))

# I gave 0 to all nas if they are missing values 

str(train_df_categorical)
# lets remove low frequenciy variables

zerovar<- nearZeroVar(train_df_categorical)
train_df_categorical<-train_df_categorical[,-zerovar]
#removing id 
train_df_categorical<- train_df_categorical[,-1]
train_df_categorical<-as.data.frame( apply(train_df_categorical,2,as.factor))
str(train_df_categorical)
library(FactoMineR) # corrispondance analisi
# corrispondence analysis between year sold and neighborhood


table(train_df_categorical$Neighborhood,train_df_categorical$YrSold)

ggplot(train_df_categorical,aes(x =YrSold)) + geom_bar()+ facet_wrap(.~Neighborhood)

ca_table<- train_df_categorical %>% group_by(Neighborhood,YrSold) %>% summarise(count = n())

ca_table<- cast(data = ca_table,Neighborhood~YrSold, fill = 0 )
ca_table$Neighborhood<- as.numeric(as.character(ca_table$Neighborhood))
?CA
res<- CA(table(train_df_categorical$Neighborhood,train_df_categorical$YrSold))
summary(res)
dimdesc(res)# it shows the rows in terms of increasing coordinates 
# values, and the columns too in terms of increasing coordinates values.

# putting lables on points that are well represented.
plot(res, selectRow = 'cos2 0.7',selectCol = 'cos2 0.7')
#useful if there are too many points on the plain.
# we can also label points as a function of ther contribution to the total inertia of the sth dimension.
plot(res, selectRow = 'contrib 4',selectCol = 'contrib 3')
# to plot row and ols that  alos have high contribution 
# we can also plot specific rows and columns 
plot(res, selectCol = '5')
#we plotted  all rows and the columns 5
# we  can aloso choose to plot other pairs of axis such as 1 and three
plot(res, axes = c(1,3))
# we can do clustering on column or rows 
res.hcpc<- HCPC(res,cluster.CA = 'rows')
?HCPC
# lets check correlation plot



cor<-corrplot(cormatrix,method = 'square',order = 'hclust')
findCorrelation(cor,cutoff = 0.85)

# Lets do another corrispondence analysis test 

res<- CA(table(train_df_categorical$BldgType,train_df_categorical$MSZoning))
summary(res)    
plot(res, axes =c(1,2))
res.hcpc<- HCPC(res,cluster.CA = 'columns')

train_df_num_V2<-as.data.frame(train_df_num_V2)
names(train_df_num_V2)
clsuter_df<- train_df_num_V2 %>%dplyr:: select(LotFrontage,LotArea,MasVnrArea)

cluster_df_ct<-train_df_categorical %>% dplyr:: select(MSZoning,Neighborhood,Condition1,CentralAir)

#calculating the distance using binary distance 
# we create dummy variables 
if(!require('dummies')){
  install.packages("dummies")
  library('dummies')
}
dummy_df<-dummy.data.frame(cluster_df_ct)

dist<- dist(dummy_df, method = 'binary')
dist<-as.matrix(dist)
# lets do an heat map to chec any possible cluster, using seriation function

if(!require(seriation)){
  install.packages("seriation")
  library(seriation)
}

dissplot(dist_v2)

#Lets use pca 

dummy_pca<-princomp(dummy_df)
fviz_eig(dummy_pca)

library(ggbiplot)
ggbiplot(dummy_pca) + theme_minimal()
# the pca shows the formation of three cluster like
#lets use elobow method or siluette 

#elbowh method
fviz_nbclust(as.matrix(dist_v2),pam, method = 'wss') 

#siluette 
fviz_nbclust(dist,pam, method = 'silhouette') 
#siluette
fviz_nbclust(dist,kmeans, method = 'silhouette') 

# removing low frequenciy vars
lofreq<- nearZeroVar(dummy_df)
dummy_df.V2<-dummy_df[,-lofreq]

# lets do some pca

dist_v2<- dist(dummy_df.V2, method = 'binary')
pca_res<-princomp(dummy_df.V2)
fviz_eig(pca_res)
#lets plot it 
ggbiplot(pca_res) + theme_minimal()

#  lets do dome exploration

# measuring the correlation between the proice and the numeric predictors

k_res<-kmeans(as.matrix(dist_v2),7, iter.max = 50, nstart = 100)


table(k_res$cluster)
fviz_cluster(k_res, data = dummy_df.V2)

table(cluster_df_ct$Neighborhood,cluster_df_ct$Condition1)

res<-CA(table(cluster_df_ct$Neighborhood,cluster_df_ct$Condition1))
summary(res)
plot(res,cex = 0.7,selectRow = 'contrib 3',selectCol = 'contrib 3')
plot(res,cex = 0.7)
test_plot<-cluster_df_ct %>% dplyr::group_by(Neighborhood,CentralAir) %>% dplyr::summarise(count = n())

library(RColorBrewer)

getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(test_plot,aes(x = Neighborhood, y = count, fill = CentralAir)) + 
  geom_bar(stat = 'identity', position = 'fill') + 
  coord_flip() + scale_fill_discrete(name = 'Condition Air')  + ylab('') +xlab('') +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = )

# other CA

# lets  do some exploratory data analysis 



table(cluster_df_ct$Neighborhood, cluster_df_ct$MSZoning))

res<- CA(table(cluster_df_ct$Neighborhood, cluster_df_ct$MSZoning)
)
summary(res)
plot(res, cex = 0.6)
plot(res,cex = 0.6,  selectRow = 'contrib 4',selectCol = 'contrib 4')

head(train_df_num_V2)
#lets visualize the correlation 
cors<-cor(train_df_num_V2)
corrplot(cor, order = 'hclust')

# measuring skweness 
apply(train_df_num_V2,2, skewness)

# lets calculte the correlation with the price



multicorr<-apply(train_df_num_V2,2,function(x,y = sale_price$SalePrice){cor(x,y) } )

train_df_num_V2<- as.data.frame(train_df_num_V2)


# plotting skwed data



plots<- lapply(train_df_num_V2, function(x){ggplot(train_df_num_V2, aes(x )) + geom_histogram() } )
grid.arrange(grobs = plots)

names(train_df_num_V2)
sapply(train_df_num_V3,skewness)

# aplying log 

plots<- lapply(train_df_num_V3, function(x){ggplot(train_df_num_V3, aes(log(x) )) + geom_histogram() } )
grid.arrange(grobs = plots)
# byappling the log of x the skeweness if redued 

# simple linear model uisng high correlated 
lm<- lm(sale_price$SalePrice~train_df_num_V2$GrLivArea)
summary(lm)
lm_2<- lm(sale_price$SalePrice~train_df_num_V2$GrLivArea +train_df_num_V2$GarageArea)
summary(lm_2)
shapiro.test
squred_term<- train_df_num_V2$GrLivArea ^2
lm_3<- lm(sale_price$SalePrice~train_df_num_V2$GrLivArea +train_df_num_V2$GarageArea + squred_term )
summary(lm_3)

lm_4<- lm(sale_price$SalePrice~train_df_num_V2$GrLivArea +train_df_num_V2$GarageArea + squred_term + train_df_num_V2$LotArea )
summary(lm_4)
# lets use the log of the values

train_df_num_V3<- data.frame(apply(train_df_num_V2,2, function(x) { ifelse(x!= 0, log(x),x) } ))
train_df_num_V3

lm<- lm(sale_price$SalePrice~train_df_num_V3$GrLivArea)
summary(lm)
lm_3<- lm(sale_price$SalePrice~train_df_num_V3$GrLivArea +train_df_num_V3$GarageArea )

summary(lm_3)
# is bit worse
# lets transform sales price in to the log as well
sales_prices_v2<-log(sale_price$SalePrice)
lm<- lm(sales_prices_v2~train_df_num_V3$GrLivArea)
summary(lm)
lm_3<- lm(sales_prices_v2~train_df_num_V3$GrLivArea +train_df_num_V3$GarageArea )
summary(lm_3)

# creating new features 
names(train_df_num_V2)
sapply(train_df_num_V3,function(x,y = sales_prices_v2){cor(x,y)} )

# lets add two features 
#first featreus is the summ of first and second florea area if available

train_df_num_V2$total_size_house<- train_df_num_V2$X1stFlrSF + train_df_num_V2$X2ndFlrSF
hist(train_df_num_V2$total_size_house)
cor(sale_price$SalePrice,train_df_num_V2$total_size_house)
# there was an increase in the correlation between sales price and the new feature

# The second feature is to craate a categorical variable if the house is one flore aor two floors
train_df_categorical_v2$is_double_floor<- ifelse(train_df_num_V2$X2ndFlrSF == 0,'No','yes')
# lets check if there is some difference between the 

train_df_categorical_v2$is_double_floor<- as.factor(train_df_categorical_v2$is_double_floor)
ggplot(train_df_categorical_v2, aes(y = sale_price$SalePrice, x = is_double_floor)) + geom_boxplot()
# there isn't a searation between the two categoris, for the two floors house the price is slightly higers

t_test_df<- data.frame(category<- train_df_categorical_v2$is_double_floor,value = sale_price$SalePrice )
str(t_test_df)
?inference
