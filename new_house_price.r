#house Prices New start

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
library(car)# used for vif
library(statsr)
library(ggrepel)
library(RColorBrewer)
library(xgboost)
library(fastDummies)
library(rlist)


train_df_or<- read.csv("train_house.csv")
sale_price<- train_df_or$SalePrice
train_df<- train_df_or %>% dplyr::select(-SalePrice)

test_df<- read.csv("test_house.csv")

lapply(list(train_df,test_df), dim)

#------------------------------------------------------
# FIXING TEST DATA FRAME 

#-----------------------------------------------------
colSums(is.na(test_df_numerical))/nrow(test_df)

numerical_features_test<- names(test_df[,grepl('(.)+Area', names(test_df)) | grepl('SF', names(test_df)) | names(test_df) %in% c('LotArea',"LotFrontage","ScreenPorch","X3SsnPorch","MiscVal", "EnclosedPorch")  ])

test_df_categorical<- test_df[,!names(test_df) %in% (numerical_features_test)]

test_df_numerical<- test_df[,numerical_features_test]

test_df_categorical<- data.frame(apply(test_df_categorical,2, as.factor))


test_Id<- test_df_categorical$Id

test_df_categorical$Id<- NULL

test_df_num_imputed<-impute.knn(as.matrix(test_df_numerical) )
colSums(is.na(test_df_num_imputed$data))
test_df_numerical<-test_df_num_imputed$data
test_df_numerical<-as.data.frame(test_df_numerical)

colSums(is.na(test_df_categorical))

vars<-colSums(is.na(test_df_categorical))

vars_null<- names(which(vars>0))



for ( i in vars_null){
  level<- levels(test_df_categorical[,i])
  level<- list.append('None',level)
  levels(test_df_categorical[,i])<- level
  test_df_categorical[is.na(test_df_categorical[,i]),i]<- 'None'
}

sort(colSums(is.na(test_df_categorical)))


# test total area

test_df_numerical$Total_Area<- test_df_numerical$X1stFlrSF + test_df_numerical$X2ndFlrSF + test_df_numerical$TotalBsmtSF + test_df_numerical$GarageArea + test_df_numerical$MasVnrArea + test_df_numerical$WoodDeckSF


test_data_v2<- dummy_cols(test_df_categorical)

names<- sapply(test_data_v2,class)
names<-names(which(names == 'integer'))
test_data_v2<-test_data_v2[,names(test_data_v2) %in% names(train_data_v2) ]


str(test_data_v2)

test_data_v2$Total_area<- test_df_numerical$Total_Area



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


train_df_num_imputed<-impute.knn(as.matrix(train_df_numerical) )
colSums(is.na(train_df_num_imputed$data))
train_df_numerical<-train_df_num_imputed$data

colSums(is.na(train_df_numerical))

# taking care of missing values in categorical variables 

vars<-colSums(is.na(train_df_categorical))

vars_null<- names(which(vars>0))



for ( i in vars_null){
  level<- levels(train_df_categorical[,i])
  level<- list.append('None',level)
  levels(train_df_categorical[,i])<- level
  train_df_categorical[is.na(train_df_categorical[,i]),i]<- 'None'
}

sort(colSums(is.na(train_df_categorical)))

#Numeric group

#removing collinearity

corr<- cor(train_df_numerical)
corrplot(corr, order = 'hclust', method = 'square')
findCorrelation(corr)
# no strong correlation found
lowfeq<-nearZeroVar(train_df_numerical)
table(train_df_numerical[,5])
train_df_numerical<-train_df_numerical[,-lowfeq]
train_df_numerical<-data.frame(train_df_numerical)
# cleaning up categorical variables
lowfreq<- nearZeroVar(train_df_categorical)
lapply(train_df_categorical[,lowfreq],function(x) { table(x)/nrow(train_df_categorical)} )

train_df_categorical<-train_df_categorical[,-lowfreq]

#-------------------------------------------------------------------------------------------
#
#EXPLORATORY DATA ANALYSIS 
#----------------------------------------------

ncol(train_df_numerical)
ncol(train_df_categorical)
# checking  correlation between traget and numeric predictors

sapply(train_df_numerical, function(x,y = sale_price){cor(y,x)})
class(train_df_numerical)


#creating two new features 

train_df_numerical$Total_house_area <- train_df_numerical$X1stFlrSF + train_df_numerical$X2ndFlrSF + train_df_numerical$TotalBsmtSF
cor(train_df_numerical$GrLivArea,train_df_numerical$Total_house_area)# the new feature is highly correlated with the GrLivArea

cor(sale_price,train_df_numerical$Total_house_area)
# the correlation with sale prices is much higher for the new created feature.

ggplot(train_df_numerical, aes(x = Total_Area, y = sale_price)) + geom_point()
#there are two houses that complitely deviated from the  rest of the data making the influential

# creating another variables house_area_w_garage

train_df_numerical$house_w_garage<- train_df_numerical$Total_house_area + train_df_numerical$GarageArea
cor(sale_price,train_df_numerical$house_w_garage)
# it has a correlatiion of 0.80

cor(train_df_numerical$Total_house_area,train_df_numerical$house_w_garage)
# they are almost identical, 0.98 correlation 

# testvar two yelds even better result 

train_df_numerical$Total_Area<- train_df_numerical$house_w_garage + train_df_numerical$MasVnrArea + train_df_numerical$WoodDeckSF

cor(sale_price,train_df_numerical)^2

# creating  a new variable, it is a ratio between the proportion of house area over all the total area of the property

var_test_1<- with(data = train_df_numerical, Total_house_area/Total_Area)

cor(sale_price,var_test_1)

# the correltaion is not very strong  -0.28

train_df_numerical[which(train_df_numerical$Total_Area >8800 & sale_price < 200000),]
# these are the outlier houses 524 and 1299
train_df_categorical$Id<-as.numeric(as.character(train_df_categorical$Id))
train_df_categorical[train_df_categorical$Id %in% c(524,1299),]

test(524,'MSZoning','Neighborhood','MSSubClass','MSZoning','LotShape','LotConfig','Neighborhood','Condition1','BldgType'  )

sale_price[1]
sale_price[524]
# I will deciede two remove these two houses 

train_df_categorical_v2<- train_df_categorical[!train_df_categorical$Id %in% c(524,1299),]
train_df_numerical_v2 <- train_df_numerical[-c(524,1299),]
sale_price_v2<-sale_price[-c(524,1299)]

lapply(list(train_df_categorical_v2,train_df_numerical_v2), nrow)

cor(sale_price_v2,train_df_numerical_v2)^2

# the correlation with sales prices has increased greatly 


# these two houses are identical in terms of categorical features, lets find all the houses that 
# have same feature, to check if there are many of them 



# lets create some categorical variable using numeric variables

table(train_df_numerical$GarageArea)
train_df_categorical$house_with_Garage<- ifelse(train_df_numerical$GarageArea > 0, 'YES','NO')
ggplot(train_df_categorical, aes(x = house_with_Garage, y = sale_price)) + geom_boxplot()

test_stat<- data.frame(y = sale_price, x = train_df_categorical$house_with_Garage)
inference(data = test_stat, y = test_stat$y, x = as.factor(test_stat$x), type = 'ht', statistic = 'median', method = 'simulation', alternative = 'less' )
# very low p value 

# lets focus on categorical variables


str(train_df_categorical)
# lets check one by one category, starting with  MSSubClass
# it represent the type of dwelling, i.e (Story, Duplex, PUD, family Conversion ecc)
levels(train_df_categorical$MSSubClass)

plot_1<-train_df_categorical %>% group_by(MSSubClass) %>% summarise(count =  n()) %>% arrange(desc(count)) %>%
  ggplot(aes(x = reorder(MSSubClass, count), y = count, fill = MSSubClass)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  xlab(' type of dwelling') +
  ylab('number of dwelling') + 
  geom_label(aes(label = paste(round(count/sum(count) * 100,1),'%') )) +
   theme_economist_white() +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0,1000,by = 100))
# we can see that the story 1946 & newer all styles is the most common typem of dwelling 
# in the state fllowed by 2 story 1946 and & newer 

# lets check the average price of these type of dewelling 
dwelling<- data.frame('MSSubClass' = train_df_categorical$MSSubClass, 'sales_prices' = sale_price)

plot_2<-dwelling %>% group_by(MSSubClass) %>% summarise(avg_price = mean(sales_prices)) %>%
  ggplot(aes(x =reorder(as.factor(MSSubClass),avg_price), y = avg_price, fill = MSSubClass)) + geom_bar(stat = 'identity') +
  geom_label(aes(label = floor(avg_price) )) + coord_flip() +
  theme_economist_white() +
  theme(legend.position = 'none') +
  xlab('type of dwelling') +
  ylab('average price') 
# Lets compare both graphs

grid.arrange(plot_1,plot_2,ncol = 2,nrow = 1)
# we can see that some of the dwellings are both low in number and in  average price,
# such as MULTILEVEL - INCL SPLIT LEV/FOYER, which has a low number of building and the average price is 
# the lowest but last. Seems that this type of dwelling was not  very popular.
#Other types of dwellings such as 1-STORY W/FINISHED ATTIC ALL AGES  and 2-1/2 STORY ALL AGES,
# are  the least frequent in the market but have high demand pusching their average price  on top 6.
#SOme other  dwelling such as 1-1/2 STORY FINISHED ALL AGES and 1-STORY 1945 & OLDER are 
#quite frequent in the market and the average price are one of the lowest on the market.

# lets calculate the analusis of variance to see if there is an sgninficance difference in the  variance 
# of pricess bteween dwelling types

dwelling %>% ggplot(aes(x = MSSubClass, y = log(sales_prices) )) +geom_boxplot()

aov_test<-aov(data = dwelling, sales_prices~MSSubClass)
summary(aov_test)
# the p value if lower than 5% significance level, meaning that 

#what was the most frequent type of dweeling built for each year.
colorCount = length(unique(dwelling$MSSubClass))
getPalette = colorRampPalette(brewer.pal(9,'Set1'))
dwelling$yearblt<- train_df_categorical$YearBuilt
dwelling$yearblt<- as.ordered(dwelling$yearblt)


plot_2<-dwelling %>% group_by(yearblt, MSSubClass) %>%  summarise(total =n()) %>% 
  ggplot(aes(x = yearblt, y = total, fill = MSSubClass)) + geom_bar(stat = 'identity', position = 'fill') +
  coord_flip() + scale_fill_manual(values = getPalette(colorCount)) +
  ylab('') + theme(legend.position = 'none')  

plot_1<-dwelling %>% group_by(yearblt, MSSubClass) %>%  summarise(total =n()) %>% 
  ggplot(aes(x = yearblt, y = total, fill = MSSubClass)) + geom_bar(stat = 'identity') +
  coord_flip() + scale_fill_manual(values = getPalette(colorCount)) +
  ylab('') 
  
grid.arrange(plot_1,plot_2,ncol = 2,nrow = 1)

#The 1-STORY 1946 & NEWER ALL STYLES has become popular strating by  the end of the 
# 4s and has become very popular ever since until the 90s 
# in that decade 2-STORY 1946 & NEWER was the most popular dwelling type 
# in The mid  early 2000s 2-STORY 1946 & NEWER became again the most popular 
# type of dwelling built in the state. We can se the number of houess bult, boomed during the pre- financial crisis
# then there was a sharp decline.

# MSZONING 
# it is about the zonining classification of the sale

plot_1<- train_df_categorical %>% group_by(MSZoning) %>% summarise(total = n()) %>%
  ggplot(aes(x =reorder(MSZoning, total), y = total, fill = MSZoning)) + geom_bar(stat = 'identity') +
           geom_label(aes(label = total)) + coord_flip() + 
  theme_foundation() + theme(legend.position = 'none')+
  xlab('MSZoning')
#most of the houses were built in the Residential Low Density area 
zoning<- data.frame('MSZoning' = train_df_categorical$MSZoning, 'sale_price' = sale_price)

plot_2 <- zoning %>% group_by(MSZoning) %>% summarise(median_sales  = median(sale_price)) %>%
  ggplot(aes(x = reorder(MSZoning,median_sales), y = median_sales, fill = MSZoning)) + 
  geom_bar(stat = 'identity') + geom_label(aes(label = floor(median_sales))) + theme_foundation() +coord_flip() +
  xlab('')
grid.arrange(plot_1,plot_2, ncol = 2,nrow=1) 
# We can see that Floating Village Residential area has a very small number of houses, but the median 
#house price is the highest, possible exclisuve area where welthier residents live.
#  the Residential Medium Density is the second are with most houses bult but has the lowest but last median house price.
# These can bee due to middle lower class leaving there, affordable houses.

# analysis of variance  for zoning area
zoning %>% ggplot(aes(x =MSZoning, y = sale_price)) + geom_boxplot()
mszoning_variance<- aov(data = zoning, sale_price~MSZoning)
summary(mszoning_variance)
# there is a significance difference between the varaine of the zoning areas.

# I will build an exploratory model as a base line 

#----------------------------------------------
 
# BASE LINE MODEL 

#----------------------------------------

train_df_categorical$Total_area<- train_df_numerical$Total_Area

train_data<- train_df_categorical
train_data$sale_price<- sale_price


# I will use the xgboost package as it should be faster

train_df_categorical$Total_area<- train_df_numerical$Total_Area

train_data<- train_df_categorical
train_data$sale_price<- sale_price

# transforming the response variable in log to reduce skeweness
train_data$sale_price<- log(train_data$sale_price)
train_data$house_with_Garage<- as.factor(train_data$house_with_Garage)

#transforming total area  in log as it is right skewed as well

train_data$Total_area<- log(train_data$Total_area)

#  one hot encoding 

train_data_v2<- dummy_cols(train_data[,-c(49,50)])

names<- sapply(train_data_v2,class)
names<-names(which(names == 'integer'))
train_data_v2<-train_data_v2[,names]
str(train_data_v2)
# removing low frequency variables and collinearity
lowfreq<- nearZeroVar(train_data_v2)
train_data_v2<-train_data_v2[,-lowfreq]
cor<-cor(train_data_v2)
cor_cols<-findCorrelation(cor)
train_data_v2<-train_data_v2[,-cor_cols]
multicollinearity<- findLinearCombos(train_data_v2)
train_data_v2<-train_data_v2[,-multicollinearity$remove]
train_data_v2<-train_data_v2[,names(test_data_v2)]

#adding total area
train_data_v2$Total_area<- train_data$Total_area
log_sale_price<- train_data$sale_price

#creating partitions 

set.seed(34577)
index<- createDataPartition(log_sale_price,p =0.80, list = FALSE )

train<- train_data_v2[index,]
log_sale_price_train<- log_sale_price[index]
test<- train_data_v2[-index,]
log_sale_price_test<-log_sale_price[-index]
# creating DM matrices for xgboost algo 

dtrain<- xgb.DMatrix(data = as.matrix(train), label = log_sale_price_train)
  
  
  default_param<- list(
    objective = 'reg:linear',
    booster = 'gbtree',
    eta = 0.01,
    gamma =  0,
    max_depth = 20,
    min_child_weight = 4,
    colsample_bytree = 0.4,
    subsample = 1)
  
  set.seed(5478)
  xgbcv_v4<-xgb.cv(params = default_param,data = dtrain, nrounds = 1000, nfold = 10,stratified = TRUE,
                   print_every_n = 40,early_stopping_rounds = 30, maximize = FALSE)
  
  xgb_mode<- xgb.train(data =dtrain,params = default_param, nrounds = 1000)
  #performance returned 0.1480686 

  
dtest<-xgb.DMatrix(data = as.matrix(test))
pred_XGB<- predict(xgb_mode,dtest)  
postResample(pred = pred_XGB,obs = log_sale_price_test)
# the RMS is 0.1242920
final_test<-xgb.DMatrix(data = as.matrix(test_data_v2))
prediction_test<- predict(xgb_mode,final_test)
prediction_df<-cbind(test_Id,prediction_test)
prediction_df<-data.frame('Id' = test_Id, 'SalePrice' = exp(prediction_test))

write.csv(prediction_df,'pred12_26_2018.csv',row.names = FALSE)
# performane was 0.56529


# ----------------------------------

# More exploratory data analysis 

#-----------------------------------

ncol(train_df_categorical)
ncol(test_df_categorical)
apply(train_data_v2,2, sum)

lowfreq<-nearZeroVar(test_data_v2)
test_data_v3<- test_data_v2[,-lowfreq]

test_cor<-cor(test_data_v3)
linear_comb<-findLinearCombos(test_cor)
test_data_v3<-test_data_v3[,-linear_comb$remove]
train_data_v3<-train_data_v2[, names(train_data_v2) %in% names(test_data_v3)]
ncol(train_data_v3)
test_data_v3$Total_area<- log(test_data_v3$Total_area)

#feeding 
dtrain_v2<- xgb.DMatrix(data = as.matrix(train_data_v3), label = log_sale_price)

default_param<- list(
  objective = 'reg:linear',
  booster = 'gbtree',
  eta = 0.01,
  gamma =  0,
  max_depth = 20,
  min_child_weight = 4,
  colsample_bytree = 0.5,
  subsample = 1)

set.seed(54777)
xgbcv_v5<-xgb.cv(params = default_param,data = dtrain_v2, nrounds = 2000, nfold = 10,stratified = TRUE,
                 print_every_n = 40,early_stopping_rounds = 30, maximize = FALSE)


xgb_mode_v2<- xgb.train(data =dtrain_v2,params = default_param, nrounds = 1031)

dtest_v2<-xgb.DMatrix(data = as.matrix(test_data_v3))
prediction_test_v2<- predict(xgb_mode_v2,dtest_v2)
prediction_df_v2<-data.frame('Id' = test_Id, 'SalePrice' = exp(prediction_test_v2))
write.csv(prediction_df_v2,'pred12_29_2018.csv',row.names = FALSE)
#performane was 0.55366

#--------------------------------------

# Redoing  datapreprocessing 

#-------------------------------------

hptrain_df<- read.csv("train_house.csv", stringsAsFactors = FALSE )
hptest_df<- read.csv("test_house.csv", stringsAsFactors = FALSE)

#-------------------
# looking at the data 
#--------------------

str(hptrain_df)

#------------
#splitting the train data in two categorical and  numerical  datasets 
#--------------

numerical_train<- hptrain_df[, names(which(sapply(hptrain_df, is.integer)== TRUE)) ]
categorical_train<- hptrain_df[, names(which(sapply(hptrain_df, is.character) == TRUE))]
str(numerical_train)
str(categorical_train)

#-------------------------------------------------
#Examining numerical dataset
#------------------------------------------------

# correlation with target variable 

cor_numVar_test <- cor(numerical_train)
corrplot(cor_numVar_test)

# sorting the matrix based on SalePrice

# --------------------
# my test

cor_numVar_test <- cor(numerical_train)

sorted<- sort(cor_numVar_test[,'SalePrice'], decreasing = TRUE)

cor_v2<- cor_numVar_test[names(sorted),names(sorted)]

corrplot(cor_v2,method = 'number')
#-------------------------------
cor_numVar_v2<-as.matrix(sort(cor_numVar[,'SalePrice'],decreasing = TRUE))

# taking the strongest correlated variables names with saleprice
CorHigh <- names(which(apply(cor_numVar_v2, 1, function(x) abs(x)>0.2)))
# Extracting them from the original matrix 
cor_numVar <- cor_numVar[CorHigh, CorHigh]
#plotting the matrix.
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

# Overall quality  GrLivArea have the strongest correlation with salPrice.

#-----------------------------------------------

# taking care of NUll values for numerical data

#--------------------------------------------------

colSums(is.na(numerical_train))[ which(colSums(is.na(numerical_train)) > 0)]
# LotFrontage MasVnrArea and GarageYrBlt 
# 17% of the data is missing for LotFrontage. It also has a low correlation with SalePrice, I will not use it in the modelling.


colSums(is.na(categorical_train))[which(grepl('MasV',names(categorical_train) ) & colSums(is.na(categorical_train)) > 0 )]

# I will impute MaVnarea by adding 0, since the number ov Na in MasVnrarea mach with the nas in MasVnrType, and that means no masVnrTYpe

numerical_train[which(is.na(numerical_train$MasVnrArea)),'MasVnrArea'] <- 0

# taking care of  lot frontage 

hptrain_df[which(is.na(hptrain_df$LotFrontage)),] %>% dplyr:: group_by(Neighborhood) %>%dplyr:: summarise(count = n()) %>% dplyr:: mutate(frequ = count/ sum(count))
# I will impute the values based on the neighbourhd median value 

for(i in unique(hptrain_df$Neighborhood)){
  value = median(hptrain_df[ which(hptrain_df$Neighborhood == i),c("LotFrontage")],na.rm = TRUE)
  numerical_train[which(categorical_train$Neighborhood == i & is.na(numerical_train$LotFrontage)),"LotFrontage"] = value
}

# taking care of GarageYrBlt.
numerical_train[which(is.na(numerical_train$GarageYrBlt)),"GarageArea"]
#  the missing values in garage year bult is due to the fact that no garage was built wth the house, I will add 0 to it 
numerical_train[which(is.na(numerical_train$GarageYrBlt)),"GarageYrBlt"]<-0


#---------------------------------------------------------

# building total area feature 

#--------------------------------------------------------


total_area_v2<- numerical_train$TotalBsmtSF + numerical_train$GrLivArea + 
  numerical_train$GarageArea +  numerical_train$MasVnrArea + 
  numerical_train$WoodDeckSF + numerical_train$OpenPorchSF + 
  numerical_train$Fireplaces + numerical_train$TotRmsAbvGrd + numerical_train$BsmtFullBath 
cor(hptrain_df$SalePrice,total_area_v2)

numerical_train$Total_area <- total_area_v2


sort(apply(numerical_train,2, function(x, y = numerical_train$Total_area) {cor(x,y)}))

used_vars<- c('TotalBsmtSF','GrLivArea','GarageArea','MasVnrArea','WoodDeckSF','OpenPorchSF','Fireplaces','TotRmsAbvGrd','BsmtFullBath')
numerical_train_v2<- numerical_train[,!names(numerical_train) %in% used_vars]
cor_v2<- cor(numerical_train_v2)
class(cor_v2)
sorted_names<- names( sort(cor_v2[,'SalePrice'],decreasing = TRUE))

cor_v2<- cor_v2[sorted_names,sorted_names]
corrplot.mixed(cor_v2, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

# checking some of the numeric variables to possbly change them into categorical variables 

table(numerical_train_v2$YrSold)
# there are 4 unique values we can change it into categorical 

numerical_train_v2$YrSold<- as.factor(numerical_train_v2$YrSold)
plot_1<-numerical_train_v2 %>% dplyr:: group_by(YrSold) %>% dplyr:: summarise(count =  n()) %>% 
  ggplot(aes(x = reorder(YrSold,count), y = count, fill = YrSold )) + 
  geom_bar(stat = 'identity') + 
  geom_label((aes(label = count)))


plot_2 <- numerical_train_v2 %>% dplyr:: group_by(YrSold) %>% dplyr:: summarise(SalePrice_median = median(SalePrice) ) %>% 
  ggplot(aes(x = reorder(YrSold,SalePrice_median), y = SalePrice_median, fill = YrSold )) + 
  geom_bar(stat = 'identity') + 
  geom_label((aes(label = SalePrice_median))) + theme(legend.position = 'none')
grid.arrange(plot_1,plot_2)
# the sales price between the years are very close, lets do an ANOVA to see if we can find any significance difference 

year_sold<- data.frame("Yrsold" = numerical_train_v2$YrSold, 'SalePrice' = numerical_train_v2$SalePrice)
av<-aov(data = year_sold,SalePrice~.)
summary(av)
# there is no significance difference between the variance of the  year in which the houeses were sold.

# checking Year built, there are 112 different unique values, since they  are too many of them to makeit into categorical var

numerical_train_v2$YearBuilt<- as.ordered(numerical_train_v2$YearBuilt)

numerical_train_v2 %>% dplyr::select(SalePrice,YearBuilt) %>% ggplot(aes(y = SalePrice, x = YearBuilt)) + geom_bar(stat = 'identity') + coord_flip()

numerical_train_v2 %>% dplyr::select(SalePrice, YearBuilt) %>% ggplot(aes(y = SalePrice, x = YearBuilt)) + geom_point()

# crating three backets
numerical_train_v2$YearBuilt<- as.integer(as.character(numerical_train_v2$YearBuilt))
numerical_train_v2<- numerical_train_v2 %>% mutate(yearbuilt_bucket = ifelse(YearBuilt >= 1872 & YearBuilt <= 1949,'first_20th_cent', ifelse(YearBuilt >=1950 & YearBuilt<=1979, 'mid_20th_cent',ifelse(YearBuilt>=1980 & YearBuilt <=2000,'late_20ths','21frst' ))))
#lets check  the new variable 


numerical_train_v2 %>% ggplot(aes(x = yearbuilt_bucket, y  = SalePrice))  + geom_bar(stat = 'summary', fun.y ='median')
bucketva<- data.frame('bucket_year' = numerical_train_v2$yearbuilt_bucket, 'Saleprice' = numerical_train_v2$SalePrice)
an<- aov(data = bucketva, Saleprice~.)
summary(an)
# there is a significance differnece in the variance between the three groups.
numerical_train_v2$YearBuilt <- hptrain_df$YearBuilt

numerical_train_v2$YearBuilt<- as.ordered(numerical_train_v2$YearBuilt)
numerical_train_v2 %>% 
  ggplot(aes(x= YearBuilt, y = SalePrice, group = 1)) + geom_line(stat = 'summary', fun.y = 'median') + geom_line(aes(x = YearBuilt, y = lagger, group = 1))

# lets calculate a moving average

year_var<- numerical_train_v2 %>% dplyr:: group_by(YearBuilt) %>% dplyr:: summarise(median_year = median(SalePrice)) %>% 
  dplyr:: mutate(lagger = lag(median_year, n = 20))

year_var$lagger<- as.numeric(as.character( (year_var$lagger)))

year_var[which(is.na(year_var$lagger)),'lagger'] <-0

year_var %>% 
ggplot(aes(x= YearBuilt, y = median_year, group = 1)) + geom_line() + geom_line(aes(x = YearBuilt, y = lagger, group = 1))

# there is much of a change in the msoothing 
numerical_train_v2$MoSold<- hptrain_df$MoSold
numerical_train_v2$MoSold<- as.ordered(numerical_train_v2$MoSold)
numerical_train_v2 %>% ggplot(aes(x = MoSold, y = SalePrice, group = 1)) + geom_line(stat = 'summary', fun.y = 'median') + facet_wrap(.~YrSold)
# noting interesting 
# Garcars

numerical_train_v2$GarageCars<- as.factor(numerical_train_v2$GarageCars)
numerical_train_v2 %>% ggplot(aes(x = GarageCars, y = SalePrice)) + geom_bar(stat = 'summary', fun.y = 'median')

# I will join 3 with 4 and make it an orderd variable 

levels(numerical_train_v2$GarageCars)[5]<-'3'
numerical_train_v2$GarageCars<- as.ordered(numerical_train_v2$GarageCars)

# Checking full bath
table(numerical_train_v2$FullBath)
#there are only 9 houses with 0 bath quite odd I will add them to 1 probably


numerical_train_v2 %>%dplyr::  group_by(FullBath) %>% dplyr:: summarise(median_price = median(SalePrice)) %>% 
  ggplot(aes(x = reorder(FullBath, median_price), y = median_price)) + geom_bar(stat = 'identity')

numerical_train_v2$FullBath<- as.ordered(numerical_train_v2$FullBath)
levels(numerical_train_v2$FullBath)[1]<-1

sort(apply(numerical_train_v2,2,function(x){ length(unique(x)) }),decreasing = FALSE)
#lets check Kitchen ABVGr

table(numerical_train_v2$KitchenAbvGr)/nrow(numerical_train_v2)
# low frequency

#lets remove low frequencity variables

lowfrequ =  nearZeroVar(numerical_train_v2)

names(numerical_train_v2[,lowfrequ])

apply(numerical_train_v2[,lowfrequ],2, function(x){ table(x)/ nrow(numerical_train_v2)})

numerical_train_v2<- numerical_train_v2[,-lowfrequ]

corrval<- cor(numerical_train_v2[,names(which(sapply(numerical_train_v2, function(x){ is.numeric(x)} ) == TRUE))])
findCorrelation(corrval)
# no linear correlation found
# Checking Overall qual

numerical_train_v2 %>%dplyr::  group_by(OverallCond) %>% dplyr:: summarise(median_price = median(SalePrice)) %>% 
  ggplot(aes(x = reorder(OverallCond, median_price), y = median_price)) + geom_bar(stat = 'identity')
table(numerical_train_v2$OverallCond)

numerical_train_v2$OverallCond<- as.factor(numerical_train_v2$OverallCond)

numerical_train_v2[which(numerical_train_v2$OverallCond %in% c('1','2','3','4','5') ),'OverallCond']<- '1'
numerical_train_v2[which(numerical_train_v2$OverallCond %in% c('6','7','8','9') ),'OverallCond']<- '2'
numerical_train_v2$OverallCond<- factor(numerical_train_v2$OverallCond)
numerical_train_v2$OverallCond<-as.factor(numerical_train_v2$OverallCond)

# lets see if there is any signifincance difference
cond<- data.frame('condition' = numerical_train_v2$OverallCond, 'SalePrice' = numerical_train_v2$SalePrice)
inference(data = cond,x = condition, y = SalePrice, statistic = 'mean',method = 'theoretical',null = 0, alternative = 'twosided',type = 'ht')

# there is a significance difference 

#---------------------------------------------
# removing outliers 
#---------------------------------


numerical_train_v2 <- numerical_train_v2[-c(524,1299),]

# lets check MSSubClass

numerical_train_v2 %>%dplyr::  group_by(MSSubClass) %>% dplyr:: summarise(median_price = median(SalePrice)) %>% 
  ggplot(aes(x = reorder(MSSubClass, median_price), y = median_price)) + geom_bar(stat = 'identity')

# I will change it into categorical variable
 numerical_train_v2$MSSubClass<-as.factor(numerical_train_v2$MSSubClass)
 
#------------------------------
 
 names(numerical_train_v2)
 
 year_built<- as.numeric(as.character(numerical_train_v2$YearBuilt))
 year_sold<-  as.numeric(as.character(numerical_train_v2$YrSold))
year_diff<- year_sold - year_built
# it has a fairly strong negative correlation  with sale price
numerical_train_v2$year_dff<- year_diff
# lets check the correlation with total_area
# its below 50

corrs<-cor(numerical_train_v2[,names(which(sapply(numerical_train_v2, function(x){ is.numeric(x)} ) == TRUE))])

sorted_cor<- names(sort(corrs[,'Total_area'], decreasing = TRUE))


corrplot.mixed(corrs[sorted_cor,sorted_cor], tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


# lets do another one
total_rooms_area<- (numerical_train_v2$X1stFlrSF + numerical_train_v2$X2ndFlrSF+ numerical_train_v2$BsmtFinSF1 +numerical_train_v2$BsmtUnfSF + numerical_train_v2$BsmtHalfBath + numerical_train_v2$HalfBath + numerical_train_v2$BedroomAbvGr)
cor(total_rooms_area  ,numerical_train_v2$SalePrice)


# lets check year remodAdd
# we can crate a categorical variable to check if the house was remodedd or not 

numerical_train_v2$is_remodded<- ifelse(numerical_train_v2$YearBuilt == numerical_train_v2$YearRemodAdd,"N",'Y')
numerical_train_v2$is_remodded<- as.factor(numerical_train_v2$is_remodded)
numerical_train_v2 %>% dplyr:: group_by(is_remodded) %>% dplyr::summarise(count = n(), median_sales = median(SalePrice)) %>%
  ggplot(aes(x = is_remodded, y = count, group = 1)) + geom_line() + geom_line(aes(x = is_remodded, y = median_sales, group = 1))

#final numeriacal variables

numerical_train_v2$total_rooms_area <- total_rooms_area
str(numerical_train_v2)
numerical_train_v2<- numerical_train_v2[,c("MSSubClass","is_remodded","total_rooms_area","Total_area","FullBath","yearbuilt_bucket","year_dff")]
#-----------------------

#CATEGORICAL VARIABLES

#-----------------------

str(categorical_train)

sort(colSums(is.na(categorical_train))[which(colSums(is.na(categorical_train)) >0)]/nrow(categorical_train), decreasing = TRUE)

#----------------------

# Electrical  variable

#----------------------
categorical_train$Electrical<- hptrain_df$Electrical

table(categorical_train$Electrical, useNA = 'ifany')



categorical_train$Electrical<- as.factor(categorical_train$Electrical)


categorical_train$Electrical[which(is.na(categorical_train$Electrical))]<-'SBrkr'

categorical_train %>% ggplot(aes (x = Electrical, y = hptrain_df$SalePrice)) + geom_bar(stat = 'summary', fun.y = 'median')

levels(categorical_train$Electrical)<- c(levels(categorical_train$Electrical),'FuseA or Other')
categorical_train$Electrical[which(categorical_train$Electrical %in% c('FuseA','FuseF','FuseP','Mix'))]<- 'FuseA or Other'

categorical_train$Electrical<- factor(categorical_train$Electrical)
categorical_train$Electrical<-as.factor(categorical_train$Electrical)

#----------------------------------------------------------------

# MasVnrType

#--------------------------------------------------------------

table(categorical_train$MasVnrType, useNA = 'ifany')


categorical_train$MasVnrType<- as.factor(categorical_train$MasVnrType)

categorical_train$MasVnrType[which(is.na(categorical_train$MasVnrType))] <- 'None'
  
categorical_train %>% ggplot(aes(x = MasVnrType, y = hptrain_df$SalePrice)) + geom_bar(stat = 'summary',fun.y = 'median')

categorical_train$MasVnrType[which(categorical_train$MasVnrType == 'BrkCmn')]<- 'None'


categorical_train$MasVnrType<- factor(categorical_train$MasVnrType)


#---------------------------------------------------------

# BsmtFinType1

#---------------------------------------------------------


table(categorical_train$BsmtFinType1, useNA = 'ifany')
table(categorical_train$BsmtFinType2, useNA = 'ifany')/nrow(categorical_train)


categorical_train$BsmtFinType1<- as.factor(categorical_train$BsmtFinType1)

categorical_train$BsmtFinType2<- as.factor(categorical_train$BsmtFinType2)

# will focus on the first type as the data is  balanced among the groups.

levels(categorical_train$BsmtFinType1)[7]<- 'No basement'

categorical_train[which(is.na(categorical_train$BsmtFinType1)),'BsmtFinType1']<-'No basement'

categorical_train %>% ggplot(aes(x= BsmtFinType1,y = hptrain_df$SalePrice)) + geom_bar(stat='summary',fun.y = 'median')


colname<-names(categorical_train)

vars_null<- names(which(colSums(is.na(categorical_train))>0))



for ( i in vars_null){
  level<- levels(categorical_train[,i])
  level<- list.append('None',level)
  levels(categorical_train[,i])<- level
  categorical_train[is.na(categorical_train[,i]),i]<- 'None'
}

#------------------------------------------------

# binarization of categorical variabls
#---------------------------------

# removing outliers

categorical_train<- categorical_train[-c(524,1299),]


str(numerical_train_v2)

categorical_train['MSSubClass']<- numerical_train_v2$MSSubClass
categorical_train['is_remodded']<- numerical_train_v2$is_remodded


numerical_train_v2<- numerical_train_v2 %>% dplyr:: select(-MSSubClass,-is_remodded)

library(fastDummies)
dummy_vars<- dummy_columns(categorical_train)

# I will fit a random forest in order to visualize which variable is the most  important

train_data_house_price<- data.frame(cbind(numerical_train_v2,categorical_train))
str(train_data_house_price)

library(randomForest)
SalePrice<- hptrain_df$SalePrice
SalePrice<- log(SalePrice[-c(524,1299)])
 
for ( i in names(train_data_house_price)){
  
  if(class(train_data_house_price$i) == 'character'){
    train_data_house_price$i<- as.factor(train_data_house_price$i)
  }
}


train_data_house_price$Total_area<- numerical_train_v2$Total_area

str(train_data_house_price)
set.seed(8788) 

rfModel<- randomForest(x = train_data_house_price,y = SalePrice, ntree = 1000, importance = TRUE)

varImpPlot(rfModel)


str(train_data_house_price)
names(dummy_vars)
names(categorical_train)
set.seed(874)
