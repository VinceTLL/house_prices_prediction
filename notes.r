#notes 

# I will split the data into partitions to evaluate the  model

splitter<- train_data$YearBuilt
train_data$Id<- NULL

set.seed(34567)
index<- createDataPartition(splitter,p =0.70, list = FALSE )

train<- train_data[index,]
test<- train_data[-index,]

# converting data into DMMatrix



#Creating10 folds 
set.seed(4789)
folds<- createMultiFolds(train_sales_price, k = 10,times = 5)

set.seed(7895)
fitControl<- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 5,
                          index = folds,
                          returnData = FALSE,
                          verboseIter = FALSE,
                          allowParallel = TRUE)


# building the grid
xgbmGrid<- expand.grid(nrounds = c(1000),
                       max_depth = c(2,4,6,8,10),
                       colsample_bytree = seq(0.5,0.9,length.out = 5),
                       eta = c(0.1,0.01, 0.001, 0.0001),
                       gamma = 1,
                       subsample = 1,
                       min_child_weight = 1
)
set.seed(458)
xgboost_train<- train(sale_price~. ,data =train,
                      tuneGrid = xgbmGrid, method = 'xgbTree', metric = 'RMSE', 
                      trControl = fitControl ) 


# I will use the xgboost package as it should be faster

train_df_categorical$Total_area<- train_df_numerical$Total_Area

train_data<- train_df_categorical
train_data$sale_price<- sale_price

# transforming the response variable in log to reduce skeweness

train_data$house_with_Garage<- as.factor(train_data$house_with_Garage)

#transforming total area  in log as it is right skewed as well



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

#adding total area
train_data_v2$Total_area<- train_data$Total_area
sale_price<- train_data$sale_price

#creating partitions 

set.seed(345)
index<- createDataPartition(sale_price,p =0.70, list = FALSE )

train<- train_data_v2[index,]
sale_price_train<- sale_price[index]
test<- train_data_v2[-index,]
sale_price_test<-sale_price[-index]
# creating DM matrices for xgboost algo 

dtrain<- xgb.DMatrix(data = as.matrix(train), label = sale_price_train)


default_param<- list(
  objective = 'reg:linear',
  booster = 'gbtree',
  eta = 0.01,
  gamma =  0,
  max_depth = 20,
  min_child_weight = 4,
  colsample_bytree = 0.4,
  subsample = 1)

set.seed(54796)
xgbcv_v4<-xgb.cv(params = default_param,data = dtrain, nrounds = 1000, nfold = 10,stratified = TRUE,
                 print_every_n = 40,early_stopping_rounds = 30, maximize = FALSE)

xgb_mode<- xgb.train(data =dtrain,params = default_param, nrounds = 947)
#performance returned 0.1480686 