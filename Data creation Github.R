setwd("C:\\Carlson MSBA\\Statistics\\Stats")

library(sqldf)
features = read.csv("features.csv")
stores = read.csv("stores.csv")
test = read.csv("test.csv")
train = read.csv("train_final.csv")

typeof(train$Date)
class(train$Date)

train_stores = merge(train, stores)
train_stores_features = sqldf("Select * from train_stores as t left join features as f on t.Store = f.Store and t.Date = f.Date ")
train_stores_features[is.na(train_stores_features)] = 0

test_stores = merge(test,stores)
test_stores_features = merge(test_stores, features)
test_stores_features[is.na(test_stores_features)] = 0

