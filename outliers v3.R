setwd("C:\\Carlson MSBA\\Statistics\\Stats")
sales=read.csv("train_final.csv",header = TRUE)
stores=read.csv("stores.csv",header = TRUE)
context=read.csv("features.csv",header=TRUE)

test=read.csv("Test_Shawn.csv")
full=merge(sales,stores)

main=merge(full,context)

main$weeknum <- as.Date(main$Date, format="%Y-%m-%d")
main$weeknum <- as.Date(main$Date, format="%m/%d/%Y")
main$weeknum <- as.numeric( format(main$weeknum+3, "%U"))

main$IsHoliday<-as.factor(main$IsHoliday)

main[is.na(main)]<-0
main$SumMark=main$MarkDown1+main$MarkDown2+main$MarkDown3+main$MarkDown4+main$MarkDown5

simple<-lm(log(Weekly_Sales)~1, data=main)
complex<-lm(log(Weekly_Sales)~Type*Size*Temperature*Fuel_Price*CPI*Unemployment*SumMark*weeknum*IsHoliday, data=main)

help(step)
a = step(simple, scope=list(lower=simple, upper=complex), direction="forward")

data<-data.frame(Size=200000, Temperature=85, Fuel_Price=2.15, CPI=241.038, Unemployment=3.8,
                  weeknum=34, SumMark=0, Type="A", IsHoliday = "FALSE")
summary(a)

new_model<-lm(formula = log(Weekly_Sales) ~ Size + Type + weeknum + SumMark + 
                CPI + Unemployment + IsHoliday + Fuel_Price + Type:CPI + 
                Size:CPI + Size:Type + CPI:Unemployment + Type:Unemployment + 
                Size:Unemployment + Size:SumMark + Type:weeknum + SumMark:CPI + 
                weeknum:IsHoliday + Size:weeknum + SumMark:Unemployment + 
                weeknum:Unemployment + Type:SumMark + SumMark:IsHoliday + 
                Unemployment:Fuel_Price + CPI:Fuel_Price + IsHoliday:Fuel_Price + 
                SumMark:Fuel_Price + weeknum:CPI + weeknum:Fuel_Price + weeknum:SumMark + 
                Unemployment:IsHoliday + Size:Type:CPI + 
                Type:CPI:Unemployment + Size:Type:Unemployment + Size:CPI:Unemployment + 
                Size:Type:SumMark + Size:SumMark:CPI + 
                CPI:Unemployment:Fuel_Price + SumMark:CPI:Unemployment + 
                SumMark:CPI:Fuel_Price + weeknum:SumMark:Fuel_Price + Size:weeknum:SumMark
              + SumMark:Unemployment:Fuel_Price + SumMark:Unemployment:IsHoliday + 
                weeknum:Unemployment:IsHoliday + Size:Type:CPI:Unemployment 
              , data = main)
summary(new_model)

# This model is slightly diff from the output of step because matt has manually removed some to check

predict(new_model, newdata=data)

test$weeknum <- as.Date(test$Date, format="%Y-%m-%d")
test$weeknum <- as.Date(test$Date, format="%m/%d/%Y")
test$weeknum <- as.numeric( format(test$weeknum+3, "%U"))

test[is.na(test)]<-0
test$SumMark=test$MarkDown1+test$MarkDown2+test$MarkDown3+test$MarkDown4+test$MarkDown5
test$IsHoliday<-as.factor(test$IsHoliday)


print ("The sales for the stores are below")

print (exp(predict(new_model, newdata=test)))
summary(main)

attach(main)
plot(SumMark)
plot(SumMark~Type , col = c("blue", "red", "green"))


boxplot((Weekly_Sales))
# We have defined Weekly_sales to be an outlier if the value is greater than Q3 + IQR or lesser than Q1 - IQR
# Weekly sales taken silo shows a lot of outliers. Weekly Sales variable when included in our model, doesnot satisfy the normal distribution assumption

# So we tried normalizing it with various transformations. The square transformation of weekly sales is giving many outliers too. 
boxplot((Weekly_Sales)^2)

# Finally, we have normalized with the log transformation, the numbers fall witin the (Q3 + IQR) and (Q1-IQR)
boxplot(log(Weekly_Sales))
# The log of weekly sales in the presence or absense of a holiday will give us a fairly symmetric distribution around the mean and donot see any outliers
# No outlier treatment was required for the same

plot(log(Weekly_Sales) ~ Type,col = c("blue", "red", "green"))
# we see that the Weekly sales varies slightly from outlier definition. But since we are not doing the analysis at store type level, we have not treated the outliers 

plot((Weekly_Sales)^2 ~ Type, col = c("blue", "red", "green"))
# The square transformation increases the number  of outliers. That is 99.7% of the values are not within 3 std of the mean

plot(log(Weekly_Sales) ~IsHoliday)
# We do not see any outliers of the log(Weekly_Sales) with respect to the week having a holiday or not

# We have not done any outlier treatment post the log transformation
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss(main)

# There were NA values for the Markdown varaibles that we had in our data set. There were totally 5 Markdowns in our dataset. 
# We have treated the NA values with the value zero. We have summed all the 5 markdowns into a single variable with a safe assumption of the markdowns are similar to each other

We have recieved 4 data files for our project
1) Train dataset - This is the sales at a store-department-week level
2) Test dataset - This is the week, department and store for which the weekly sales has to be predicted
3) Features dataset - This has the features of the store like Total Markdown that is promotions, IsHoliday
Also it has the store location features like temperature, Fuel price, CPI, Unemploymenr
4) Stores dataset - This has the store, store type and store size. This remains same with time

Since all the features dataset is at a store level, we will not see any variation of them across the departments of each store  
This is the reason we have performed our analysis at a store level, ignoring the store-department level

To create the final dataset we have followed the below procedure,
1) Group the train dataset to a store level from the store-department level. Name the new data set as 
2) Merge the "full" dataset with "stores" dataset on the key "Store" of both the datasets. Name the new dsata set as "full"
3) Now  merge the "full" dataset with "features" dataset on the key "Store" and "Date" from both the datasets. Rename the dataset as "main"

The "main" data set is the input for the future analysis




