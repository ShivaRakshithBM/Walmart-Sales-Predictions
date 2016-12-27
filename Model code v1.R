setwd("C:\\Carlson MSBA\\Summer\\Statistics\\Stats")

sales=read.csv("train_final.csv",header = TRUE)
stores=read.csv("stores.csv",header = TRUE)
context=read.csv("features.csv",header=TRUE)

test=read.csv("Test_Shawn.csv")
full=merge(sales,stores)

#full2=subset(full,select = -c(Date))
main=merge(full,context)

main$weeknum <- as.Date(main$Date, format="%Y-%m-%d")
main$weeknum <- as.Date(main$Date, format="%m/%d/%Y")
main$weeknum <- as.numeric( format(main$weeknum+3, "%U"))

main[is.na(main)]<-0
main$SumMark=main$MarkDown1+main$MarkDown2+main$MarkDown3+main$MarkDown4+main$MarkDown5
simple<-lm(Weekly_Sales~1, data=main)
complex<-lm(Weekly_Sales~Type*Size*Temperature*Fuel_Price*CPI*Unemployment*SumMark*weeknum, data=main)
#complex<-lm(Weekly_Sales~Type*Size*Temperature*Fuel_Price*weeknum, data=main)

step(simple, scope=list(lower=simple, upper=complex), direction="forward")


test$weeknum <- as.Date(test$Date, format="%Y-%m-%d")
test$weeknum <- as.Date(test$Date, format="%m/%d/%Y")
test$weeknum <- as.numeric( format(test$weeknum+3, "%U"))

test[is.na(test)]<-0
test$SumMark=test$MarkDown1+test$MarkDown2+test$MarkDown3+test$MarkDown4+test$MarkDown5

final=lm(formula = log(Weekly_Sales) ~ Size + weeknum + SumMark + Type +
           CPI + Unemployment + Fuel_Price + Type:CPI + Size:CPI + Size:Type +
           CPI:Unemployment + Size:Unemployment + Type:Unemployment +
           Size:weeknum + SumMark:Unemployment + weeknum:SumMark + SumMark:CPI +
           weeknum:Unemployment + CPI:Fuel_Price + weeknum:Fuel_Price +
           weeknum:CPI + Type:Fuel_Price + Size:Fuel_Price + Unemployment:Fuel_Price +
           weeknum:Type + Size:Type:CPI + Size:Type:Unemployment + Type:CPI:Unemployment +
           Size:CPI:Fuel_Price + Size:weeknum:Fuel_Price + Size:Type:Fuel_Price +
           Type:CPI:Fuel_Price + Size:Unemployment:Fuel_Price + Size:weeknum:Unemployment +
           SumMark:CPI:Unemployment + CPI:Unemployment:Fuel_Price +
           weeknum:CPI:Unemployment + Size:Type:CPI:Fuel_Price, data = main)


final_test=lm(formula = log(Weekly_Sales) ~ Size + weeknum + SumMark + Type +
           CPI + Unemployment + Fuel_Price + Type:CPI + Size:CPI + Size:Type +
           CPI:Unemployment + Size:Unemployment + Type:Unemployment +
           Size:weeknum + SumMark:Unemployment + weeknum:SumMark + SumMark:CPI +
           weeknum:Unemployment + CPI:Fuel_Price + weeknum:Fuel_Price +
           weeknum:CPI + Type:Fuel_Price + Size:Fuel_Price + Unemployment:Fuel_Price +
           weeknum:Type + Size:Type:CPI + Size:Type:Unemployment + Type:CPI:Unemployment +
           Size:CPI:Fuel_Price + Size:weeknum:Fuel_Price + Size:Type:Fuel_Price +
           Type:CPI:Fuel_Price + Size:Unemployment:Fuel_Price + Size:weeknum:Unemployment +
           SumMark:CPI:Unemployment + CPI:Unemployment:Fuel_Price +
           weeknum:CPI:Unemployment + Size:Type:CPI:Fuel_Price, data = test)





print ("The sales for the stores are below")

print (exp(predict(final, newdata=test)))



# plot(final)


# 
# 
# 
# main$summarkdown=main$MarkDown1+main$MarkDown2+main$MarkDown3+main$MarkDown4+main$MarkDown5
# main[is.na(main)]=0
# main=subset(main,main$Type=='A')
# 
# main$TypeFStore=factor(main$Store)
# 
# 
# #main$weeknumber <- as.Date(main$Date, format="%Y-%m-%d")
# main$weeknumber <- as.Date(main$Date, format="%m/%d/%Y")
# main$weeknumber <- as.numeric( format(main$weeknumber+3, "%U"))
# 
# aggdata <-aggregate(main, by=list(Type,weeknumber), 
#                     FUN=mean, na.rm=TRUE)
# 
# 
# 
# 
# 
# attach(aggdata)
# 
# 
# 
# 
# 
# 
# # #with size,unemployment,cpi....without markdown
# # linefit14=lm(Weekly_Sales~weeknumber*Fuel_Price*Temperature*CPI*Unemployment*IsHoliday*Size*summarkdown)
# # summary(linefit14)
# # 
# # plot(Date,Weekly_Sales)
# 
# 
# linefit15=lm(Weekly_Sales~weeknumber+Size+summarkdown+Type)
# summary(linefit15)
# 
# plot(weeknumber,Weekly_Sales)
# 
# 
# 
# 
# 
