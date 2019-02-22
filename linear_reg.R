library(car) #contains module for variance inflation factor
library(GGally)
df_all <- read.csv("Elantra242-Spring2018.csv")

#dataset from 2010 - 2014
df_train <- df_all[df_all$Year<= 2014,]

#data set from 2015 - 2017
df_test <- df_all[df_all$Year > 2014,]

#Show Scatter Matrix
print (ggscatmat(df_train, columns = 4:8, alpha = 0.8))

###############################
print ("Section 3.a")
print ("Model Info:")

fit <- lm(ElantraSales ~ Unemployment + ElantraQueries + CPI.All + CPI.Energy, data = df_train)
print(summary(fit))
print(vif(fit))

print ("Unemployement Removed")
fit <- lm(ElantraSales ~ ElantraQueries + CPI.All + CPI.Energy, data = df_train)
print(summary(fit))
print ("VIF:")
print(vif(fit))

print("CPI.Energy Removed")
fit <- lm(ElantraSales ~ Unemployment + ElantraQueries + CPI.All , data = df_train)
print(summary(fit))
print(vif(fit))


print("CPI.All Removed")
fit <- lm(ElantraSales ~ Unemployment + ElantraQueries + CPI.Energy , data = df_train)
print(summary(fit))
print(vif(fit))



print("CPI.Energy and Unemployment Removed")
fit <- lm(ElantraSales ~  ElantraQueries + CPI.All , data = df_train)
print(summary(fit))
print(vif(fit))

print ("CPI.All and CPI.Energy Removed")
fit <- lm(ElantraSales ~ ElantraQueries + Unemployment, data = df_train)
print(summary(fit))
print(vif(fit))


print("Section 3.b")
########################
fit <- lm(ElantraSales ~ MonthFactor + Unemployment + ElantraQueries + CPI.All + CPI.Energy, data = df_train)
print(summary(fit))
print(vif(fit))

final_model <- lm(ElantraSales ~ MonthFactor + Unemployment + ElantraQueries  , data = df_train)
print(summary(fit))
print(vif(fit))



print ("Section 3.c")
################################
predicted <- predict(final_model,df_test)
SSE<-sum((df_test$ElantraSales-predicted)^2)
SST<-sum((df_test$ElantraSales-mean(df_train$ElantraSales))^2)
OSR2<-1-(SSE/SST)
print ("-Predicted OSR------")
print (OSR2)



print("Section 3.d")
###############################
#Analysis of sales data for GMC Sierra

fit <- lm(zzzSales ~ Unemployment + zzzQueries + CPI.All + CPI.Energy, data = df_train)
print(summary(fit))
print(vif(fit))

print ("Unemployement Removed")
fit <- lm(zzzSales ~ zzzQueries + CPI.All + CPI.Energy, data = df_train)
print(summary(fit))
print ("VIF:")
print(vif(fit))

print("CPI.Energy Removed")
fit <- lm(zzzSales ~ Unemployment + zzzQueries + CPI.All , data = df_train)
print(summary(fit))
print(vif(fit))

print("CPI.Energy and Unemployment Removed")
fit <- lm(zzzSales ~  zzzQueries + CPI.All , data = df_train)
print(summary(fit))
print(vif(fit))