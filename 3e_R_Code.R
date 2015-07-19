##########
#Variabel Zoo
##########
L_1 = 0.005
Sr_1 = 0.1 
S_1 = 0.01 
K_1 = 0.01
B_1 = 0.01 
Sp_1 = 0.01 
Lt_1 = 0.01 
Pc_1 = 0.9 

 setwd("C:/Users/Lauren/Documents/Delta Contract/Mappings/rapid testing")

 library(Hmisc)

data = read.csv("MutipleRegression3e.csv", header = T)

# Continoius vairbles 
reduced <- lm(data$R1 ~ data$V2 + data$V3 + data$V4, data = data)
full <- lm(data$R1~as.factor(data$V2 + data$V3 + data$V4), data= data)
anova(reduced, full)

# Pass LoF
LoF_P_value = anova(reduced,full)[2,6]
print("Pass Lof?")
print(LoF_P_value > L_1)

# Spearman 
# For this mapping should all be below .9
rcorr(as.matrix(data))

# Multiple Regression with all variables 
regression = lm(data$R1 ~ data$V2 + data$V3 + data$V4, data = data)
residuals = regression$residuals
sw<-shapiro.test(regression$residuals)
sw_P_value = shapiro.test(regression$residuals)[2]
sw_P_value
if(sw_P_value < S_1){
	print("Fail Shapiro")
}else{
	print("Pass Shapiro")
}

Response_Transformed = data$R1** .5
# Multiple Regression with all variables 
regression = lm(data$Response_Transformed ~ data$V2 + data$V3 + data$V4, data = data)
residuals = regression$residuals

### I tHink We Pass Shiprio 
sw<-shapiro.test(regression$residuals)
sw_P_value = shapiro.test(regression$residuals)[2]
sw_P_value
if(sw_P_value < S_1){
	print("Fail Shapiro")
}else{
	print("Pass Shapiro")
}

res<-regression$residual
fit<-regression$fitted
#run barlett's test
bar <- bartlett.test(list(res, fit), data=dataset)
bar_P_value = bar[3]

if(bar_P_value > B_1){  # if P > B_1, then the Residuals are the the same and we "Pass", and Pass = true 
print("Pass Bartlet")
}else{
	print("Fail Bartlet")
}

Response_Transformed_2 = 1/(data$R1 + .0001)

regression = lm(data$Response_Transformed_2 ~ data$V2 + data$V3 + data$V4, data = data)
residuals = regression$residuals

### I tHink We Pass Shiprio 
sw<-shapiro.test(regression$residuals)
sw_P_value = shapiro.test(regression$residuals)[2]
sw_P_value
if(sw_P_value < S_1){
	print("Fail Shapiro")
}else{
	print("Pass Shapiro")
}

res<-regression$residual
fit<-regression$fitted
#hist(res)
#run barlett's test
bar <- bartlett.test(list(res, fit), data=dataset)
bar_P_value = bar[3]

if(bar_P_value > B_1){  # if P > B_1, then the Residuals are the the same and we "Pass", and Pass = true 
print("Pass Bartlet")
}else{
	print("Fail Bartlet")
}


# data = cbind(data, Response_Transformed, Response_Transformed_2)
# write.csv(data, "MutipleRegression3e.csv")


