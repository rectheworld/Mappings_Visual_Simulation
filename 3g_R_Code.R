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


rename = function(file){
	file[,1] -> R1
	file[,2] -> V2
	file[,3] -> V3
	file[,4] -> V4
	file[,5] -> V5
	file[,6] -> V6
	return(data.frame(R1,V2,V3,V4, V5, V6))
}
setwd("C:/Users/Lauren/Documents/Delta Contract/Mappings")

library(Hmisc)

sim3 = read.csv("MultipleRegressionMapping3g.csv")
head(sim3)

# Continoius vairbles 
reduced <- lm(sim3$R1 ~ sim3$V2 + sim3$V3 + sim3$V4 + sim3$V5, data = sim3)
full <- lm(sim3$R1~as.factor(sim3$V2 + sim3$V3 + sim3$V4 + sim3$V5), data= sim3)
#round(sim3$V2) + round(sim3$V3) + round(sim3$V4) + round(sim3$V5) + round(sim3$V6)
# Pass LoF
anova(reduced,full)
LoF_P_value = anova(reduced,full)[2,6]
print("Pass Lof?")
print(LoF_P_value > L_1)
print(LoF_P_value)

# Spearman ( Did not check per Alexis Email ) 
rcorr(as.matrix(sim3))


# Multiple Regression with all variables 
regression = lm(sim3$R1 ~ sim3$V2 + sim3$V3 + sim3$V4 + sim3$V5)
residuals = regression$residuals
### I tHink We Pass Shiprio 
sw<-shapiro.test(regression$residuals)
sw_P_value = shapiro.test(regression$residuals)[2]
if(sw_P_value < S_1){
	print("Fail Shapiro")
}else{
	print("Pass Shapiro")
}

# transform response variable 
#sim3[,2] = sim3[,2]** 2 

#grab residuals
res<-regression$residual
#grab fitted values
fit<-regression$fitted
#run barlett's test

bar <- bartlett.test(list(res, fit), data=sim3)
bar_P_value = bar[3]
# I need to make the data MORE hetero !!! 
if(bar_P_value > B_1){  # if P > B_1, then the Residuals are the the same and we "Pass", and Pass = true 
print("Pass Bartlet")
}else{
	print("Fail Bartlet")
}
print(bar_P_value)

