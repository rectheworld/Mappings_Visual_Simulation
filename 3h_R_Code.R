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

 setwd("C:/Users/Lauren/Documents/Delta Contract/Mappings")

 library(Hmisc)

 rename = function(file){
	file[,1] -> R1
	file[,2] -> V2
	file[,3] -> V3
	file[,4] -> V4
	file[,5] -> V5
	file[,6] -> V6
	file[,7] -> V7
	return(data.frame(R1,V2,V3,V4,V5, V6, V7))
}



file = read.csv("example.csv", header = T, sep = ",")

sim2 = rename(file)

# Continoius vairbles 
reduced <- lm(sim2$R1 ~  sim2$V2 + sim2$V3 + sim2$V4 + sim2$V7, data = sim2)
full <- lm(sim2$R1~as.factor(sim2$V2 + sim2$V3 + sim2$V4 + sim2$V7), data= sim2)
anova(reduced, full)


# Pass LoF
anova(reduced,full)
LoF_P_value = anova(reduced,full)[2,6]
print("Pass Lof?")
print(LoF_P_value > L_1)

# Spearman ( Did not check per Alexis Email ) 
rcorr(as.matrix(sim2))

# Multiple Regression with all variables 
regression = lm(sim2$R1 ~  sim2$V2 + sim2$V3 + sim2$V4 + sim2$V5 + sim2$V6 + sim2$V7)
residuals = regression$residuals


### I tHink We Pass Shiprio 
sw<-shapiro.test(regression$residuals)
sw_P_value = shapiro.test(regression$residuals)[2]

if(sw_P_value < S_1){
	print("Fail Shapiro")
}else{
	print("Pass Shapiro")
}

#grab residuals
res<-regression$residual
#grab fitted values
fit<-regression$fitted
#run barlett's test
bar <- bartlett.test(list(res, fit), data=dataset)
bar_P_value = bar[3]

if(bar_P_value > B_1){  # if P > B_1, then the Residuals are the the same and we "Pass", and Pass = true 
print("Pass Bartlet")
}else{
	print("Fail Bartlet")
}
}

