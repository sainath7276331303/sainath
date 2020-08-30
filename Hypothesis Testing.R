#-------------------------Cutlets Dataset--------------------------------#

#Ho: There is no difference in the diameter of the cutlets
#Ha: There is a difference in the diameter of the cutlets
#y is continous and x is discrete
library(readr)
Cutlets <- read_csv("file.choose()")
View(Cutlets)
attach(Cutlets)
shapiro.test(`Unit A`) #Performing Normality  test for Unit A
#p-value = 0.32 > 0.05 => It follows Normal Distribution
shapiro.test(`Unit B`) #Performing Normality test for Unit B
#p-value = 0.5255 > 0.05 => It follows Normal Distribution
var.test(`Unit A`,`Unit B`) #Checking Variance
#variance = 0.3136 > 0.05 => Variance is equal
t.test(`Unit A`,`Unit B`, alternative = "two.sided", conf.level = 0.95, correct = TRUE) #Performing 2 sample t test
#p-value = 0.4723 > 0.05 => Fail to reject Null Hypothesis


#--------------------------------LabTAT Dataset--------------------------------------#

#Ho: There is no difference in the average TAT of reports of the laboratories
#Ha: There is a difference in the average TAT of reports of the laboratories
#y is continuous and x is discrete
LabTAT <- read.csv("file.choose()", header=FALSE)
View(LabTAT)
attach(LabTAT)
l1 <- c(`V1`)
l2 <- c(`V2`)
l3 <- c(`V3`)
l4 <- c(`V4`)
shapiro.test(l1) #Performing Normality test on Lab1
#P-value = 0.5508 > 0.05 => It follows Normal Distribution
shapiro.test(l2) #Performing Normality test on Lab2 
#p-value = 0.8637 > 0.05 => It follows Normal Distribution  
shapiro.test(l3) #Performing Normality test on Lab3
#p-value = 0.4205 > 0.05 => It follows Normal Distribution
shapiro.test(l4) #Performing Normality test on Lab4 
#p-value = 0.6619 > 0.05 => It follows Normal Distribution
#Performing Variance test on data
var.test(l1,l2) #p-value = 0.1675 > 0.05
var.test(l1,l3) #p-value = 0.013 < 0.05 
var.test(l1,l4) #p-value = 0.014 < 0.05
var.test(l2,l3) #p-value = 0.274 > 0.05
#All the variances are not equal
Stacked_Data <- stack(LabTAT)
View(Stacked_Data)
attach(Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data) #performing one way anova test
summary(Anova_results)
#p-value < 0.05
#We reject Null Hypothesis



#----------------------------------BuyerRatio Dataset---------------------------#

#Ho: Male-Female buyer ratio is similar across regions
#Ha: Male-Female buyer ratio is different across regions
#y is discrete and x is discrete 
BuyerRatio1 <- read_excel("file.choose()")
View(BuyerRatio1)
attach(BuyerRatio1)
#creating table for Direction and males
table(Direction, Males)
t <- prop.table(table(Males, Females, Direction))
t
chisq.test(table(Direction, Males)) #Performing chi-squared test
#p-value = 0.2133 > 0.05 => We fail to reject Null Hypothesis


#--------------------------Customer Order Form------------------#

#Ho: Defective % doesn't varies by centre
#Ha: Defetive % varies by centre
#y is discrete and x is discrete
Defective_Country <- read_excel("file.choose()")
View(Defective_Country)
attach(Defective_Country)
table(Country, Defective)
t <- prop.table(table(Country, Defective))
t
chisq.test(Country,Defective)
#p-value = 0.2771 > 0.05 => We fail to reject null hypothesis


#-----------------------------------Fantaloons Dataset-------------------------------#

#Ho: % of males versus females walking in to the store doesn't differ based on day of the week. 
#Ha: % of males versus females walking in to the store differ based on day of the week. 
#y is discrete and x is discrete
Faltoons1_csv <- read_excel("file.choose()")
View(Faltoons1_csv)
attach(Faltoons1_csv)
table(Days, Males, Female)
prop.table(table(Days, Males, Female))
chisq.test(table(Males, Female)) #Performing chi-squared test
#p-value > 0.05 => we fail to reject Null Hypothesis