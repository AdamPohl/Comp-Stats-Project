# install.packages("lawstat", repos="http://cran.us.r-project.org")
# install.packages("permute", repos="http://cran.us.r-project.org")
# install.packages("plotrix", repos="http://cran.us.r-project.org")
# install.packages("MASS", repos="http://cran.us.r-project.org")
library("plotrix")
library("lawstat")
library("permute")
library("MASS")
                          #Directory link
adult = read.table("/home/huaraz2/Desktop/Comp-Stats-Project/adults.txt", header =  FALSE, sep = ",")

adult = na.omit(adult)

adult$V3 = NULL #This removes the fnlwgt variable.

names(adult) = c('Age', 'Workclass', 'EduCat', 'EduNum', 'Marital-status', 'Occupation', 'Relationship', 'Race', 'Sex', 'Capital-Gain', 'Capital-Loss', 'HPW', 'Native-Country', 'Income')

                          #Exploritory Data Section
summary(adult)

"How does someone's race affect their yearly income."

#White Race Vs Income.
'White Race Greater Than'
WhiteRace = subset(adult, adult[,"Race"] == "White", select = c(Race, Income))
WhiteRaceGreaterThan = subset(adult, adult[,"Race"] == "White" & adult[,"Income"] == ">50K", select = c(Race, Income))
(nrow(WhiteRaceGreaterThan) / nrow(WhiteRace)) * 100
(nrow(WhiteRaceGreaterThan) / nrow(adult)) * 100

'White Race Less Than'
WhiteRaceLessThan = subset(adult, adult[,"Race"] == "White" & adult[,"Income"] == "<=50K", select = c(Race, Income))
(nrow(WhiteRaceLessThan) / nrow(WhiteRace)) * 100
(nrow(WhiteRaceLessThan) / nrow(adult)) * 100

#Asian-Pac-Islander Race Vs Income.
'Asian-Pac-Islander Race Greater Than'
AsianPacIslanderRace = subset(adult, adult[,"Race"] == "AP Islander", select = c(Race, Income))
AsianPacIslanderRaceGreaterThan = subset(adult, adult[,"Race"] == "AP Islander" & adult[,"Income"] == ">50K", select = c(Race, Income))
nrow(AsianPacIslanderRaceGreaterThan) / nrow(AsianPacIslanderRace) * 100
nrow(AsianPacIslanderRaceGreaterThan) / nrow(adult) * 100
'Asian-Pac-Islander Race Less Than'
AsianPacIslanderRaceLessThan = subset(adult, adult[,"Race"] == "AP Islander" & adult[,"Income"] == "<=50K", select = c(Race, Income))
nrow(AsianPacIslanderRaceLessThan) / nrow(AsianPacIslanderRace) * 100
nrow(AsianPacIslanderRaceLessThan) / nrow(adult) * 100

#Amer-Indian-Eskimo Race Vs Income.
'Amer-Indian-Eskimo Race Greater Than'
AmerIndianEskimoRace = subset(adult, adult[,"Race"] == "AI Eskimo", select = c(Race, Income))
AmerIndianEskimoRaceGreaterThan = subset(adult, adult[,"Race"] == "AI Eskimo" & adult[,"Income"] == ">50K", select = c(Race, Income))
nrow(AmerIndianEskimoRaceGreaterThan) / nrow(AmerIndianEskimoRace) * 100
nrow(AmerIndianEskimoRaceGreaterThan) / nrow(adult) * 100
'Amer-Indian-Eskimo Race Less Than'
AmerIndianEskimoRaceLessThan = subset(adult, adult[,"Race"] == "AI Eskimo" & adult[,"Income"] == "<=50K", select = c(Race, Income))
nrow(AmerIndianEskimoRaceLessThan) / nrow(AmerIndianEskimoRace) * 100
nrow(AmerIndianEskimoRaceLessThan) / nrow(adult) * 100

#Other Race Vs Income.
'Other Race Greater Than'
OtherRace = subset(adult, adult[,"Race"] == "Other", select = c(Race, Income))
OtherRaceGreaterThan = subset(adult, adult[,"Race"] == "Other" & adult[,"Income"] == ">50K", select = c(Race, Income))
nrow(OtherRaceGreaterThan) / nrow(OtherRace) * 100
nrow(OtherRaceGreaterThan) / nrow(adult) * 100
'Other Race Less Than'
OtherRaceLessThan = subset(adult, adult[,"Race"] == "Other" & adult[,"Income"] == "<=50K", select = c(Race, Income))
nrow(OtherRaceLessThan) / nrow(OtherRace) * 100
nrow(OtherRaceLessThan) / nrow(adult) * 100

#Black Race Vs Income.
'Black Race Greater Than'
BlackRace = subset(adult, adult[,"Race"] == "Black", select = c(Race, Income))
BlackRaceGreaterThan = subset(adult, adult[,"Race"] == "Black" & adult[,"Income"] == ">50K", select = c(Race, Income))
nrow(BlackRaceGreaterThan) / nrow(BlackRace) * 100
nrow(BlackRaceGreaterThan) / nrow(adult) * 100
'Black Race Less Than'
BlackRaceLessThan = subset(adult, adult[,"Race"] == "Black" & adult[,"Income"] == "<=50K", select = c(Race, Income))
nrow(BlackRaceLessThan) / nrow(BlackRace) * 100
nrow(BlackRaceLessThan) / nrow(adult) * 100

#Graph of Race Vs. Income.
raceVincome = rbind(WhiteRace, AsianPacIslanderRace, AmerIndianEskimoRace, OtherRace, BlackRace)
RVI = table(raceVincome)
RVI
barplot(RVI[,1], beside = TRUE, main = "Race Vs. <=50K income", xlab = "Race", ylab = "No. of people", col = rainbow(5))
barplot(RVI[,2], beside = TRUE, main = "Race Vs. >50K income", xlab = "Race", ylab = "No. of people", col = rainbow(5))

#Spread of different races in the census.
race = rbind(WhiteRace, AsianPacIslanderRace, AmerIndianEskimoRace, OtherRace, BlackRace)
race$Income = NULL
table(race)
barplot(table(race), main = "Spread of the Race through the Census", xlab = "Race", ylab = "No. of people", col = rainbow(6))

#Spread of sex in the census.
pie3D(table(adult[, "Sex"]), main = "Spread of gender through the Census",  col = rainbow(2))
table(adult[, "Sex"])

                    #Chapter 3 Research questions
"Q1: Is there a difference in Capital gains between Males and Females?"
Sex = adult[, "Sex"]
CapitalGain = adult[,"Capital-Gain"]
SexVCapitalGain = table(CapitalGain,Sex)
nrow(SexVCapitalGain)
barplot(SexVCapitalGain[2:118,1], xlab = "Capital Gains", ylab = "Number of people", beside = TRUE, main = "Spread of Capital Gains for Females", col = rainbow(50))
barplot(SexVCapitalGain[2:118,2], xlab = "Capital Gains", ylab = "Number of people", beside = TRUE, main = "Spread of Capital Gains for Males", col = rainbow(60))
barplot(SexVCapitalGain[1,], xlab = "Capital Gains", ylab = "Number of people", beside = TRUE, main = "Zero Capital Gains for Males and Females", col = rainbow(2))

#My original thought was to go through the differences and work something from there but this never panned out but hey it is nice code so I left it in.
Difference = {}
for (i in 1:118){
  X = SexVCapitalGain[i,1] - SexVCapitalGain[i,2]
  Difference = rbind(Difference, X)
  }
SexVCapitalGain = cbind(SexVCapitalGain, Difference)
FemaleCapitalGain = subset(SexVCapitalGain, SexVCapitalGain[, 3] > 0, select = c(3))
MaleCapitalGain = subset(SexVCapitalGain, SexVCapitalGain[, 3] > 0, select = c(3))
nrow(MaleCapitalGain)
nrow(FemaleCapitalGain)



#Females
shapiro.test(SexVCapitalGain[,1])
symmetry.test(SexVCapitalGain[,1])

#Males
shapiro.test(SexVCapitalGain[,2])
symmetry.test(SexVCapitalGain[,2])

"H0: There is no difference in Capital Gains for Males and Females."
"HA: There is a difference in Capital Gains for Males and Females."

SvCG = c(SexVCapitalGain[,1], SexVCapitalGain[,2])
MeanDiff = rep(0,1000)
for (i in 1:1000){
  s = shuffle(236)
  MeanDiff[i] = mean(SvCG[s[1:118]]) - mean(SvCG[s[119:236]])
}
originalMeandiff = mean(SvCG[1:118]) - mean(SvCG[119:236])
#If pval < 0.05 then we can reject the null hypothesis that there is no link between the gender of a person and their capital gain.
pval = length(MeanDiff[MeanDiff >= originalMeandiff]) / 1000
if(pval > 0.05){
  "As the pval > 0.05 there is not enough evidence to discard H0."
} else {
  "As the pval < 0.05 there is enough evidence to discard H0."
}
pval


"Q2: Does the Race affect the number of hours per week people work?"
RvNHW = lm(adult[,12]~adult[,8])
summary(RvNHW)

par(mfrow = c(1,2))
plot(RvNHW, which = 1:2)
plot(RvNHW, which = 3:4)
plot(RvNHW, which = 5:6)

# OH FOR FUDGE SAKE!!!!

rVnhw = table(adult[,12], adult[,8])
shapiro.test(rVnhw)


#I HATE THIS PROJECT!!! (just don't tell Andreas)

                    #Chapter 4 Research questions
"Q3: Can we build a model to see which variables effect the income?"
par(mfrow = c(1,1))
occupation = as.numeric(adult[, "Occupation"])
sex = as.numeric(adult[, "Sex"])

model1 = lm(as.numeric(adult[,14])~ adult[,1] + adult[,2] + adult[,4] + adult[,6] + adult[,8] + adult[,9] + adult[,10] + adult[,11] + adult[,12] + adult[,13])
summary(model1)

#From the regression table we can imply that there is little or no link between a person's native country and their income. Therefore we will delete this from our model and work out a new regression table.

model2 = lm(as.numeric(adult[,14])~ adult[,1] + adult[,2] + adult[,4] + adult[,6] + adult[,8] + adult[,9] + adult[,10] + adult[,11] + adult[,12])
summary(model2)

#Unsurpringly as we just got rid of native country, we are able to imply from the new regression table that the Race of someone has little or no effect on their income.  We will therefore remove Race and try again.

model3 = lm(as.numeric(adult[,14])~ adult[,1] + adult[,2] + adult[,4] + adult[,6] + adult[,9] + adult[,10] + adult[,11] + adult[,12])
summary(model3)

#From the regression table for model3 we can see that the next variable that we should get rid of is Workclass (adult[, 2]) as we can infer that it does not affect the income.

model4 = lm(as.numeric(adult[,14])~ adult[,1] + adult[,4] + adult[,6] + adult[,9] + adult[,10] + adult[,11] + adult[,12])
summary(model4)

#From the regression table we can imply that all of the variables in model4 are signifcant in deciding what a person's income will be.  we will now perform a Principal Componant Analysis to check if what variables are actually linked to the income.

model6 = subset(adult, select = c('Age', 'EduNum', 'Capital-Gain', 'Capital-Loss', 'HPW'))
model6 = cbind(model6, occupation, sex)
PCA2 = prcomp(model6[,1:7])
PCA2
summary(PCA2)

plot(PCA2$sd, type = "l", xlab = "Component number", ylab = "Eigenvalues", main = "Scree plot")



"Q4: Test if Education and Capital gain are related. You can use as many of the other variables in your model you want."
model = lm(adult[, 10]~adult[, 3])
summary(model)
cor.test(adult[, 10], as.numeric(adult[, 3]))

#Be aware that this for-loop takes a long time to run.
x = {}
y = {}
for (i in 1:30162) {
  if(adult[i, "EduCat"] != "Bachelors") {
    if(adult[i, "EduCat"] != "Doctorate") {
      if(adult[i, "EduCat"] != "Masters") {
        if(adult[i, "EduCat"] != "Prof-school") {
          x = rbind(x, adult[i,])
        } else {
          y = rbind(y, adult[i,])
        }
      } else {
        y = rbind(y, adult[i,])
      }
    } else {
      y = rbind(y, adult[i,])
    }
  } else {
    y = rbind(y, adult[i,])
    }
}

model1 = lm(x[, 10]~ x[, 3])
summary(model1)
cor.test(x[, 10], as.numeric(x[, 3]))

model2 = lm(y[, 10]~ y[, 3])
summary(model2)
cor.test(y[, 10], as.numeric(y[, 3]))
