# install.packages("lawstat", repos="http://cran.us.r-project.org")
# install.packages("permute", repos="http://cran.us.r-project.org")
                          #Directory links
adult = read.table("/home/huaraz2/Desktop/Comp-Stats-Project/adults.txt", header =  FALSE, sep = ",")

adult$V3 = NULL
names(adult) = c('Age', 'Workclass', 'EduCat', 'EduNum', 'Marital-status', 'Occupation', 'Relationship', 'Race', 'Sex', 'Capital-Gain', 'Capital-Loss', 'HPW', 'Native-Country', 'Income')

                      #How does someone's race affect their yearly income.
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
pie(table(adult[, "Sex"]), main = "Spread of gender through the Census",  col = rainbow(2))
table(adult[, "Sex"])

                    #Chapter 3 Research questions
#Is there a difference in Capital gains between Males and Females?
Sex = adult[, "Sex"]
CapitalGain = adult[,"Capital-Gain"]
SexVCapitalGain = table(CapitalGain,Sex)
barplot(SexVCapitalGain[2:119,1], beside = TRUE, main = "Spread of Capital Gains for Females", col = rainbow(50))
barplot(SexVCapitalGain[2:119,2], beside = TRUE, main = "Spread of Capital Gains for Males", col = rainbow(60))
barplot(SexVCapitalGain[1,], beside = TRUE, main = "Zero Capital Gains for Males and Females", col = rainbow(2))

#My original thought was to go through the differences and work something from there but this never panned out but hey it is nice code so I left it in.
Difference = {}
for (i in 1:119){
  X = SexVCapitalGain[i,1] - SexVCapitalGain[i,2]
  Difference = rbind(Difference, X)
  }
SexVCapitalGain = cbind(SexVCapitalGain, Difference)
FemaleCapitalGain = subset(SexVCapitalGain, SexVCapitalGain[, 3] > 0, select = c(3))
MaleCapitalGain = subset(SexVCapitalGain, SexVCapitalGain[, 3] > 0, select = c(3))
nrow(MaleCapitalGain)
nrow(FemaleCapitalGain)

library("lawstat")
library("permute")

var.test(SexVCapitalGain[,1], SexVCapitalGain[,2])
#Females
shapiro.test(SexVCapitalGain[,1])
symmetry.test(SexVCapitalGain[,1])
#Males
shapiro.test(SexVCapitalGain[,2])
symmetry.test(SexVCapitalGain[,2])

"H0: There is no difference in Capital Gains for Males and Females."
"H1: There is a difference in Capital Gains for Males and Females."

SvCG = c(SexVCapitalGain[,1], SexVCapitalGain[,2])
ratiovar = rep(0,1000)
for (i in 1:1000){
  s = shuffle(238)
  ratiovar[i] = var(SvCG[s[1:119]]) / var(SvCG[s[120:238]])
}
originalratio = var(SvCG[1:119]) / var(SvCG[120:238])
#If this is > 0.05 then we can accept the null hypothesis that there is no link between the gender of a person and their capital gain.
pval = length(ratiovar[ratiovar >= originalratio]) / 1000
if(pval > 0.05){
  "There is not enough evidence to discard H0."
} else {
  "There is enough evidence to discard H0."
}
pval


#Does the Race affect the number of hours per week people work?
RvNHW = lm(adult[,12]~adult[,8])
summary(RvNHW)
anova(RvNHW)

plot(adult[,12], adult[,8])
lines(adult[,12], RvNHW$fitted.values)

par(mfrow = c(2,3))
plot(RvNHW, which = 1:6)

par(mfrow = c(1,2))
plot(RvNHW, which = 1:2)
plot(RvNHW, which = 3:4)
plot(RvNHW, which = 5:6)

                    #Chapter 4 Research questions
#Can we build a model to see which variables effect the income?

#Test if Education and Capital gain are related. You can use as many of the other variables in your model you want.
