                          #Directory links
                          #For Adam
adult <- read.table("/home/huaraz2/Desktop/Comp-Stats-Project/adults.txt", header =  FALSE, sep <- ",")
                          #For Joe
#adult <- read.table("H:/My Documents/adults.txt", header =  FALSE, sep <- ",") #(home computer)

adult$V3 <- NULL
names(adult) <- c('Age', 'Workclass', 'EduCat', 'EduNum', 'Marital-status', 'Occupation', 'Relationship', 'Race', 'Sex', 'Capital-Gain', 'Capital-Loss', 'HPW', 'Native-Country', 'Income')
head(adult)

#How does someone's race affect their yearly income.
#White Race Vs Income.
'White Race Greater Than'
WhiteRace <- subset(adult, adult[,"Race"] == "White", select <- c(Race, Income))
WhiteRaceGreaterThan <- subset(adult, adult[,"Race"] == "White" & adult[,"Income"] == ">50K", select <- c(Race, Income))
(nrow(WhiteRaceGreaterThan) / nrow(WhiteRace)) * 100
(nrow(WhiteRaceGreaterThan) / nrow(adult)) * 100
'White Race Less Than'
WhiteRaceLessThan <- subset(adult, adult[,"Race"] == "White" & adult[,"Income"] == "<=50K", select <- c(Race, Income))
(nrow(WhiteRaceLessThan) / nrow(WhiteRace)) * 100
(nrow(WhiteRaceLessThan) / nrow(adult)) * 100

#AsianPacIslander Race Vs Income.
'Asian Pac Islander Race Greater Than'
AsianPacIslanderRace <- subset(adult, adult[,"Race"] == "Asian Pac Islander", select <- c(Race, Income))
AsianPacIslanderRaceGreaterThan <- subset(adult, adult[,"Race"] == "Asian Pac Islander" & adult[,"Income"] == ">50K", select <- c(Race, Income))
nrow(AsianPacIslanderRaceGreaterThan) / nrow(AsianPacIslanderRace) * 100
nrow(AsianPacIslanderRaceGreaterThan) / nrow(adult) * 100
'Asian Pac Islander Race Less Than'
AsianPacIslanderRaceLessThan <- subset(adult, adult[,"Race"] == "Asian Pac Islander" & adult[,"Income"] == "<=50K", select <- c(Race, Income))
nrow(AsianPacIslanderRace)
nrow(AsianPacIslanderRaceLessThan) / nrow(AsianPacIslanderRace) * 100
nrow(AsianPacIslanderRaceLessThan) / nrow(adult) * 100

#Amer Indian Eskimo Race Vs Income.
'Amer Indian Eskimo Race Greater Than'
AmerIndianEskimoRace <- subset(adult, adult[,"Race"] == "Amer Indian Eskimo", select <- c(Race, Income))
AmerIndianEskimoRaceGreaterThan <- subset(adult, adult[,"Race"] == "Amer Indian Eskimo" & adult[,"Income"] == ">50K", select <- c(Race, Income))
nrow(AmerIndianEskimoRaceGreaterThan) / nrow(AmerIndianEskimoRace) * 100
nrow(AmerIndianEskimoRaceGreaterThan) / nrow(adult) * 100
'Amer Indian Eskimo Race Less Than'
AmerIndianEskimoRaceLessThan <- subset(adult, adult[,"Race"] == "Amer Indian Eskimo" & adult[,"Income"] == "<=50K", select <- c(Race, Income))
nrow(AmerIndianEskimoRaceLessThan) / nrow(AmerIndianEskimoRace) * 100
nrow(AmerIndianEskimoRaceLessThan) / nrow(adult) * 100

#Other Race Vs Income.
'Other Race Greater Than'
OtherRace <- subset(adult, adult[,"Race"] == "Other", select <- c(Race, Income))
OtherRaceGreaterThan <- subset(adult, adult[,"Race"] == "Other" & adult[,"Income"] == ">50K", select <- c(Race, Income))
nrow(OtherRaceGreaterThan) / nrow(OtherRace) * 100
nrow(OtherRaceGreaterThan) / nrow(adult) * 100
'Other Race Less Than'
OtherRaceLessThan <- subset(adult, adult[,"Race"] == "Other" & adult[,"Income"] == "<=50K", select <- c(Race, Income))
nrow(OtherRaceLessThan) / nrow(OtherRace) * 100
nrow(OtherRaceLessThan) / nrow(adult) * 100

#Black Race Vs Income.
'Black Race Greater Than'
BlackRace <- subset(adult, adult[,"Race"] == "Black", select <- c(Race, Income))
BlackRaceGreaterThan <- subset(adult, adult[,"Race"] == "Black" & adult[,"Income"] == ">50K", select <- c(Race, Income))
nrow(BlackRaceGreaterThan) / nrow(BlackRace) * 100
nrow(BlackRaceGreaterThan) / nrow(adult) * 100
'Black Race Less Than'
BlackRaceLessThan <- subset(adult, adult[,"Race"] == "Black" & adult[,"Income"] == "<=50K", select <- c(Race, Income))
nrow(BlackRaceLessThan) / nrow(BlackRace) * 100
nrow(BlackRaceLessThan) / nrow(adult) * 100

#Spread of different races in the census.
race <- rbind(WhiteRace, AsianPacIslanderRace, AmerIndianEskimoRace, OtherRace, BlackRace)
race$Income <- NULL
barplot(table(race), xlab = "Race", ylab = "no. of people", col = rainbow(6))
                    #Chapter 3 Research questions
#Is there a difference in Capital gains between Males and Females?
#Does the Race affect the number of hours per week people work?

                    #Chapter 4 Research questions
#Can we build a model to see which variables effect the income?
#Test if Education and Capital gain are related. You can use as many of the other variables in your model you want.
