#HW 1

############################################
#1 WHO dataset
WHO = read.csv("WHO.csv")
str(WHO)
names(WHO)
unique(WHO$Region)

#b. country with the biggest population
PopMax <- which.max(WHO$Population)
WHO$Country[PopMax]

#c. population in Malaysia
WHO$Population[WHO$Country == "Malaysia"]

#d. country with the lowest literacy
LitMin <- which.min(WHO$LiteracyRate)
WHO$Country[LitMin]

#e. richest coutnry in Europe based on GNI
WHO.Europe = subset(WHO, Region == "Europe")
Europe.MaxGNI <- which.max(WHO.Europe$GNI)
WHO.Europe$Country[Europe.MaxGNI]

#f. mean life expectancy of countries in Africa
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy, na.rm = TRUE)

#g. number of countries with population greater than 10,000
sum(WHO$Population > 10000)

#h. top 5 countries in the Americas with the highest child mortality
WHO.Americas = subset (WHO, Region == "Americas")
WHO.Americas$Country[order(WHO.Americas$ChildMortality)[35:31]]

############################################
#2. NBA dataset
#with xlsx package
require(xlsx)
NBA = read.xlsx("Historical NBA Performance.csv")
#if no xlsx package
NBA = read.csv("Historical NBA Performance.csv")
str(NBA)
names(NBA)
unique(NBA$Team)

#a. the year bulls has the highest winning percentage
NBA.Bulls = subset(NBA, Team == "Bulls")
Bulls.MaxWin <- which.max(NBA.Bulls$Winning.Percentage)
NBA.Bulls$Year[Bulls.MaxWin]

#b. teams with an even win-loss record in a year
NBA.Even = subset(NBA, Winning.Percentage == 0.5)
unique(NBA.Even$Team)

############################################
#3. Season Stats
Stats = read.csv("Seasons_Stats.csv")
str(Stats)
names(Stats)

#a. player with the highest 3-pt attempt rate in a season
Stats.Agg3p = setNames(aggregate(cbind(Stats$X3P, Stats$X3PA), by=list(Year=Stats$Year, Player=Stats$Player), FUN=sum), c("Year", "Player", "TP", "TPA"))
Stats.Agg3p$TPR <- Stats.Agg3p$TP / Stats.Agg3p$TPA
MaxTPR <- max(Stats.Agg3p$TPR, na.rm = TRUE)
Stats.MaxTPR.Players = subset(Stats.Agg3p, TPR == MaxTPR)
Stats.MaxTPR.Players

#b. player with the highest free throw rate in a season
Stats.AggFT = setNames(aggregate(cbind(Stats$FT,Stats$FTA),by=list(Year=Stats$Year, Player=Stats$Player), FUN=sum), c("Year", "Player", "FT", "FTA"))
Stats.AggFT$FTR <- Stats.AggFT$FT / Stats.AggFT$FTA
MaxFTR <- max(Stats.AggFT$FTR, na.rm = TRUE)
Stats.MaxFTR.Players = subset(Stats.AggFT, FTR == MaxFTR)
Stats.MaxFTR.Players

#c. what year/season did Lebron James scored the highest?
Stats.AggPTS = setNames(aggregate(Stats$PTS, by=list(Year=Stats$Year, Player=Stats$Player), FUN=sum), c("Year","Player","PTS"))
Stats.LeBronPTS = subset(Stats.AggPTS, Player == "LeBron James")
Lebron.MaxPTS <- which.max(Stats.LeBronPTS$PTS)
Stats.LeBronPTS$Year[Lebron.MaxPTS]

#d. what year/season did Michael Jordan scored the highest?
Stats.AggPTS = setNames(aggregate(Stats$PTS, by=list(Year=Stats$Year, Player=Stats$Player), FUN=sum), c("Year","Player","PTS"))
Stats.JordanPTS = subset(Stats.AggPTS, Player == "Michael Jordan*")
Jordan.MaxPTS <- which.max(Stats.JordanPTS$PTS)
Stats.JordanPTS$Year[Jordan.MaxPTS]

#e. player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
Stats.Kobe = subset(Stats, Player == "Kobe Bryant")
Kobe.MinMP <- which.min(Stats.Kobe$MP)
Stats.Kobe$PER[Kobe.MinMP]

#############################################
#4. National Universities Rankings
NUR = read.csv("National Universities Rankings.csv")
names(NUR)
str(NUR)

#a. University with the most number of undergrads
NUR$Undergrad.Enrollment <- as.numeric(gsub(",","",NUR$Undergrad.Enrollment))
MaxUGE <- which.max(NUR$Undergrad.Enrollment)
NUR$Name[MaxUGE]

#b. Average Tuition in the Top 10 University
NUR$Tuition.and.fees <- as.numeric(gsub("[$,.]","",as.character(NUR$Tuition.and.fees)))
Top10 = subset(NUR, Rank < 11)
mean(Top10$Tuition.and.fees)

