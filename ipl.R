setwd("F:/R/datasets/Projects")
ipl=read.csv("IPL Ball-by-Ball 2008-2020.csv")
matches=read.csv("IPL Matches 2008-2020.csv")

#Packages
library(MASS)
library(dplyr)
library(caret)
library(caretEnsemble)
library(ggplot2)
library(lubridate)

#basic info
head(matches)
tail(ipl)
summary(ipl)
str(ipl)
View(ipl)

#checking N\A
table(is.na(ipl$dismissal_kind))

#matches
head(matches)
tail(matches)
summary(matches)
str(matches)
View(ipl)

#converting date format
matches$date = as.Date(matches$date)
matches$season = format(matches$date,'%Y')

#grouping matches as per season
table(matches$season)
matchbyseason=matches%>%
  group_by(season)%>%
  summarise(Total=n())
matchbyseason
ggplot(matchbyseason,aes(season,Total))+geom_bar(stat = 'identity')

#win by teams
mostwin=matches%>%
  group_by(winner)%>%
  summarise(wins=n())%>%
  arrange(wins)
mostwin
ggplot(mostwin,aes(winner,wins,fill=winner))+geom_bar(stat = 'identity')

#most matches hosted by venue
ven=matches%>%
  group_by(venue)%>%
  summarise(matchh=n())%>%
  arrange(desc(matchh))
ven

            ###BATSMAN ANALYSIS###

#top scoring batsman
top=ipl%>%
  group_by(batsman)%>%
  summarise(batsman_runs=sum(batsman_runs))%>%
  arrange(desc(batsman_runs))%>%
  top_n(10)
top

#most player of matches
pom=matches%>%
  group_by(player_of_match)%>%
  summarise(no=n())%>%
  arrange(desc(no))%>%
  top_n(10)
pom

ggplot(pom,aes(player_of_match,no,fill=player_of_match))+geom_bar(stat = "identity")

#Most no of sixes
mos=ipl%>%
  group_by(batsman)%>%
  filter(batsman_runs==6)%>%
  summarise(maximum=n())%>%
  arrange(desc(maximum))%>%
  top_n(10)
mos

ggplot(mos,aes(x=reorder(batsman,maximum),y=maximum))+
         geom_bar(stat = "identity",fill="red")

#most no of fours
mof=ipl%>%
  group_by(batsman)%>%
  filter(batsman_runs==4)%>%
  summarise(fours=n())%>%
  arrange(desc(fours))%>%
  top_n(10)
mof
 
ggplot(mof,aes(x=reorder(batsman,fours),y=fours))+
  geom_bar(stat = "identity",fill="#00abff")

#strike rate above 1500 runs
StrikeRateDF = ipl %>% 
  group_by(batsman) %>% 
  summarize(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2)) %>%
  filter(TotalRuns >=1500) %>%
  arrange(desc(StrikeRate)) 
StrikeRateDF


#strike rate above 3000 runs
StrikeRate = ipl %>% 
  group_by(batsman) %>% 
  summarize(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2)) %>%
  filter(TotalRuns >=3000) %>%
  arrange(desc(StrikeRate)) 
StrikeRate


StrikeRate1 = ipl %>% 
  group_by(batsman) %>% 
  summarize(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2)) %>%
  arrange(desc(StrikeRate)) 
StrikeRate1

StrikeRate2 = ipl %>% 
  group_by(batsman) %>% 
  summarize(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2)) %>%
  filter(TotalRuns >= 100) %>%
  arrange(desc(StrikeRate)) 
StrikeRate2

            #####dinesh karthick analysis######
dk=ipl%>%
  filter(batsman=='KD Karthik')%>%
  summarise(totalrun=sum(batsman_runs))
dk

dksr=ipl%>%
  filter(batsman=='V Kohli')%>%
  summarise(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2))
dksr        

dkfo=ipl%>%
  filter(batsman=='KD Karthik')%>%
  group_by(over>=10)%>%
  summarise(totalrun=sum(batsman_runs))
dkfo

dk15=ipl%>%
  filter(batsman=='MS Dhoni')%>%
  filter(over>=15)%>%
  summarise(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2))
dk15

dkl15=ipl%>%
  filter(batsman=='MS Dhoni')%>%
  filter(over<15)%>%
  summarise(TotalRuns = sum(batsman_runs) ,StrikeRate = round( (sum(batsman_runs)/n()) *100,2))
dkl15


dkteam=ipl%>%
  filter(batsman =='KD Karthik')%>%
  group_by(batting_team)%>%
  count(batting_team)
dkteam

table(ipl$batsman_runs)

dk6=ipl%>%
  filter(batsman=='KD Karthik',batsman_runs==6)%>%
  summarise(maximum=n())
dk6

dk4=ipl%>%
  filter(batsman=='KD Karthik')%>%
  count(batsman_runs==4)
dk4

dka15=ipl%>%
  filter(batsman=='KD Karthik',batsman_runs==6)%>%
  filter(over>=15)%>%
  summarise(maximum=n())
dka15

dkm=ipl%>%
  filter(batsman=="KD Karthik")%>%
  count(id)
dkm
head(matches1)
matches1=matches1[-c(1)]



####BOWLER ANALYSIS####

#most no of wickets
wicketmode=c("bowled","caught","caught and bowled","hit wicket","lbw","stumped")
mow=ipl%>%
  group_by(bowler)%>%
  filter(dismissal_kind%in%wicketmode)%>%
  summarise(wickets=n())%>%
  arrange(desc(wickets))%>%
  top_n(10)
mow
ggplot(mow,aes(x=reorder(bowler,wickets),y=wickets))+
  geom_bar(stat = "identity",fill= "navy")

#most six conceded by bowlers
bos=ipl%>%
  group_by(bowler)%>%
  filter(batsman_runs==6)%>%
  summarise(maximum=n())%>%
  arrange(desc(maximum))%>%
  top_n(10)
bos
ggplot(bos,aes(x=reorder(bowler,maximum),y=maximum))+
  geom_bar(stat = "identity",fill="orange")

#most no of fours conceded by bowler
bof=ipl%>%
  group_by(bowler)%>%
  filter(batsman_runs==4)%>%
  summarise(fours=n())%>%
  arrange(desc(fours))%>%
  top_n(10)
bof
ggplot(bof,aes(x=reorder(bowler,fours),y=fours))+
  geom_bar(stat = "identity",fill="sky blue")

#Best economical bowler
economyDF = ipl %>% 
  group_by(bowler) %>%  
  summarise(ipl = n(), economyrate = round((sum(total_runs)/n())*6,2) ) %>% 
  arrange(economyrate) %>%
  filter(ipl >= 600)
economyDF

        ####FIELDING ANALYSIS####
#most catch by fielder
catch=ipl%>%
  group_by(fielder)%>%
  filter(dismissal_kind=="caught")%>%
  summarise(nos=n())%>%
  arrange(desc(nos))%>%
  top_n(10)
catch

#most runout by fielder
ro=ipl%>%
  group_by(fielder)%>%
  filter(dismissal_kind=="run out")%>%
  summarise(runot=n())%>%
  arrange(desc(runot))%>%
  top_n(10)
ro

#most stumping by wk
stumping=ipl%>%
  group_by(fielder)%>%
  filter(dismissal_kind=="stumped")%>%
  summarise(stp=n())%>%
  arrange(desc(stp))%>%
  top_n(10)
stumping

#total innings
inn=ipl%>%
  group_by(inning)%>%
  summarise(n=n())
inn

#toss winning by teams
toss=matches%>%
  group_by(toss_winner)%>%
  summarise(won=n())%>%
  arrange(won)
toss

#influence of toss
res=matches%>%
  group_by(toss_decision)%>%
  summarise(n())
res

batting1st=matches%>%
  group_by(season)%>%
  filter(result=='runs')%>%
  summarise(result=n())
batting1st  

batting2nd=matches%>%
  group_by(season)%>%
  filter(result=='wickets')%>%
  summarise(result=n())
batting2nd

rm=matches%>%
  filter(result=='runs')%>%
  summarise(run=max(result_margin))
rm
head(matches)
matches1=matches[-c(1,4,6,13:17)]
head(matches1)
matches1$weekday = weekdays(matches$date)

#converting to factor
matches1$venue=as.factor(matches1$venue)
matches1$team1=as.factor(matches1$team1)
matches1$team2=as.factor(matches1$team2)
matches1$toss_winner=as.factor(matches1$toss_winner)
matches1$toss_decision=as.factor(matches1$toss_decision)
matches1$winner=as.factor(matches1$winner)
matches1$result=as.factor(matches1$result)
matches1$season=as.factor(matches1$season)
str(matches1)
matches1=matches1[-c(1,2)]
head(matches1)

#Removing Null Values
table(is.na(matches1))
matches2=na.omit(matches1)
table(is.na(matches2))


#selecting current ipl teams
matches3=matches2%>%
  filter(team1 %in% c("Chennai Super Kings", "Delhi Capitals", "Delhi Daredevils",
                      "Kings XI Punjab","Kolkata Knight Riders","Mumbai Indians",
                      "Rajasthan Royals","Royal Challengers Bangalore","Sunrisers Hyderabad"))
matches3=matches2%>%
  filter(team2 %in% c("Chennai Super Kings", "Delhi Capitals", "Delhi Daredevils",
                      "Kings XI Punjab","Kolkata Knight Riders","Mumbai Indians",
                      "Rajasthan Royals","Royal Challengers Bangalore","Sunrisers Hyderabad"))
table(matches3$team2)
str(matches3)

#Filtering only runs and wicket
res=matches2%>%
  filter(result=="runs"|result=="wickets")
table(res$result)

#converting o and 1
res$Result=ifelse(res$result=="runs",1,0)
head(res)
matches3=res[-c(7,8)]
matches3$Result=as.factor(matches3$Result)
str(matches3)
matches3=matches3[-c(7)]
summary(matches3)
head(matches3)
dim(matches3)
matches3$Result=as.numeric(matches3$Result)

#one hot encoding
dummy <- dummyVars(" ~ .", data=matches3)
matches4 <- data.frame(predict(dummy, newdata = matches3)) 
head(matches4)
matches4$Result=as.factor(matches4$Result)

#Logistic Regression Model
# Fit the model
model = glm(Result ~., data = matches4, family = binomial)

# Summarize the final selected model
summary(model)

# Make predictions
probabilities = model %>%
  predict(matches4, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
mean(predicted.classes==matches4$Result)

#set up train control
set.seed(123)
mycontrol=trainControl(method="cv",
                       number=3,
                       savePredictions = TRUE,
                       allowParallel = TRUE)

#SVM
# training model with SVM
SVModel <- train(Result ~ ., data = matches4,
                 method = "svmPoly",
                 trControl= mycontrol,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1),
                 preProcess = c("pca","scale","center"),
                 na.action = na.omit
)

SVMPredictions <-predict(SVModel, matches4)

# Create confusion matrix
cmSVM <-confusionMatrix(SVMPredictions, matches4$Result)
print(cmSVM)

# Train a model with above parameters. We will use C5.0 algorithm
DecTreeModel <- train(Result ~ ., data = matches4, 
                      method = "C5.0",
                      preProcess=c("scale","center"),
                      trControl= mycontrol,
                      na.action = na.omit
)

#Predictions
DTPredictions <-predict(DecTreeModel, matches4, na.action = na.pass)

# Print confusion matrix and results
cmTree <-confusionMatrix(DTPredictions, matches4$Result)
print(cmTree)

