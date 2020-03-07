###########################################################

# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

###########################################################

library(RCurl)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(arules)
library(arulesViz)
library(tm)
library(wordcloud)
library(tidytext)
library(caret)
library(mapproj)
library(kernlab)
library(rgl)
library(misc3d)
library(e1071)
library(ElemStatLearn)
library(scatterplot3d)

df <- jsonlite::fromJSON("C:/Syracuse/Saltz/Project/fall2019-survey-M08.json")
View(df)

df$Destination.City <- gsub(",.*", "", df$Destination.City)
df$Origin.City <- gsub(",.*", "", df$Origin.City)

df <- df %>%
  mutate(type = cut(Likelihood.to.recommend,
                    breaks = c(0, 6, 8, 10),
                    labels = c("detractor","passive","promoter"))) %>%
  mutate(agegroup = cut(Age, 
                        breaks = c(0, 17, 59, Inf),
                        labels = c("Minor","Adult","SrCitizen"))) %>%
  mutate(Day.of.Flight = weekdays(as.Date(Flight.date, '%m/%d/%Y'))) %>%
  mutate(spend = cut(Shopping.Amount.at.Airport + Eating.and.Drinking.at.Airport,
                     breaks =  c(-Inf, 0, Inf),
                     labels = c("no", "yes")))

df[df$Day.of.Flight %in% c('Saturday','Sunday'),'Day.of.Flight'] <- 'weekend'
df[df$Day.of.Flight %in% c('Monday','Tuesday','Wednesday','Thursday','Friday'), 'Day.of.Flight'] <- 'weekday'

# Cleaning City and removing State Abbreviation
df$Destination.City <- gsub(",.*", "", df$Destination.City)
df$Origin.City <- gsub(",.*", "", df$Origin.City)

delete_list <- which(df$Flight.cancelled == 'No' & ((is.na(df$Arrival.Delay.in.Minutes) == TRUE) | (is.na(df$Departure.Delay.in.Minutes) == TRUE) | (is.na(df$Flight.time.in.minutes)) == TRUE))

length(delete_list)   

colnames(df)[colSums(is.na(df)) > 0]

# Delete rows with index label a & b    

df <- df[-delete_list,]

#Removing NAs where flight was cancelled and had NAs in Departure Delay, Arrival Delay and Flight time in minutes

df_test <- df %>% group_by(Origin.City, Destination.City)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100, 
            flight_distance = mean(Flight.Distance),
            flight_time = mean(Flight.time.in.minutes))

df_test <- na.omit(df_test)

df_test$flight_speed <- df_test$flight_distance/df_test$flight_time

# Average speed for a flight in miles/min
avg_flight_speed <- mean(df_test$flight_speed)

#Calculating Max delay for arrival delay and departure delay

max_dep_delay <- max(df$Departure.Delay.in.Minutes,na.rm = TRUE)
max_arr_delay <- max(df$Arrival.Delay.in.Minutes, na.rm = TRUE)

for(i in 1:nrow(df))
{
  if(df$Flight.cancelled[i] == 'Yes')
  {
    df$Flight.time.in.minutes[i] <- df$Flight.Distance[i] / avg_flight_speed
    df$Departure.Delay.in.Minutes[i] <- max_dep_delay
    df$Arrival.Delay.in.Minutes[i] <- max_arr_delay
  }
}

#Creation of Arrival Delay Ratio and Departure Delay Ratio

df$ArrivalDelayRatio <- round(df$Arrival.Delay.in.Minutes/df$Flight.time.in.minutes,2)
df$DepartureDelayRatio <- round(df$Departure.Delay.in.Minutes/df$Flight.time.in.minutes,2)



#---------------------------------------------------- DF CLEAN CHECK

df[(df$Flight.cancelled == 'Yes'),c(22:26)]

colnames(df)[colSums(is.na(df)) > 0]

nrow(df[is.na(df$Arrival.Delay.in.Minutes) | is.na(df$Departure.Delay.in.Minutes) | is.na(df$Flight.time.in.minutes),])

########################################################TEXT MINING#############################################################################

### Create a stopword list
# add a few words to the default stopword list
stop_words <- tibble(word = c(stopwords("english"),"flight","southeast"))
### Frequent Word in Promoter & Detractor
# promoter words

df_pro <- df %>%
  filter(type == "promoter") %>%
  unnest_tokens(word, freeText) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
# wordcloud

wordcloud(df_pro$word, df_pro$n, min.freq = 10, ordered.colors= TRUE)
# detractor words

df_det <- df %>%
  filter(type == "detractor") %>%
  unnest_tokens(word, freeText) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
# wordcloud

wordcloud(df_det$word, df_det$n, min.freq = 10, ordered.colors= TRUE)
#### Bigram Analysis
# promoter bigram 

df %>%
  filter(type == "promoter") %>%
  unnest_tokens(word, freeText, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(word,word1, word2, sep = " ")%>%
  count(word, sort =TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y=n))+
  geom_col()+
  coord_flip()+
  ggtitle("Frequent Phrases for Promoters")
# detractor bigram 

df %>%
  filter(type == "detractor") %>%
  unnest_tokens(word, freeText, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(word,word1, word2, sep = " ")%>%
  count(word, sort =TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y=n))+
  geom_col()+
  coord_flip()+
  ggtitle("Frequent Phrases for Detractors")


###########################################REMOVING COLUMN OF FREE TEXT###########################################################################

df <- df[,-32]

##########################################LINEAR PLOTS####################################################################

#A
#model<- lm(formula =  Likelihood.to.recommend ~ Loyalty, data= df)
#summary(model)


#plot(model1)

#model2<- lm(formula = Likelihood.to.recommend ~ Flights.Per.Year, data= df)

#plot(df$Arrival.Delay.in.Minutes, df$Flight.Distance)
#abline(model1)

#plot(df$Flights.Per.Year, df$Likelihood.to.recommend)
#abline(model2)

#plot1<- ggplot(df, aes(y=Arrival.Delay.in.Minutes, x= Flight.Distance))
#plot1<- plot1+ geom_point(aes(colour = factor(type)))+ stat_smooth(method = "lm", color= "yellow")+ ggtitle("Linear Model : Arrival Delay In Minutes Vs Flight Distance")
#plot1

plot1<- ggplot(df, aes(y = Loyalty, x= Total.Freq.Flyer.Accts ))
plot1<- plot1+ geom_point(aes(colour = factor(type))) + geom_smooth(method = "lm", color= "black")+ ggtitle("Linear Model : Total Frequent Flyer Accounts Vs Loyalty")
plot1

plot2<- ggplot(df, aes(y = Likelihood.to.recommend, x= Flights.Per.Year))
plot2<- plot2+ geom_point(aes(colour = factor(type))) + stat_smooth(method = "lm", color= "black")+ ggtitle("Linear Model : Flights Per Year Vs Likelihood to Recommend")
plot2

plot3<- ggplot(df, aes(y = Likelihood.to.recommend, x= Loyalty))
plot3<- plot3+ geom_point(aes(colour = factor(type))) + stat_smooth(method = "lm", color= "black")+ ggtitle("Linear Model : Loyalty Vs Likelihood to Recommend")
plot3

plot4<- ggplot(df, aes(y = Likelihood.to.recommend , x= Total.Freq.Flyer.Accts))
plot4<- plot4+ geom_point(aes(colour = factor(type))) +  geom_smooth(method = "lm", color= "black")+ ggtitle("Linear Model : Total Frequent Flyer Accounts Vs Likelihood to Recommend")
plot4

plot5<- ggplot(df, aes(y = Likelihood.to.recommend , x= ArrivalDelayRatio))
plot5<- plot5+ geom_point(aes(colour = factor(type))) +  geom_smooth(method = "lm", color= "black")+ ggtitle("Linear Model : Arrival Delay Ratio Vs Likelihood to Recommend")
plot5

plot6<- ggplot(df, aes(y = Likelihood.to.recommend , x= DepartureDelayRatio))
plot6<- plot6+ geom_point(aes(colour = factor(type))) +  geom_smooth(method = "lm", color= "black")+ ggtitle("Linear Model : Departure Delay Ratios Vs Likelihood to Recommend")
plot6


###########################################LINEAR MODELS######################################################3

model1<- lm(formula = Likelihood.to.recommend ~ Age + Price.Sensitivity + Flights.Per.Year, data= df)
summary(model1)

model2<- lm(formula = Likelihood.to.recommend ~ Loyalty + Price.Sensitivity + Flights.Per.Year, data= df)
summary(model2)

model3<- lm(formula = Likelihood.to.recommend ~ Loyalty + Total.Freq.Flyer.Accts + Flights.Per.Year, data= df)
summary(model3)

model4<- lm(formula = Likelihood.to.recommend ~ Loyalty + Total.Freq.Flyer.Accts + Age, data= df)
summary(model4)

model5<- lm(formula = Likelihood.to.recommend ~ Age + Price.Sensitivity + Total.Freq.Flyer.Accts, data= df)
summary(model5)

model6<- lm(formula = Likelihood.to.recommend ~ ArrivalDelayRatio + DepartureDelayRatio, data= df)
summary(model6)

model7<- lm(formula = Likelihood.to.recommend ~ Loyalty + Total.Freq.Flyer.Accts + Age + Price.Sensitivity + Flights.Per.Year + ArrivalDelayRatio+ DepartureDelayRatio , data= df)
summary(model7)



#######################################################################################
df %>%
  filter(df$type == 'promoter') %>%
  summarise( avg_flights = mean(Flights.Per.Year))
  
  



df %>% 
  group_by(Class) %>%
  summarise(mean_nps = median(Likelihood.to.recommend))




dfLowNPS <- df %>%
  filter(Likelihood.to.recommend < 7) %>%        
  group_by(Partner.Name) %>%
  summarise(LowNPS = n())

dfHighNPS<- df %>%
  filter(Likelihood.to.recommend > 8) %>%        
  group_by(Partner.Name) %>%
  summarise(HighNPS = n())

dfTotalNPS <- df %>%
  group_by(Partner.Name) %>%
  summarise(TotalEntries = n())

lowhighmerge <- merge(dfLowNPS,dfHighNPS, by = "Partner.Name")

partnerdf <- merge(lowhighmerge,dfTotalNPS, by = "Partner.Name")

partnerdf$percentageLowNPS <- round((partnerdf$LowNPS / partnerdf$TotalEntries) * 100,2)
partnerdf$percentageHighNPS <- round((partnerdf$HighNPS / partnerdf$TotalEntries) * 100,2)
partnerdf$NPS <- partnerdf$percentageHighNPS - partnerdf$percentageLowNPS
partnerdf

dflowTypeclassgender<- df %>%
  filter(Likelihood.to.recommend < 7) %>%        
  group_by(Type.of.Travel,Class,Gender) %>%
  summarise(LowNPS = n())

dfhighTypeclassgender<- df %>%
  filter(Likelihood.to.recommend > 8) %>%        
  group_by(Type.of.Travel,Class,Gender) %>%
  summarise(HighNPS = n())

dfTotalTypeclassgender<- df %>%
  group_by(Type.of.Travel,Class,Gender) %>%
  summarise(TotalEntries = n())

lowhighTypeclassgenderhmerge <- merge(dflowTypeclassgender,dfhighTypeclassgender, by = c("Type.of.Travel","Class","Gender"))
Typeclassgenderdf <- merge(lowhighTypeclassgenderhmerge,dfTotalTypeclassgender, by = c("Type.of.Travel","Class","Gender"))

Typeclassgenderdf$percentageLowNPS <- round((Typeclassgenderdf$LowNPS / Typeclassgenderdf$TotalEntries) * 100,2)
Typeclassgenderdf$percentageHighNPS <- round((Typeclassgenderdf$HighNPS / Typeclassgenderdf$TotalEntries) * 100,2)
Typeclassgenderdf$NPS <- Typeclassgenderdf$percentageHighNPS - Typeclassgenderdf$percentageLowNPS

Typeclassgenderdf[Typeclassgenderdf$NPS < 0 , ]


partnerdf

agerange(df)

#########################BAR PLOTS#####################################################




### Partner Name VS NPS
partner_compare <- df %>% group_by(Partner.Name)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for partner airlines nps score

partner_plot <- partner_compare %>% ggplot(aes(x = Partner.Name, y = NPS,fill = NPS)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("The NPS of Partner Airline Companies")+
  geom_col()
partner_compare
partner_plot
### Gender VS NPS
gender_compare <- df %>% group_by(Gender)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for Gender nps score

gender_plot <- gender_compare %>% ggplot(aes(x = Gender, y = NPS, fill = NPS)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("The NPS of Male and Female")+
  geom_col()
gender_plot
### Class VS NPS
class_compare <- df %>% group_by(Class)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for class nps score

class_plot <- class_compare %>% ggplot(aes(x = Class, y = NPS, fill = NPS)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("The NPS of Different Class")+
  geom_col()
class_plot
### Price Sensitivity vs NPS
price_compare <- df %>% group_by(Price.Sensitivity)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for price sensitivity nps score

price_plot <- price_compare %>% ggplot(aes(x = Price.Sensitivity, y = NPS, fill = NPS)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("The NPS of Different Price.Sensitivity")+
  geom_col()
price_plot
### Airline Status vs NPS
status_compare <- df %>% group_by(Airline.Status)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for airline status nps score

status_plot <- status_compare %>% ggplot(aes(x = Airline.Status, y = NPS, fill = NPS)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("The NPS of Different Airline Status")+
  geom_col()
status_plot
### Age vs NPS
age_compare <- df %>% group_by(agegroup)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for travel nps score

age_plot <- age_compare %>% ggplot(aes(x = agegroup , y = NPS,fill = NPS)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("The NPS of Age Group")+
  geom_col()
age_plot
### Type of Travel vs NPS
travel_compare <- df %>% group_by(Type.of.Travel)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
# Plot for travel nps score

travel_plot <- travel_compare %>% ggplot(aes(x = Type.of.Travel , y = NPS,fill = NPS)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("The NPS of Type of Travel")+
  geom_col()
travel_plot



#####################ASSOCIATIVE RULE MINING###########################################################

subset_df <- df[c('Destination.City', 'Origin.City','Airline.Status', 'Gender', 'Type.of.Travel', 'Class', 'Partner.Name', 'Flight.cancelled', 'type', 'agegroup', 'Day.of.Flight', 'spend')]
View(subset_df)

library(arules)
library(arulesViz)

subsetX_df<- as(subset_df, "transactions")
arules::inspect(subsetX_df)

#itemFrequency(subsetX_df)

#View(subsetX_df)

#ALL THE RULES FOR DETRACTORS#

ruleset_subsetX_df<- apriori(subsetX_df,
                  parameter= list(support= 0.1, confidence = 0.5),
                  appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset_subsetX_df)

plot(ruleset_subsetX_df)

############### 1 : PROMOTERS AND DETRACTORS WITH : AIRLINE STATUS | TYPE OF TRAVEL| CLASS  #####################################################################################

subset1_df<- df[c('Airline.Status','Type.of.Travel', 'Class','type')]

subset1X_df<- as(subset1_df, "transactions")

# DETRACTOR #

ruleset1_subsetX_df<- apriori(subset1X_df,
                             parameter= list(support= 0.01, confidence = 0.05),
                             appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset1_subsetX_df)

goodRules<- ruleset1_subsetX_df[quality(ruleset1_subsetX_df)$lift>2] 

arules::inspect(goodRules)

# PLOT #

d_AirlineStatus_TypeOfTravel_Class<- plot(goodRules, method="paracoord", control=list(reorder=TRUE))

# PROMOTER #

ruleset2_subsetX_df<- apriori(subset1X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset2_subsetX_df)


goodRules2<- ruleset2_subsetX_df[quality(ruleset2_subsetX_df)$lift>1.3]

arules::inspect(goodRules2)

#PLOT

p_AirlineStatus_TypeOfTravel_Class<- plot(goodRules2, method="paracoord", control=list(reorder=TRUE))

############### 2 : PROMOTERS AND DETRACTORS WITH : PARTNER NAME | GENDER | AGEGROUP  #####################################################################################

subset2_df<- df[c('Gender','Partner.Name', 'agegroup','type')]

subset2X_df<- as(subset2_df, "transactions")

# DETRACTOR #

ruleset3_subsetX_df<- apriori(subset2X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset3_subsetX_df)

goodRules3<- ruleset3_subsetX_df[quality(ruleset3_subsetX_df)$lift>1.5]

arules::inspect(goodRules3)

# PLOT #

plot(goodRules3, method="paracoord", control=list(reorder=TRUE))

# PROMOTER #

ruleset4_subsetX_df<- apriori(subset2X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset4_subsetX_df)

goodRules4<- ruleset4_subsetX_df[quality(ruleset4_subsetX_df)$lift>1.3]

arules::inspect(goodRules4)

# PLOT #

plot(goodRules4, method="paracoord", control=list(reorder=TRUE))

############### 3 : PROMOTERS AND DETRACTORS WITH : TYPE OF TRAVEL | FLIGHT CANCELLED | DAY OF FLIGHT  #############################

subset3_df<- df[c('Type.of.Travel','Flight.cancelled', 'Day.of.Flight','type')]

subset3X_df<- as(subset3_df, "transactions")

# DETRACTOR #

ruleset5_subsetX_df<- apriori(subset3X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset5_subsetX_df)

goodRules5<- ruleset5_subsetX_df[quality(ruleset5_subsetX_df)$lift>1.0]

arules::inspect(goodRules5)

# PLOT #

plot(goodRules5, method="paracoord", control=list(reorder=TRUE))

# PROMOTER #

ruleset6_subsetX_df<- apriori(subset3X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset6_subsetX_df)

goodRules6<- ruleset6_subsetX_df[quality(ruleset6_subsetX_df)$lift>1.3]

arules::inspect(goodRules6)

# PLOT #

plot(goodRules6, method="paracoord", control=list(reorder=TRUE))

colnames(subset_df)

############### 4 : PROMOTERS AND DETRACTORS WITH : TYPE OF TRAVEL | FLIGHT CANCELLED | CLASS  #############################

subset4_df<- df[c('Type.of.Travel','Flight.cancelled', 'Class','type')]

subset4X_df<- as(subset4_df, "transactions")

# DETRACTOR #

ruleset7_subsetX_df<- apriori(subset4X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset7_subsetX_df)

goodRules7<- ruleset7_subsetX_df[quality(ruleset7_subsetX_df)$lift>1.5]

arules::inspect(goodRules7)

# PLOT #

plot(goodRules7, method="paracoord", control=list(reorder=TRUE))

# PROMOTER #

ruleset8_subsetX_df<- apriori(subset4X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset8_subsetX_df)

goodRules8<- ruleset8_subsetX_df[quality(ruleset8_subsetX_df)$lift>1.0]

arules::inspect(goodRules8)

# PLOT #

plot(goodRules8, method="paracoord", control=list(reorder=TRUE))

#df[df$Flight.cancelled == 'yes',]
############### 5 : PROMOTERS AND DETRACTORS WITH : TYPE OF TRAVEL | DAY OF FLIGHT | CLASS  #############################

subset5_df<- df[c('Type.of.Travel','Day.of.Flight', 'Class','type')]

subset5X_df<- as(subset5_df, "transactions")

# DETRACTOR #

ruleset9_subsetX_df<- apriori(subset5X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset9_subsetX_df)

goodRules9<- ruleset9_subsetX_df[quality(ruleset9_subsetX_df)$lift>1.5]

arules::inspect(goodRules9)

# PLOT #

plot(goodRules9, method="paracoord", control=list(reorder=TRUE))

# PROMOTER #

ruleset10_subsetX_df<- apriori(subset5X_df,
                              parameter= list(support= 0.01, confidence = 0.05),
                              appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset10_subsetX_df)

goodRules10<- ruleset10_subsetX_df[quality(ruleset10_subsetX_df)$lift>1.3]

arules::inspect(goodRules10)

# PLOT #

plot(goodRules10, method="paracoord", control=list(reorder=TRUE))



#########CHICAGO - LA##############33

Chicago_LosAngeles <- subset(subset_df, Origin.City== "Chicago" & Destination.City== "Los Angeles")
View(Chicago_LosAngeles)

Chicago_LosAngelesX<- as(Chicago_LosAngeles, "transactions")

# DETRACTOR #

ruleset_Chicago_LosAngelesX_df<- apriori(Chicago_LosAngelesX,
                              parameter= list(support= 0.3, confidence = 0.5),
                              appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset_Chicago_LosAngelesX_df)

goodRules<- ruleset_Chicago_LosAngelesX_df[quality(ruleset_Chicago_LosAngelesX_df)$lift>2]

arules::inspect(goodRules)

plot(goodRules, method="paracoord", control=list(reorder=TRUE))

###############################Flyfast and Northwest############################################################

flyfast <- subset(subset_df, Partner.Name == "FlyFast Airways Inc.")
#View(flyfast)

flyfastX<- as(flyfast, "transactions")

ruleset_flyfastX_df<- apriori(flyfastX,
                                         parameter= list(support= 0.15, confidence = 0.8),
                                         appearance = list(default = "lhs", rhs=("type=detractor")))

arules::inspect(ruleset_flyfastX_df)

flyfast_detractor<- plot(ruleset_flyfastX_df, method="paracoord", control=list(reorder=TRUE))

flyfast_detractor



northwest<- subset(subset_df, Partner.Name == "Northwest Business Airlines Inc.")
View(northwest)
northwestX<- as(northwest, "transactions")

ruleset_northwestX_df<- apriori(northwestX,
                                parameter= list(support= 0.3, confidence = 0.55),
                                appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset_northwestX_df)

plot(ruleset_northwestX_df, method="paracoord", control=list(reorder=TRUE))



subsetNorthwest <- northwest[c('Class', 'Type.of.Travel', 'Airline.Status', 'Partner.Name', 'type')]

subsetnorthwestX<- as(subsetNorthwest, "transactions")

ruleset_subsetnorthwestX_df<- apriori(subsetnorthwestX,
                                parameter= list(support= 0.1, confidence = 0.2),
                                appearance = list(default = "lhs", rhs=("type=promoter")))

arules::inspect(ruleset_subsetnorthwestX_df)

northwest_promoter<- plot(ruleset_subsetnorthwestX_df, method="paracoord", control=list(reorder=TRUE))

######################## INTERSTATE INTRASTATE ################################

df_intrastate <- df %>% group_by(Origin.State)%>%
  filter(Destination.State == Origin.State) %>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS_Intra = (promoter_count/n - detractor_count/n)*100) %>%
  filter(n > 5) %>%
  select(Origin.State, NPS_Intra)

df_interstate <- df %>% group_by(Origin.State)%>%
  filter(Destination.State != Origin.State) %>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS_Inter = (promoter_count/n - detractor_count/n)*100) %>%
  filter(n > 5) %>%
  select(Origin.State, NPS_Inter)

inter_intra_plot <- df_intrastate %>%
  inner_join(df_interstate, by = "Origin.State") %>%   
  gather(`NPS_Intra`, `NPS_Inter`, key = "type", value = "NPS") %>%
  ggplot(aes(x = Origin.State, y = NPS, fill=type))+
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

inter_intra_plot

######################### MAP ##########################

# loading states dataframe for ggplot
states <- map_data("state")
### Best 5 and Worst 5 in terms of destination city
# Grouping by destination city and then calculating NPS value for each city. 
# Only considering those cities which had more than 25 flights flying in the dataset.
# DESTINATION CITY PLOT

df_city_des <- df %>% group_by(Destination.City, dlong, dlat)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100
  ) %>%
  mutate(NPS_type = cut(NPS, 
                        breaks = c(-Inf,0,Inf),
                        labels = c("Detractor","Promoter"))) %>%
  filter(n > 25) %>%
  arrange(NPS)
# Selecting only best 5 and worst five cities
df_city_des <- df_city_des[-6:-51,]
# Plot
dest_city_plot <- ggplot() +
  geom_polygon(data = states, color="black", fill= "#edf8b1",
               aes(x=long,y=lat, group=group)) +
  geom_point(data = df_city_des, aes(x = dlong, y =  dlat, colour = factor(NPS_type),shape = factor(NPS_type)),size =8,alpha = 0.8) + ggtitle("Destination City Top 5 Promoter & Detractor Cities") + theme(axis.title.x = element_blank(), axis.title.y = element_blank())+ labs(colour = "NPS", shape = "NPS") + coord_map() + geom_text(data = df_city_des,aes(x = dlong, y = dlat, label = Destination.City, group = NULL), size = 5)

dest_city_plot
### Best 5 and Worst 5 in terms of Origin city
# Grouping by origin city and then calculating NPS value for each city. 
# Only considering those cities which had more than 25 flights flying in the dataset.
# ORIGIN CITY PLOT

df_city_arr <- df %>% group_by(Origin.City, olong, olat)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100
  ) %>%
  mutate(NPS_type = cut(NPS, 
                        breaks = c(-Inf,0,Inf),
                        labels = c("Detractor","Promoter"))) %>%
  filter(n > 25) %>%
  arrange(NPS)



# Selecting only best 5 and worst five cities
df_city_arr <- df_city_arr[-6:-51,]
# PLOT
ori_city_plot <- ggplot() +
  geom_polygon(data = states, color="black", fill= "#edf8b1",
               aes(x=long,y=lat, group=group)) +
  geom_point(data = df_city_arr, aes(x = olong, y =  olat, colour = factor(NPS_type),shape = factor(NPS_type)),size =8,alpha = 0.8) + ggtitle("Origin City Top 5 Promoter & Detractor Cities") + theme(axis.title.x = element_blank(), axis.title.y = element_blank())+ labs(colour = "NPS", shape = "NPS") + coord_map() + geom_text(data = df_city_arr,aes(x = olong, y = olat, label = Origin.City, group = NULL), size = 5)

ori_city_plot
### Best 5 and Worst 5 in terms of Route
df_route <- df %>% group_by(Origin.City, Destination.City, olong, olat, dlong, dlat)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100) %>%
  mutate(NPS_type = cut(NPS, 
                        breaks = c(-Inf,0,Inf),
                        labels = c("Detractor","Promoter"))) %>%
  filter(n>25)%>%
  arrange(NPS)
# Selecting only best 5 and worst five cities
df_route <- df_route[-6:-25,]
# Storing as dataframe
df_route <- as.data.frame(df_route)
# Route plot
route_plot<- ggplot() + geom_polygon(data = states, color="black", fill= "#edf8b1",aes(x=long,y=lat, group=group)) + geom_curve(data = df_route ,aes(x = olong, y = olat, xend = dlong, yend = dlat, colour = factor(NPS_type)), arrow = arrow(angle = 15, length = unit(0.5, "cm"), type = "closed"), size = 1.1, alpha = 0.8, curvature = 0.15, inherit.aes = TRUE) +  coord_map() + coord_cartesian() + ggtitle("Top 5 and Worst 5 routes") + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + geom_text(data = df_route,aes(x = olong, y = olat, label = Origin.City, group = NULL, check_overlap = TRUE), size = 5) + geom_text(data = df_route,aes(x = dlong, y = dlat, label = Destination.City, group = NULL, check_overlap = TRUE), size = 5) + labs(colour = "NPS", shape = "NPS")
route_plot
states <- map_data("state")
df_states_dest <- df %>% group_by(Destination.State)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100
  )%>%
  mutate(region=tolower(Destination.State))%>%
  inner_join(states, by = "region")
df_states_dest_plot <- df_states_dest %>%
  mutate(Destination.State=tolower(Destination.State))%>%
  mutate(NPS_type = cut(NPS, 
                        breaks = c(-Inf,0,Inf),
                        labels = c("Detractor","Promoter"))) %>%
  ggplot(aes(map_id = Destination.State, label = Destination.State)) + geom_map(map = states, aes(fill = NPS),color = "white")+ expand_limits(x = states$long, y = states$lat)+ coord_map() + ggtitle('Destination State NPS')
df_states_dest_plot
df %>% group_by(Destination.State)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100) %>%
  arrange(n)
df %>% group_by(Origin.State)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100) %>%
  arrange(n) 
df_states_ori <- df %>% group_by(Origin.State)%>%
  summarise(n=n(),promoter_count=length(type[type == "promoter"]),
            detractor_count=length(type[type == "detractor"]),
            NPS = (promoter_count/n - detractor_count/n)*100)
df_states_ori_plot <- df_states_ori %>%
  mutate(Origin.State=tolower(Origin.State))%>%
  mutate(NPS_type = cut(NPS, 
                        breaks = c(-Inf,0,Inf),
                        labels = c("Detractor","Promoter"))) %>%
  ggplot(aes(map_id = Origin.State, label = Origin.State)) + geom_map(map = states, aes(fill = NPS),color = "white")+ expand_limits(x = states$long, y = states$lat)+ coord_map() + ggtitle('Origin State NPS')
df_states_ori_plot



##########################################LOGISTIC REGRESSION#########################################################


# remove the passive customers from the dataframe

df_classification <- df %>% 
  # only include the promoter&detractor
  filter(type != 'passive')%>%
  mutate(prob = factor(ifelse(type == "detractor", 1, 0), levels = c(0,1)))
# check if there is still any n.a
colSums(is.na(df_classification)) > 0
### 2. Create train set and test set
library(caret)
# create an index
set.seed(100) # set the seed for random number
index <- sample(1:6961, 6961) # generate a list of random number
# separate the data into train and test set
trainList <- createDataPartition(index,p=.8,list=FALSE)
train <- df_classification[trainList, ]
test <- df_classification[-trainList, ]
### 3. Train a Multi-Regression Model
model1 <- glm(prob ~ Airline.Status + Type.of.Travel + Class,
              family=binomial,
              data=train)
summary(model1)
### 4. Evaluate the model with the test data
# predict
pred1 <- predict(model1, newdata = test, type = "response")
# test the accuracy 

y_pred_num <- ifelse(pred1 > 0.5, 1, 0) 
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$prob
# Confusion Matrix
table(y_pred, y_act)
# accuracy rate
mean(y_pred == y_act)
### 5. Plotting the single logistic regression
#### DepartureDelayRatio vs probability of being a detractor
df_classification %>%
  ggplot(aes(DepartureDelayRatio, (as.numeric(prob)-1))) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("DepartureDelayRatio") +
  ylab("Probability of Detractor")
#### DepartureDelayRatio & gender vs probability of being a detractor
df_classification %>%
  ggplot(aes(x = DepartureDelayRatio, y= (as.numeric(prob)-1), color = Gender)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("DepartureDelayRatio") +
  ylab("Probability of Detractor")
#### ArrivalDelayRatio vs probability of being a detractor
df_classification %>%
  ggplot(aes(ArrivalDelayRatio, (as.numeric(prob)-1))) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("ArrivalDelayRatio") +
  ylab("Probability of Detractor")
#### ArrivalDelayRatio & gender vs probability of being a detractor
df_classification %>%
  ggplot(aes(x = ArrivalDelayRatio, y= (as.numeric(prob)-1), color = Gender)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("ArrivalDelayRatio") +
  ylab("Probability of Detractor")
#### Eating.and.Drinking.at.Airport vs probability of being a detractor
df_classification %>%
  ggplot(aes(Eating.and.Drinking.at.Airport, (as.numeric(prob)-1))) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Eating.and.Drinking.at.Airport") +
  ylab("Probability of Detractor")
#### Loyalty vs probability of being a detractor
df_classification %>%
  ggplot(aes(Loyalty, (as.numeric(prob)-1))) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Loyalty") +
  ylab("Probability of Detractor")
model_loyalty <- glm(prob ~ Loyalty,
                     family=binomial,
                     data=df_classification)
summary(model_loyalty)
#### Loyalty & Gender vs probability of being a detractor
df_classification %>%
  ggplot(aes(x = Loyalty, y= (as.numeric(prob)-1), color = Gender)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Loyalty") +
  ylab("Probability of Detractor")
model_loyalty_gender <- glm(prob ~ Loyalty+ Gender,
                            family=binomial,
                            data=df_classification)
summary(model_loyalty_gender)
#### Loyalty & Airline.Status vs probability of being a detractor
df_classification %>%
  ggplot(aes(x = Loyalty, y= (as.numeric(prob)-1), color = Airline.Status)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Loyalty") +
  ylab("Probability of Detractor")






