PATH="~/Documents/Education/Rockhurst/ADM/"

packages_to_be_loaded=c("corrplot","leaps","dummies","leaps","forecast","gains","reshape")

lapply(packages_to_be_loaded,function(x){
  if(x%in%installed.packages()[,1]==F){ install.packages(x)}
  require(x,character.only = T)
})

# Part 1

## (a) Predict the number of fatalities that will result from a given accident

### Step 1: Develop an understanding of the data mining project

Can we create a model to predict the number of fatalities that will result from a given accident?
  
### Step 2: Obtain the dataset to be used in the analysis

The US department of transportation provided number of fatalities and 52 other variables for 5,000 fatal car crashes in 2016.

### Step 3: Explore, clean, and preprocess the data

Accident.df <- read.csv(file.path(PATH,"fatalities_data_accident.csv"), header=TRUE)

dim(Accident.df)
head(Accident.df)
str(Accident.df)
View(Accident.df)
names(Accident.df)
summary(Accident.df)
heatmap(1*is.na(Accident.df),Rowv=NA,Colv=NA) #heatmap to check for missing values
hist(Accident.df$number_of_fatalities) #outcome variable is skewed right

#'X', 'consecutive_number', 'land_use','national_highway_system', and 'gid' were removed since they do not provide useful meaning in this context
#'year_of_crash' removed since all observations are in 2016
# all time variables containing hour or minute were removed since observations contain records where time of crash is later than time of notification and/or arrival = unreliable data
#'atmospheric_conditions_1' and 'atmospheric_conditions_2' were removed as analytical user guide appendix B explains how 'atmospheric_conditions' combines these variables
#'functional_system'and 'ownership' were removed as 'route_signing' will be used to categorize much of the same information / overlap
#'relation_to_junction_within_interchange_area', 'type_of_intersection', and 'rail_grade_crossing_identifier' were removed since much of the same information is captured in the 'relation_to_junction_specific_location'
#'first_harmful_event' was removed due to overlap with variables kept in the model as well as large number of factors

vehicles<-(Accident.df$number_of_vehicle_forms_submitted_all - Accident.df$number_of_motor_vehicles_in_transport_mvit - Accident.df$number_of_parked_working_vehicles)
summary(vehicles)

#'number_of_motor_vehicles_in_transport_mvit' and 'number_of_parked_working_vehicles' removed since this information is included in 'number_of_vehicle_forms_submitted_all'.

#'county','city','day_of_crash', 'month_of_crash','trafficway_identifier', and'trafficway_identifier_2'  removed due to having large numbers of irreducable levels - trafficway ID's contain overlapping information with route_signing according to analytical user's guide in Appendix C
#'latitude','longitude' removed since it does not provide any meaning for predicting number of fatalities (increase or decrease in this predictor value is nonsensical in relation to outcome variable since there are no additional reference coordinates other than site of crash)

Accident_eda.df<-Accident.df[,c(50,2,4,7:10,16,23,29,31,33,35:37,40:41,47:49,51)] #bring number of accidents to front & remove irrelevant variables
names(Accident_eda.df)
str(Accident_eda.df)

# convert categorical variables to factors
Accident_eda.df$route_signing<-as.factor(Accident_eda.df$route_signing)
Accident_eda.df$special_jurisdiction<-as.factor(Accident_eda.df$special_jurisdiction)
Accident_eda.df$manner_of_collision<-as.factor(Accident_eda.df$manner_of_collision)
Accident_eda.df$relation_to_junction_specific_location<-as.factor(Accident_eda.df$relation_to_junction_specific_location)
Accident_eda.df$relation_to_trafficway<-as.factor(Accident_eda.df$relation_to_trafficway)
Accident_eda.df$light_condition<-as.factor(Accident_eda.df$light_condition)
Accident_eda.df$atmospheric_conditions<-as.factor(Accident_eda.df$atmospheric_conditions)
Accident_eda.df$related_factors_crash_level_1<-as.factor(Accident_eda.df$related_factors_crash_level_1)
Accident_eda.df$related_factors_crash_level_2<-as.factor(Accident_eda.df$related_factors_crash_level_2)
Accident_eda.df$related_factors_crash_level_3<-as.factor(Accident_eda.df$related_factors_crash_level_3)

#### Dummies & transforming variables

#'state_number' converted to 'region' variable to reduce levels from 50 to 4
#1=Northeast(PA,NJ,NY,NH,VT,RI,MA,ME,CT), 2=Midwest(OH,IN,IL,MI,WI,MN,ND,SD,NE,IA,MO,KS), 3=South(MD,DE,DC,WV,VA,KY,TN,NC,SC,GA,FL,AL,MS,LA,AR,OK,TX), 4=West(MT,ID,WA,OR,CA,NV,NM,AZ,UT,CO,WY,AK,HI)

Accident_eda.df$region<-ifelse(Accident_eda.df$state_number %in% c(42,50,25,9,23,34,36,33,44),3,Accident_eda.df$state_number)
Accident_eda.df$region<-ifelse(Accident_eda.df$region %in% c(29,17,55,39,31,19,26,20,27,38,18,46),7,Accident_eda.df$region)
Accident_eda.df$region<-ifelse(Accident_eda.df$region %in% c(1,5,10,11,12,13,21,22,24,28,37,40,45,47,48,51,54),9,Accident_eda.df$region)
Accident_eda.df$region<-ifelse(Accident_eda.df$region %in% c(2,4,6,8,15,16,30,32,35,41,49,53,56),5,Accident_eda.df$region)
Accident_eda.df$region<-ifelse(Accident_eda.df$region == 3,1,Accident_eda.df$region)
Accident_eda.df$region<-ifelse(Accident_eda.df$region == 7,2,Accident_eda.df$region)
Accident_eda.df$region<-ifelse(Accident_eda.df$region == 9,3,Accident_eda.df$region)
Accident_eda.df$region<-ifelse(Accident_eda.df$region == 5,4,Accident_eda.df$region)
Accident_eda.df$region<-as.factor(Accident_eda.df$region)
str(Accident_eda.df$region)

#remove 'state_number' as it was replaced by 'region'
Accident_eda.df<-Accident_eda.df[,c(-2)]

#'day_of_week' converted to dummy variable called 'weekday' with values of 0 (accident not on a weekday) and 1 (accident on a weekday)

Accident_eda.df$day_of_week <- ifelse(Accident.df$day_of_week == 1, 8, Accident.df$day_of_week)
Accident_eda.df$weekday<-ifelse(Accident_eda.df$day_of_week < 7, 1,0)
Accident_eda.df$weekday<-as.factor(Accident_eda.df$weekday)

#remove 'day_of_week' as it was replaced by 'weekday'
Accident_eda.df<-Accident_eda.df[,c(-7)]

#'atmospheric_conditions' converted to dummy variable called 'adverse_weather' with values of 0 (none) and 1 (rain,snow,or other adverse conditions)

Accident_eda.df$atmospheric_conditions<-as.integer(Accident_eda.df$atmospheric_conditions)
summary(Accident_eda.df$adverse_weather)
Accident_eda.df$adverse_weather <- ifelse(Accident_eda.df$atmospheric_conditions == 1,100,Accident_eda.df$atmospheric_conditions)
Accident_eda.df$adverse_weather <- ifelse(Accident_eda.df$adverse_weather < 98,101,Accident_eda.df$adverse_weather)
Accident_eda.df$adverse_weather <- ifelse(Accident_eda.df$adverse_weather < 100,9,Accident_eda.df$adverse_weather)
Accident_eda.df$adverse_weather <- ifelse(Accident_eda.df$adverse_weather == 100,0,Accident_eda.df$adverse_weather)
Accident_eda.df$adverse_weather <- ifelse(Accident_eda.df$adverse_weather == 101,1,Accident_eda.df$adverse_weather)
Accident_eda.df$adverse_weather <- as.factor(Accident_eda.df$adverse_weather)

#remove 'atmospheric_conditions' as it was replaced by 'adverse_weather'
Accident_eda.df<-Accident_eda.df[,c(-14)]

#'light_conditions' converted to dummy variable with same name containing values of 0 (Unknown), 1 (Daylight), 2 (Dark including dawn/dusk), 3 (Dark but lighted)
Accident_eda.df$light_condition<-as.integer(Accident_eda.df$light_condition)
Accident_eda.df$light_condition <- ifelse(Accident_eda.df$light_condition > 6,0,Accident_eda.df$light_condition)
Accident_eda.df$light_condition <- ifelse(Accident_eda.df$light_condition %in% c(2,4,5,6),2,Accident_eda.df$light_condition)
Accident_eda.df$light_condition<-as.factor(Accident_eda.df$light_condition)

#'manner_of_collision' converted to dummy variable with same name containing values of 0 (No Collision), 1 (Head-on Collision), 2 (Other form of Collision)
Accident_eda.df$manner_of_collision<-as.integer(Accident_eda.df$manner_of_collision)
hist(Accident_eda.df$manner_of_collision)
Accident_eda.df$manner_of_collision<-ifelse(Accident_eda.df$manner_of_collision == 2, 3, Accident_eda.df$manner_of_collision)
Accident_eda.df$manner_of_collision<-ifelse(Accident_eda.df$manner_of_collision == 1, 2, Accident_eda.df$manner_of_collision)
Accident_eda.df$manner_of_collision<-ifelse(Accident_eda.df$manner_of_collision == 3, 1, Accident_eda.df$manner_of_collision)
Accident_eda.df$manner_of_collision<-ifelse(Accident_eda.df$manner_of_collision > 3, 2, Accident_eda.df$manner_of_collision)
Accident_eda.df$manner_of_collision<-as.factor(Accident_eda.df$manner_of_collision)

#'relation_junctloc' converted to dummy variable named 'inter_sect_chang' containing values of 0 (Did not occur at intersection or interchange), 1 (Occurred at intersection or interchange)
Accident_eda.df$relation_junctloc<-as.integer(Accident_eda.df$relation_junctloc)
hist(Accident_eda.df$relation_junctloc)
Accident_eda.df$inter_sect_chang<-ifelse(Accident_eda.df$relation_junctloc == 1, 0, Accident_eda.df$relation_junctloc)
Accident_eda.df$inter_sect_chang<-ifelse(Accident_eda.df$inter_sect_chang > 0, 1, Accident_eda.df$inter_sect_chang)
Accident_eda.df$inter_sect_chang<-as.factor(Accident_eda.df$inter_sect_chang)

#remove 'relation_junctloc' as it was replaced by 'inter_sect_chang'
Accident_eda.df<-Accident_eda.df[,c(-10)]

# attempt at 'work_zone' converted to dummy variable with same name containing values of 0 (Not a Workzone), 1 (Workzone)
Accident_eda.df$work_zone<-as.integer(Accident_eda.df$work_zone)
hist(Accident_eda.df$work_zone)
summary(Accident_eda.df$work_zone)
#remove'work_zone' instead of converting to dummy as all observations were within a work zone providing zero variance so removing this variable
Accident_eda.df<-Accident_eda.df[,c(-10)]

#'relation_to_trafficway' converted to dummy variable with same name containing values of 0 (Off Roadway), 1 (On Roadway)
Accident_eda.df$relation_to_trafficway<-as.integer(Accident_eda.df$relation_to_trafficway)
Accident_eda.df$relation_to_trafficway<-ifelse(Accident_eda.df$relation_to_trafficway > 1, 0, Accident_eda.df$relation_to_trafficway)
Accident_eda.df$relation_to_trafficway<-as.factor(Accident_eda.df$relation_to_trafficway)

#'school_bus' converted to dummy variable with same name containing values of 0 (Not bus related), 1 (Bus related)
Accident_eda.df$school_bus_related<-ifelse(Accident_eda.df$school_bus_related == "No", 0, 1)
Accident_eda.df$school_bus_related<-as.factor(Accident_eda.df$school_bus_related)

### Step 4: Reduce the data dimension

# 'route_signing' reduced from 9 to 7 levels
Accident_eda.df$route_signing<-as.integer(Accident_eda.df$route_signing)
hist(Accident_eda.df$route_signing)
Accident_eda.df$route_signing<-ifelse(Accident_eda.df$route_signing %in% c(6,7), 5, Accident_eda.df$route_signing)
Accident_eda.df$route_signing<-as.factor(Accident_eda.df$route_signing)

#rename variables with long names for corrplot view
colnames(Accident_eda.df)[2]<-"all_vehicles"
colnames(Accident_eda.df)[3]<-"non_occupants"
colnames(Accident_eda.df)[4]<-"non_motorists"
colnames(Accident_eda.df)[5]<-"motorists"
colnames(Accident_eda.df)[6]<-"occupants"
colnames(Accident_eda.df)[10]<-"relation_junctloc"
colnames(Accident_eda.df)[15]<-"factors_level1"
colnames(Accident_eda.df)[16]<-"factors_level2"
colnames(Accident_eda.df)[17]<-"factors_level3"
colnames(Accident_eda.df)[18]<-"drunk_drivers"

# zoom on correlation heatmap areas where there appear to be high correlations among predictors
numeric_var.df<-Accident_eda.df[,c(1,2:6)]
names(numeric_var.df)
str(numeric_var.df)

corrs <- cor(numeric_var.df[,c(1:6)])
corrs.matrix <- as.matrix(corrs)
corrplot(corrs.matrix, method="number")

#remove non_occupants due to high correlation with non_motorists
#remove motorists and occupants due to high correlation with all_vehicles

Accident_eda.df<-Accident_eda.df[,c(-3,-5,-6)]
names(Accident_eda.df)

#recode factors_level variables to integer to run correlation plot

Accident_eda.df$factors_level1<-as.integer(Accident_eda.df$factors_level1)
Accident_eda.df$factors_level2<-as.integer(Accident_eda.df$factors_level2)
Accident_eda.df$factors_level3<-as.integer(Accident_eda.df$factors_level3)

numeric_var.df<-Accident_eda.df[,c(1,10:12)]
names(numeric_var.df)

corrs <- cor(numeric_var.df[,c(1:4)])
corrs.matrix <- as.matrix(corrs)
corrplot(corrs.matrix, method="number")

#remove factors_level3 due to high correlation with factors_level2

Accident_eda.df<-Accident_eda.df[,c(-12)]

# recode factors_level categorical variables back to factor

Accident_eda.df$factors_level1<-as.factor(Accident_eda.df$factors_level1)
Accident_eda.df$factors_level2<-as.factor(Accident_eda.df$factors_level2)

names(Accident_eda.df)
str(Accident_eda.df)
summary(Accident_eda.df)

boxplot(Accident_eda.df$number_of_fatalities~Accident_eda.df$route_signing)
boxplot(Accident_eda.df$number_of_fatalities~Accident_eda.df$factors_level1)
boxplot(Accident_eda.df$number_of_fatalities~Accident_eda.df$factors_level2)

### Step 5: Determine the data mining task

We want to predict number_of_fatalities using the resulting predictor variables from steps 3 & 4.
Since we have a target variable, this is a supervised learning task.

### Step 6: Partition the data

We are using an 70-30 split, which is 70% of the data will be used to train our prediction model. 
The other 30% of the data will be used to "score" our prediction model. 

packages_to_be_loaded=c("class","e1071","caret","ROCR")

lapply(packages_to_be_loaded,function(x){
  if(x%in%installed.packages()[,1]==F){ install.packages(x)}
  require(x,character.only = T)
})

set.seed(123)

trainIndex <- createDataPartition(Accident_eda.df$number_of_fatalities, p = .7, 
                                  list = FALSE, 
                                  times = 1)

Accident.train <- Accident_eda.df[ trainIndex,]
Accident.valid  <- Accident_eda.df[-trainIndex,]

### Step 7: Choose the data mining techniques to be used

We will use multiple linear regression to build a prediction model as the target variable is not categorical.

### Step 8: Use algorithms to perform the task

# multiple linear regression initial model
options(scipen = 999) #ensure no scientific notation is displayed
Accident.lm <- lm(number_of_fatalities~., data=Accident.train)
summary(Accident.lm)

# stepwise selection 
stepreg <- step(Accident.lm, direction="both")
summary(stepreg)

### Step 9: Interpret the results

The resulting model from stepwise selection does a poor job at explaining variability in number of fatalities shown by the
low Adjusted R square value of 3.2%.  

summary(Accident.df$number_of_fatalities)
table<-table(Accident.df$number_of_fatalities)
table

However, it should be noted that there is very little variation in the number of fatalities within 
the given data set to begin with, where both mean and median are at or very near 1 fatality observed 
with 4,628 out of 5,000 observations resulting in 1 fatality. 
Part of the reason may be due to the data set only ocurring within one year, so a better prediction model
may be possible if more observations spread across a wider range of time were included in the data set.

### Step 10: Deploy the model

pred_v <- predict(stepreg, Accident.valid)
accuracy(pred_v, Accident.valid$number_of_fatalities)

As expected, the model didn't do too bad at predicting 
number of fatalities in the validation set but only because 92%
of the total observations result in 1 fatality! We could
simply guess that a given accident will result in only 1 fatality
and a prediction model would not be needed to be accurate most of the time
for this data set.



