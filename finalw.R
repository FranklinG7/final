library(tidyverse)
library(tidycensus)
library(sf)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

#Read Crime Data
data = read.csv("boston_crime_2018.csv")

#Read Census Data
sf = read_sf("../final/tl_2010_25025_tract10/tl_2010_25025_tract10.shp")

#Check how many observations are missing
sum(is.na(data$Lat))
sum(is.na(data$Long))

#the same observations are missing for both
all(is.na(data$Lat) == is.na(data$Long))

#remove na values
data = data %>% drop_na(Long)
data = data %>% drop_na(Lat)

#convert to sf data
map = st_as_sf(data, coords=c("Long", "Lat"), crs=4326)

#Change projection so it matches
sf = st_transform(sf, crs=4326)

#Join the data
join = st_join(sf, map)


#Create list of crime groups  
list_crime = unique(join$OFFENSE_CODE_GROUP)
view(list_crime)

#See how many crime categories are in the data
unique(join$UCR_PART)


#Seperate part 1,2,and 3 crimes 
#Part 1 crimes
part_one = join %>% 
  filter(UCR_PART == "Part One")

#Part 2 crimes 
part_two = join %>% 
  filter(UCR_PART == "Part Two")

#Part 3 crimes 
part_three = join %>% 
  filter(UCR_PART == "Part Three")


# create table for each part
part_oneg = table(part_one$OFFENSE_CODE_GROUP)
view(part_oneg)

part_twog = table(part_two$OFFENSE_CODE_GROUP)
view(part_twog)

part_threeg = table(part_three$OFFENSE_CODE_GROUP)
view(part_threeg)


#Received error that (Error: `data` must be a data frame, or other object 
#coercible by `fortify()`, not an S3 object with class table.)
#Solution: https://stackoverflow.com/questions/10758961/how-to-convert-a-table-to-a-data-frame
#Make tables into a data frame so I can graph the data
one = as.data.frame(table(part_one$OFFENSE_CODE_GROUP))
two = as.data.frame(table(part_two$OFFENSE_CODE_GROUP))
three = as.data.frame(table(part_three$OFFENSE_CODE_GROUP))


#barplots visualizing occurrence each type of crime 

ggplot(one,                                   
       aes(x = Var1,
           y = Freq)) + 
  geom_bar(stat = "identity", fill="blue") +
  labs(title="Part One Crimes", x="", y="Frequency") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=40, hjust=1))

ggplot(two,                                   
       aes(x = Var1,
           y = Freq)) + 
  geom_bar(stat = "identity", fill="blue") +
  labs(title="Part Two Crimes", x="", y="Frequency") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))

ggplot(three,                                   
       aes(x = Var1,
           y = Freq)) + 
  geom_bar(stat = "identity", fill="blue") +
  labs(title="Part Three Crimes", x="", y="Frequency") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))


#Make table into data frame and add day of the week
one_day = as.data.frame(table(part_one$OFFENSE_CODE_GROUP, part_one$DAY_OF_WEEK))
two_day = as.data.frame(table(part_two$OFFENSE_CODE_GROUP, part_two$DAY_OF_WEEK))
three_day = as.data.frame(table(part_three$OFFENSE_CODE_GROUP, part_three$DAY_OF_WEEK))


#Create stacked bar plot
ggplot(one_day, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Part One Crimes by Day", x="", y="Frequency")+ 
  scale_fill_manual(values = c("grey", "brown", "blue", "black", "purple", "gold", "green")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 18), legend.title=element_blank())


#Find occurrence of crime by day of the week
week_1 = group_by(one_day, Day=Var2) %>%
  summarize(
    Part_One_Crimes_Per_Day=sum(Freq))

week_2 = group_by(two_day, Day=Var2) %>%
  summarize(
    Part_Two_Crimes_Per_Day=sum(Freq))

week_3 = group_by(three_day, Day=Var2) %>%
  summarize(
    Part_Three_Crimes_Per_Day=sum(Freq))


#Find occurrence of crime by month
per_month1 = as.data.frame(table(part_one$OFFENSE_CODE_GROUP, part_one$MONTH))
per_month2 = as.data.frame(table(part_two$OFFENSE_CODE_GROUP, part_two$MONTH))
per_month3 = as.data.frame(table(part_three$OFFENSE_CODE_GROUP, part_three$MONTH))

month_1 = group_by(per_month1, Month=Var2) %>%
  summarize(
    Part_One_Crimes_Per_Month=sum(Freq))

month_2 = group_by(per_month2, Day=Var2) %>%
  summarize(
    Part_Two_Crimes_Per_Month=sum(Freq))

month_3 = group_by(per_month3, Day=Var2) %>%
  summarize(
    Part_Three_Crimes_Per_Month=sum(Freq))


#Retrieve information from the 2018 5-year ACS
#Create table for all variables in dataset
acs_vars = load_variables(2018, "acs5")


#Save data as csv file
write_csv(acs_vars, "acsvars.csv")


#Retrieve Census Data
total_pop = 
  get_acs(geography = "tract", variables = "B01001_001", state = "MA", 
          county = "Suffolk", year = 2018)


#Population of each census tract
popu = 
  group_by(total_pop, NAME) %>%
  summarize(population = sum(estimate))


#Join the data
both = left_join(popu, total_pop, by="NAME")


#Rename GEOID column so it matches
both = rename(both, GEOID10 = GEOID)


#Join Data
join = left_join(join, both, by="GEOID10")


#Count total crime for each census tract
crime = 
  group_by(join, NAME) %>%
  summarize(total_crime = n())


#Join Data
test = left_join(both, crime, by="NAME")


#Calculate crime rate
test$crime_rate = (test$total_crime / test$population) * 100




