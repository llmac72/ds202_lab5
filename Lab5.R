library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(maps)
##install.packages('devtools')
install.packages("mapdata")

accidents <- read_excel('C:/Users/Lisa.McDaniel/Documents/DS202/Lab5/FARS.xlsx', 
                        sheet="accidents")

persons <- read_excel('C:/Users/Lisa.McDaniel/Documents/DS202/Lab5/FARS.xlsx', 
                         sheet="persons")

str(accidents)

##1.	Are there some days of the week where more accidents happen 
##than the others (see FARS manual, use variable DAY_WEEK)?

DoW <- accidents %>%
  filter(DAY_WEEK != "NA") %>%
  mutate(DoW  = wday(DAY_WEEK, label = TRUE))%>%
  select(DAY_WEEK, DoW) %>%
##  group_by(DAY_WEEK) %>%
##  mutate(DoW = as.character(DoW)) %>%
  count(DoW, na.rm=TRUE)
 
str(day)

ggplot(DoW, aes(DoW, n, fill=DoW)) + 
  geom_bar(stat = "identity") + labs(x="Day", y="Accidents") +
  ggtitle("Accidents by Day") + 
  theme(plot.title = element_text(hjust = 0.5))
  
##2.	Create a data frame containing the persons who are 
##fatally hurt in the accidents (look up variable INJ_SEV)

##latlong <- accidents %>%
##  select(STATE, ST_CASE, LATITUDE, LONGITUD)
  
Injury <- persons %>%
##  select(STATE, ST_CASE, INJ_SEV) %>%
  filter(INJ_SEV == 4)
##  inner_join(latlong)

head(Injury)  
  
##3. Create a data frame containing the most dangerous 
##vehicle make in each state. The number of persons fatally 
##hit in the vehicle make is used to assess the (non-)safety 
##of a make. Make sure to handle the missing values appropriately. 
##(look up variable MAKE)  

##latlong <- accidents %>%
##  select(STATE, ST_CASE, LATITUDE, LONGITUD) %>%
##  distinct(STATE, LATITUDE, LONGITUD)

DanVeh <- Injury %>%
  select(STATE, INJ_SEV, MAKE) %>%
  filter(MAKE != "NA") %>%
  group_by(STATE, MAKE) %>%
  count(MAKE) %>%
##  arrange(desc(n,STATE, MAKE)) %>%
  group_by(STATE) %>%
  top_n(n=1, n) %>%
  mutate(MAKE = as.character(MAKE)) %>%
  mutate(Vehicle = str_replace_all(MAKE, c("49" = "Toyota",                                           "CULVERT EXTENSION|CULVERT REPAIR" = "Maintenance",
                                           "12" = "Ford",
                                           "20" = "Chevrolet",
                                           "37" = "Honda"))) %>%
  rename(`State Code` = STATE)

head(DanVeh)

Danger <- DanVeh %>%
  inner_join(latlong)
##  rename(`State Code` = STATE)
  


##4.	Create a map, and label each state with the most 
##dangerous vehicle. Discuss the definition of the most 
##dangerous vehicle, and what you find from the map. (Hint: 
##Read the description for the STATE and COUNTY columns in the
##FARS manual. The state & county codes are Geographic Locator 
##Codes (GLCs) from the General Services Administration's (GSA)
##publication. Use readxl::read_xlsx to read in the GLCs.)

install.packages("mapproj")

GLCUS <- read_excel('C:/Users/Lisa.McDaniel/Documents/DS202/Lab5/GLC_US.xlsx')

latlong <- read_excel('C:/Users/Lisa.McDaniel/Documents/DS202/Lab5/latlong.xlsx')

latlong <- latlong %>%
  mutate(State = tolower(name)) %>%
  select(-name, -state)
  

warnings()
str(GLCUS)

st <- GLCUS %>%
  distinct(`State Code`, `State Name`)
##  mutate(`State Code`= as.character(`State Code`))

str(st)
str(DanVeh)

Vehicle <- st %>% 
  inner_join(DanVeh) %>%
  mutate(State = tolower(`State Name`))

str(Vehicle)

??tolower

??map_data

states <- map_data('state')

auto <- Vehicle %>%
  left_join(latlong)%>%
  filter(State != "alaska") %>%
  filter(State != "hawaii")
  

ggplot(states, aes(x=long, y=lat),) + 
  geom_path(aes(group=group)) + 
  geom_polygon(aes(group=group), colour='black', fill=NA) +
  geom_text(data=auto, aes(x=longitude, y=latitude, label = Vehicle), size=1.75) 

##  geom_text(DanVeh, label=Vehicle)

##5.	Join the accident and person table (work out which 
##variable(s) to use)

PerAcc <- persons %>% 
  left_join(accidents)

head(PerAcc) 
str(PerAcc)

##6.	Tally the number of accidents by day of the week 
##(DAY_WEEK), hour of the day (HOUR) and gender (SEX). 
##Visualize the results.         

??tally
PerAcc1 <- PerAcc %>%
  filter(DAY_WEEK != "NA", HOUR != "NA", SEX != ("NA")) %>%
  mutate(DoW  = wday(DAY_WEEK, label = TRUE))%>%
  select(DAY_WEEK, DoW, HOUR, SEX) %>%
  group_by(DoW, HOUR, SEX)%>%
  tally()

ggplot(PerAcc1, aes(DoW, n, fill=SEX)) + 
  geom_bar(stat = "identity") + labs(x="Day", y="Accidents") +
  ggtitle("Accidents by Day") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~HOUR)

##7.	Now plot a choropleth map of the number of deaths on a 
##county level.

StCo <- GLCUS %>%
  distinct(`State Code`, `State Name`, `County Code`, `County Name`) %>%
  rename(STATE = `State Code`, COUNTY = `County Code`) %>%
  mutate(COUNTY = as.numeric(COUNTY))

str(StCo)

county <- PerAcc %>%
  select(STATE, INJ_SEV, COUNTY, LATITUDE, LONGITUD)%>%
  filter(INJ_SEV == 4) %>%
  left_join(StCo) %>%
  group_by(STATE, COUNTY, LATITUDE, LONGITUD)

str(county)  

ggplot(states, aes(x=long, y=lat)) + geom_polygon(aes(group=group)) + 
  geom_point(data=county, aes(x=LONGITUD, y=LATITUDE), color='lightgreen', size=0.02, alpha=0.2) +
  coord_map(xlim=c(-130, -60), ylim=c(20, 50))

##8.	Is summer or winter more dangerous? Does this depend on 
##states? Explore and explain.

str(accidents)

Sea1 <- accidents %>%
  select(STATE, MONTH) %>%
  filter(MONTH != 99 & MONTH !=3 & MONTH !=4 &
           MONTH !=5 & MONTH !=6 & MONTH !=10 & 
           MONTH != 11) %>%
  mutate(Month  = month(MONTH, label = TRUE)) %>%
  select(-MONTH)%>%
  mutate(Season = str_replace_all(Month, c("Jan" = "Winter",
                  "Feb" = "Winter", "Dec" = "Winter", "Jul" = "Summer",
                  "Aug" = "Summer", "Sep" = "Summer"))) %>%
  group_by(Season) %>%
  count(Season)

ggplot(Sea1, aes(Season, n, fill=Season)) + 
  geom_bar(stat = "identity") + labs(x="Season", y="Accidents") +
  ggtitle("Accidents by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(x=Season,y=n,label=n),vjust=-0.5, size =2) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank())


Sea2 <- accidents %>%
  select(STATE, MONTH) %>%
  filter(MONTH != 99 & MONTH !=3 & MONTH !=4 &
           MONTH !=5 & MONTH !=6 & MONTH !=10 & 
           MONTH != 11) %>%
  mutate(Month  = month(MONTH, label = TRUE)) %>%
  select(-MONTH)%>%
  rename(`State Code` = STATE) %>%
  inner_join(st) %>%
  mutate(Season = str_replace_all(Month, c("Jan" = "Winter",
                                           "Feb" = "Winter", "Dec" = "Winter", "Jul" = "Summer",
                                           "Aug" = "Summer", "Sep" = "Summer"))) %>%

  group_by(`State Name`, Season) %>%
  count(Season)
  
ggplot(Sea2, aes(Season, n, fill=Season)) + 
  geom_bar(stat = "identity") + labs(x="Season", y="Accidents") +
  ggtitle("Accidents by Season") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~`State Name`, scales = "free") +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  geom_text(aes(x=Season,y=n,label=n),vjust=0, size =2)
