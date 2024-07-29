install.packages("tidyverse")
install.packages("conflicted")
library(tidyverse)  #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv") #here we assighned the databases to a variable
q1_2020 <- read.csv("divvy_Trips_2020_Q1.csv")

colnames(q1_2019) #here we checked the column names for both our database.
colnames(q1_2020) #we decided to make the Q1 2020 file the main one. so we are gonna change the column names so they are all matching

(q1_2019 <- rename(q1_2019,  #this is the code, we switched the column names so they can match in both databases
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  end_station_id = from_station_id,
                  end_station_name = to_station_name,
                  member_casual = usertype))


str(q1_2019) #we see thatsome colmns data types dont match up so we want to fix that. like ride_id in 2019 is int while in the 2020 its a chr
str(q1_2020)

q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id), #this code will change the data types of what we specify.
                  rideable_type = as.character(rideable_type)) #in this we changed them to characters.

all_trips <- bind_rows(q1_2019, q1_2020) #this lets us combine our data into one big dataframe instead of the two seperate ones we were working on.

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

all_trips <- all_trips %>%  #this code is going to change the the values it can be. in this case it can only be subscriber and customer.
  mutate(member_casual = recode(member_casual, #we are using the recode() function to change it to member and casual instead of subscriber and customer
                                "Subscriber" = "member",
                                "Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m") #this will tell us the month
all_trips$day <- format(as.Date(all_trips$date), "%d") #this will tell us the day
all_trips$year <- format(as.Date(all_trips$date), "%Y") #this will tell us the year.
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") #this will tell us if its a tuesday or thursday.

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at) #wow this is why i im starting to love R, it has the abilaty to tell me in seconds
#how long each ride was with this code right here and then added a new column it titled ride length

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length) #checks if its a factor
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) #this code is going to change the it from saying how many seconds(factor) to a number(numeric) in seconds
is.numeric(all_trips$ride_length) #checks if its a numeric value

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean) #this gets the mean for both casual and members of each day of the week.

#this code will put the days of the week in order.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) 

# well run that code again and this time it will come out in order.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean) #this created a new visual under the counts variable

write.csv(counts, "C:\\Users\\19254\\OneDrive\\Desktop\\School notes\\Portfolio\\Cycleistic project\\counts.csv")  #this is how we save it into a csv. file