# bike project
My bike project

## This is a project i did on the cyclic dataset, which is a fake bike sharing company.

Here we are looking at the data provided to us by cyclistic bike sharing company. We are trying to find out if causal riders make up more of the customer base then the subcscribed customers. we see who rides more frequent and at what durations they ride for.

When we look at specific days, Tuesday to Thursday seem to be the favorites among members. This lines up with the idea that they use the bikes mainly for their work commutes during the weekdays.

No matter if you're a member or a casual user, it's common to see more rides happening between 3 p.m. and 6 p.m. It looks like these hours are popular for people, maybe for casual rides or running errands.

Different stations catch the attention of each group: members start or end rides often at Kingsbury and Kinzie St., while casual users prefer Streeter Dr. and Grand Ave. This difference might be because of where they live or hang out.

Casual users tend to take rides mostly on weekends, which suggests they're into weekend activities and fun. On the other hand, members stick to their routines more.

It's interesting that casual users tend to ride longer distances on average compared to members. This could mean they're up for exploring or just taking longer rides for enjoyment.

When we talk about the whole year, July has the most riders. It's probably due to good weather. But come December, ridership goes down, maybe because it's colder.

Saturdays are a hit with everyone – both members and casual users. It's the busiest day. Interestingly, casual users edge out members a bit on this day. That shows how Saturdays are a big deal for everyone, with casual users showing a tad more love.

# My 3 Recommendations Based On My Analysis

The marketing strategy should focus on increasing membership awareness and attracting casual users by offering special deals in locations where casual users are most frequent. By targeting specific areas, the marketing efforts can be more effective while reducing overall expenditure.

Incentives should be designed to encourage casual riders to transition to becoming members. Emphasize the benefits of membership, such as reducing one carbon footprint, saving money, and improving health through low-impact physical activity. These angles can resonate with potential members and motivate them to make the switch.

Conduct targeted surveys for all user types to gain valuable insights. By using data-driven user type hotspots, the surveys can gather detailed feedback from different user segments. This information will help in understanding user preferences, needs, and pain points, allowing for more informed decision-making and tailored marketing strategies.

How do annual members and casual riders use Cyclistic differently?
The provided information pertains specifically to the difference between annual members and casual riders during the period from July 2022 to June 2023. It is important to note that these insights are limited to the data we have on hand, and further implications may require addressing the remaining stakeholder questions.

# Here is a summary of the key findings:
Members prefer riding on weekdays, with the most frequent rides occurring from Tuesday to Thursday. In contrast, casual users prefer weekends, particularly Saturdays, suggesting different usage patterns for both groups.

Kingsbury and Kinzie St. are the top start and end stations for members, whereas Streeter Dr. and Grand Ave. are preferred by casual users. Station preferences may align with the locations of residential and commercial areas frequented by each group.

Classic bikes are the most preferred bike type among both members and casual users, indicating a preference for traditional biking options.

Both groups commonly ride between 3-6 p.m., likely corresponding to the late afternoon when many people use the service for various activities.

April to October is the preferred period for both members and casual users, likely due to favorable weather conditions during these months.

July is the most popular month, likely due to summer weather and increased outdoor activities. December records the lowest ridership, possibly due to colder weather and holiday-related factors.

Saturday is the most popular day of the week across all users, and it is the only day when casual users slightly surpass members in usage, although the difference is marginal.

It is important to consider these findings when formulating strategies and making data-driven decisions to better serve both member and casual user segments effectively.
The data has been made available by Motivate International Inc. under this [license](uhttps://ride.divvybikes.com/data-license-agreementrl)

## First we are going to load up the librarys we need for the analysis we are about to do wich are dplyr and tidyvrse.

install.packages("tidyverse")

install.packages("conflicted")

library(tidyverse)  #helps wrangle data

### Use the conflicted package to manage conflicts

library(conflicted)

### Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")

conflict_prefer("lag", "dplyr")

### Here we are assighning our databases to easier variables
q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv") #here we assighned the databases to a variable

q1_2020 <- read.csv("divvy_Trips_2020_Q1.csv")

### This is a simple way to see which column names are the same and which we are going to have to change.
colnames(q1_2019) #here we checked the column names for both our database.

colnames(q1_2020) #we decided to make the Q1 2020 file the main one. so we are gonna change the column names so they are all matching


### Now that we know which columns to change, we use this code to match the columns

(q1_2019 <- rename(q1_2019,
                  
                  ride_id = trip_id,
                  
                  rideable_type = bikeid,
                  
                  started_at = start_time,
                  
                  ended_at = end_time,
                  
                  start_station_name = from_station_name,
                  
                  end_station_id = from_station_id,
                  
                  end_station_name = to_station_name,
                  
                  member_casual = usertype))
### This is how we are going to check the data structures, make sure the numbers are numbers and not charecters.
str(q1_2019) 

str(q1_2020)

#### we see that some colmns data types dont match up so we want to fix that. like ride_id in 2019 is int while in the 2020 its a chr

q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id),

                  
                  rideable_type = as.character(rideable_type)) #in this we changed them to characters.

all_trips <- bind_rows(q1_2019, q1_2020) 
#### this lets us combine our data into one big dataframe instead of the two seperate ones we were working on.

all_trips <- all_trips %>% 

  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

### Inspect the new table that has been created

colnames(all_trips)  #List of column names

nrow(all_trips)  #How many rows are in data frame?

dim(all_trips)  #Dimensions of the data frame?

head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)

str(all_trips)  #See list of columns and data types (numeric, character, etc)

summary(all_trips)  #Statistical summary of data. Mainly for numerics

