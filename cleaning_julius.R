# Install, load relevant Packages; Pacman--------
install.packages("pacman")

## Load relevant packages ------------------------------------------------------

pacman::p_load(
  rio,
  here,
  janitor,
  lubridate,
  matchmaker,
  epikit,
  tidyverse,
  openxlsx,
  skimr
)

# Import Data ------------------------------------------------------------------
states <- import("by_state.xlsx") # replace with the location of the file


# start a cleaning pipe chain for state level data -----------------------------

states_clean <- states %>% # load the original set, store in a different name in the environment
  rename(
    "state" = ...1,
    "type_of_injury" = ...2,
    "location_of_injury" = ...3,
    "year" = ...4,
    "january" = "2011",
    "february" = ...7,
    "march" = ...9,
    "april" = ...11,
    "may" = ...13,
    "june" = ...15,
    "july" = ...17,
    "august" = ...19,
    "september" = ...21,
    "october" = ...23,
    "november" = ...25,
    "december" = ...27
  ) %>% 
  clean_names() %>% 
  select("state","type_of_injury", "location_of_injury", "year", "january",
         "february", "march", "april", "may", "june", "july", "august",
         "september", "october","november", "december" ) %>% 
  filter(state != "state" & state != "",
                  type_of_injury != "Total",
                  location_of_injury != "Total") %>% 

  distinct() # removes duplicates


# 1. Determine the total number of observations (rows) in your data frame-------
total_rows <- nrow(states_clean)

# 2. Create the specific sequence of years you want to repeat-------------------
year_sequence <- 2011:2025


# 3. Repeat each year 192 times-------------------------------------------------
# The 'each' argument repeats *each element* of 'year_sequence' 192 times.
repeated_values <- rep(year_sequence, each = 192)


# 4. Trim the vector to exactly match the number of rows in your data frame-----
# This is crucial to prevent an error when adding the column.
YearGroup <- repeated_values[1:total_rows]


# 5. Add the new vector as a column to your data frame--------------------------
states_clean$year <- YearGroup
  

# 6. pivot data into long format---------------------------------------------------
state_long <- states_clean %>% 
  pivot_longer(
    cols = 5:16,
    names_to = "month",
    values_to = "cases"
  )
  



# import data
casualties <- import("number_casualties.xlsx")

# start a cleaning pipe chain for state level data -----------------------------

  
# 1. pivot data into long format---------------------------------------------------
  casualties_long <- casualties %>% 
  select(-c("Total")) %>% 
    pivot_longer(
      cols = 6:16,
      names_to = "age_group",
      values_to = "cases",
)   
  
#remove total from casualties_long----------------------------------------------
 
  
# data cleaning pipe -----------------------------------------------------------


#----------------------------


