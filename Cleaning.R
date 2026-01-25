pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse, # data management and visualization
  skimr
)
 
# IMPORT -----------------------------------------------------------------------
 
linelist_raw <- import("linelist_raw.xlsx")
#skimr::skim(linelist_raw)


# CLEANING --------------------------------------------------------------------
linelist <- linelist_raw %>% 
  
  ## Clean Column Names --------------------------------------------------------
  janitor::clean_names() %>% 
  
  ## Rename Column Names --------------------------------------------------------
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)%>% 
  
  distinct()


#### ---------------------------------------------------
pacman::p_load(
  tidyverse,      # includes ggplot2 and other data management tools
  janitor,        # cleaning and summary tables
  ggforce,        # ggplot extras
  rio,            # import/export
  here,           # file locator
  stringr         # working with characters   
)
linelist <- rio::import("linelist_cleaned.rds")


##### Mapping data to the plot #####------------------------------------------
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+
  geom_point()    

#####--------------------------------------------------------
