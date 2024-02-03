# LOAD PACKAGES -----------------------------------------------------------
library(tidyverse)
library(heatmaply)


# CHECK DATA --------------------------------------------------------------
head(mtcars) 
data1 <- mtcars |> 
  rownames_to_column("car_type") |> # CHANGE ROW NAMES TO COLUMN 
  mutate(Car_status = ifelse(grepl("Merc", car_type), "Merc", # IF CAR IS MERC, LABEL AS MERC  
                             ifelse(grepl("Mazda", car_type), "Mazda",  # IF CAR IS MAZDA, LABEL AS MAZDA  
                                    ifelse(grepl("Toyota", car_type), "Toyota", "Other")))) # IF CAR IS TOYOTA, LABEL AS TOYOTA 
# IF CAR_STATUS IS NOT ANY OF THE MANUFACTURERS PREVIOUSLY LISTED, LABEL AS OTHER 

head(data1) # CHECK DATA                     
# CREATE GRAPH  -----------------------------------------------------------

# PLOT 2 ------------------------------------------------------------------

# Which car is best for me to buy? 
# Which type of car is best for me to buy? 
# Group cars by Merc, Mazda, Toyota, Other? 
data1 |> pivot_longer(cols = mpg:carb, # PIVOT LONGER COLUMN WITH VARAIABLES 
                      names_to = "values_names", # VARIABLE NAME 
                      values_to = "values") |> # VALUES IN THE VARIABLES
  ggplot() + 
  geom_jitter(aes(x = Car_status, # SET X VALUES TO CAR TYPES 
                  y = values, # SET Y VALUES TO VALUES 
                  colour = Car_status), # SET COLOUR TO CAR TYPES 
              alpha = 0.7, # SET ALPHA TO 0.7 
              width = 0.2, # SET WIDTH TO 0.2 
              height = 0) + # SET HEIGHT TO 0
  theme_bw() + # SET THEME 
  ggtitle("Scatter plot of variables in the mtcars dataset") + # TITLE PLOT 
  labs(colour = "Car Names") + # TITLE LEGEND 
  labs(colour = "Car Types") + # TITLE LEGEND 
  xlab("Variables in mtcars dataset") + # TITLE X-AXIS 
  ylab("Values (refer to variable for units)") + # TITLE Y-AXIS 
  facet_wrap(~values_names, # FACET WRAP BASED ON VARIABLE NAMES 
             scales = "free") # SET EACH Y-AXIS FOR EACH FACET TO INDIVDUALLY 


