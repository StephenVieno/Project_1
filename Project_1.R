# LOAD PACKAGES -----------------------------------------------------------
library(tidyverse)
library(heatmaply)


# CHECK DATA --------------------------------------------------------------
head(mtcars,6) 
data1 <- mtcars |> 
  rownames_to_column("car_type") |> # CHANGE ROW NAMES TO COLUMN 
  mutate(Car_status = ifelse(grepl("Merc", car_type), "Merc", # IF CAR IS MERC, LABEL AS MERC  
                             ifelse(grepl("Mazda", car_type), "Mazda",  # IF CAR IS MAZDA, LABEL AS MAZDA  
                                    ifelse(grepl("Toyota", car_type), "Toyota", "Other")))) # IF CAR IS TOYOTA, LABEL AS TOYOTA 
# IF CAR_STATUS IS NOT ANY OF THE MANUFACTURERS PREVIOUSLY LISTED, LABEL AS OTHER 

head(data1) # CHECK DATA                     
# CREATE GRAPH  -----------------------------------------------------------

# PLOT 1 ------------------------------------------------------------------

# Which car is best for me to buy? 
data1 |> pivot_longer(cols = mpg:carb, # PIVOT LONGER COLUMN WITH VARAIABLES 
                      names_to = "values_names", # VARIABLE NAME 
                      values_to = "values") |> # VALUES IN THE VARIABLES 
  ggplot() + 
  geom_jitter(aes(x = values_names, # GEOM_JITTER # X VALUES IS VARIABLE NAMES 
                  y = values, # Y VALUES IS VALUES IN THE VARAIBLES 
                  colour = car_type)) + # COLOUR POINTS ON THE BASIS OF CAR NAME  
  theme_bw() + # SET THEME 
  ggtitle("Scatter plot of variables in the mtcars dataset") + # TITLE PLOT 
  labs(colour = "Car Names") + # TITLE LEGEND 
  xlab("Variables in mtcars dataset") + # TITLE X-AXIS 
  ylab("Value (refer to variable for units)") # TITLE Y-AXIS 
