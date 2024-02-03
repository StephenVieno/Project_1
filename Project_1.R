# LOAD PACKAGES -----------------------------------------------------------
library(tidyverse)
library(heatmaply)


# CHECK DATA --------------------------------------------------------------
head(mtcars) 
data1 <- mtcars |> 
  rownames_to_column("car_type") |> # CHANGE ROW NAMES TO COLUMN 
  mutate(Car_status = ifelse(grepl("Merc", car_type), "Merc", # IF CAR IS MERC, LABEL AS MERC  
                             ifelse(grepl("Mazda", car_type), "Mazda",  # IF CAR IS MAZDA, LABEL AS MAZDA  
                                    ifelse(grepl("Toyota", car_type), "Toyota", "Other")))) |>  # IF CAR IS TOYOTA, LABEL AS TOYOTA 
  # IF CAR_STATUS IS NOT ANY OF THE MANUFACTURERS PREVIOUSLY LISTED, LABEL AS OTHER 
  mutate(mpg = percentize(mpg), # TRANFORM VARIABLE TO PERCENTILES FOR EACH VARIABLE 
         cyl = percentize(cyl),
         disp = percentize(disp),
         hp = percentize(hp), 
         drat = percentize(drat),
         wt = percentize(wt), 
         qsec = percentize(qsec), 
         vs = percentize(vs),
         am = percentize(am),
         gear = percentize(gear),
         carb = percentize(carb)) 

head(data1) # CHECK DATA                     
# CREATE GRAPH  -----------------------------------------------------------

# PLOT 4 ------------------------------------------------------------------

# Which type of car is best for me to buy? 
# Group cars by Merc, Mazda, Toyota, Other? 
# What is the median value for each variable for each car type? Which manufacturer would most likely be best?  
# What is the median value for each variable for each car type? 
# Which manufacturer would most likely be best?  
# Transform variables to percentiles 
data1 |> pivot_longer(cols = mpg:carb, # PIVOT LONGER COLUMN WITH VARAIABLES 
                      names_to = "values_names", # VARIABLE NAME 
                      values_to = "values") |> # VALUES IN THE VARIABLES 
  ggplot() + 
  geom_boxplot(aes(y = Car_status, # SET Y-AXIS TO CAR TYPES 
                   x = values, # SET X-AXIS TO PERCENTILE VALUES 
                   fill = Car_status)) + # FILL ON THE BASIS OF CAR TYPE   
  theme_bw() + # SET THEME 
  ggtitle("Box plot of variables in the mtcars dataset") + # TITLE PLOT 
  labs(fill = "Car Types") + # TITLE LEGEND 
  labs(title = "Box plot of variables in the mtcars dataset", # TITLE PLOT 
       subtitle = "Variables transformed to percentiles", # SUBTITLE PLOT
       fill = "Car Types") + # TITLE LEGEND 
  ylab("Car Types") + # TITLE Y-AXIS 
  xlab("") + # TITLE X-AXIS 
  xlab("Percentile") + # TITLE X-AXIS 
  facet_wrap(~values_names) + # FACET WRAP BASED ON VARIABLE NAMES 
  theme(axis.text.x = element_text(vjust=0.6, angle=-45)) # CHANGE SLANT OF LABELS ON X-AXIS 


# PLOT 5 ------------------------------------------------------------------
data1 |> pivot_longer(cols = mpg:carb, # PIVOT LONGER COLUMN WITH VARAIABLES 
                      names_to = "values_names", # VARIABLE NAME 
                      values_to = "values") |> # VALUES IN THE VARIABLES 
  ggplot() + 
  geom_boxplot(aes(y = Car_status, # SET Y-AXIS TO CAR TYPES 
                   x = values, # SET X-AXIS TO PERCENTILE VALUES 
                   fill = Car_status)) + # FILL ON THE BASIS OF CAR TYPE   
  theme_bw() + # SET THEME 
  labs(title = "Box plot of variables in the mtcars dataset", # TITLE PLOT 
       subtitle = "Variables transformed to percentiles", # SUBTITLE PLOT
       fill = "Car Types") + # TITLE LEGEND 
  ylab("Car Types") + # TITLE Y-AXIS 
  xlab("Percentile") + # TITLE X-AXIS 
  facet_wrap(~values_names, # FACET WRAP BASED ON VARIABLE NAMES 
             scales = "free") + # EACH X-AXIS IS SCALED TO EACH FACET 
  theme(axis.text.x = element_text(vjust=0.6, angle=-45)) # CHANGE SLANT OF LABELS ON X-AXIS 

