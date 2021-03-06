library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)

# Prepare Data
# Filter by cause variable
table1.causes <- table1 %>%
  filter(cause %in% c('Three Wheeler', 'Two Wheeler', 'Bicycle', 'Pedestrian',
         'Others (Please Specify)', 'Total Truck/Lorry', 'Total Bus',
         'Total Tempo/Vans', 'Total Jeep', 'Total Car'))

# group by year and cause
# sum deaths by gender
# melt the data frame
table1.causes <- table1.causes %>%
  filter(!is.na(male), !is.na(female)) %>%
  group_by(year, cause) %>%
  summarise(male = sum(male)/1000, female = sum(female)/1000) %>%
  melt(id.vars = c('year', 'cause'))

# remove the '(Government)' and '(Private)' strings from cause variable 
table1.causes$cause <- sapply(table1.causes$cause, ModifyCause)

# vector of all causes for which plot is needed
allcauses <- c("pedestrian", "bicycle", "two wheeler", "three wheeler", "bus", 
               "car", "jeep", "tempo/vans", "truck/lorry", "others")
table1.causes$cause <- factor(allcauses, levels = allcauses)

# list of year vs deaths for every cause
plot_list <- lapply(allcauses, GetPlot)
plot_list[["ncol"]] = 2
do.call(grid.arrange, plot_list)
