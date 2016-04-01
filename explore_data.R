library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# read csv
# read as tbl_df instead of data.frame
table1 <- read.csv(
  file = "Data/Years_individual/Details_of_road_accident_deaths_by_situation_state.csv",
  stringsAsFactors = FALSE
) %>% tbl_df()

names(table1) <- names(table1) %>% tolower()


table2 <- table1 %>% 
  group_by(year) %>%
  summarise(male = sum(male)/1000, female = sum(female)/1000) %>% 
  melt(id.vars = "year")


table2_gg1 <- table2 %>% ggplot(aes(x = year, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_continuous(breaks = seq(2000, 2014, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  labs(title = 'Deaths in road accidents from 2001-2012',
       y = 'Casualties (in thousands)',
       x = 'Year') +
  theme_bw() +
  theme(legend.position = 'bottom')
  
# get statewise total casualties
# remove state with less than 5k casualties
table3 <- table1 %>% 
  filter(!grepl('TOTAL', state.ut)) %>% 
  group_by(state.ut) %>% 
  summarise(male = sum(male)/1000, female = sum(female)/1000, total = sum(total)/1000) %>% 
  filter(total > 5) %>%
  arrange(total)

# explicitly mention order of x axis ticks to stop ggplot from
# reordering them
table3$state.ut <- factor(table3$state.ut, levels = table3$state.ut) 

table3 %>% ggplot(aes(x = state.ut, y = total)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Deaths in road accidents from 2001-2012',
       y = 'Casualties (in thousands)',
       x = 'States/UTs')
