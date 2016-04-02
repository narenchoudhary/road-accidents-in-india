library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Prepare Data -------------------------------------------------------
# read csv
# read as tbl_df instead of data.frame
csv_f <- "Data/Years_individual/Details_of_road_accident_deaths_by_situation_state.csv"
table1 <- tbl_df(read.csv( file = csv_f, stringsAsFactors = FALSE))
names(table1) <- tolower(names(table1))


# Explore Data -------------------------------------------------------

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


table4 <- table1 %>% 
  filter(!grepl('Total', cause)) %>% 
  group_by(cause) %>% 
  summarize(male = sum(male, na.rm = T)/1000, female = sum(female, na.rm = T)/1000) %>% 
  arrange(male) %>% 
  melt(id.vars = 'cause')

table4$cause <- factor(table4$cause, levels = table4$cause)

ggplot(table4, aes(x = cause, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Deaths in road accidents from 2001-2012',
       y = 'Casualties (in thousands)',
       x = 'Vehicle involved')

table5 <- table1 %>% 
  filter(!grepl('TOTAL', state.ut)) %>% 
  group_by(year) %>% 
  summarise(male = sum(male)/1000, female = sum(female)/1000) %>% 
  melt(id.vars = 'year')

ggplot(table5, aes(x = year, y = value, color = variable)) + 
  geom_line()