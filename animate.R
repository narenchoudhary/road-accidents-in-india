library(ggplot2)
library(animation)
library(gganimate)
  
p <- ggplot(table1.causes, aes(x = year, y = value, color = variable, frame = cause)) + 
  geom_line() + 
  geom_point() +
  theme_bw() + 
  theme(legend.position = 'bottom')+
  labs(y = 'Deaths\n(in thousands)', x = 'year', color = 'Gender') +
  scale_x_continuous(breaks = seq(2000, 2013, by = 1))

p_animate <- gg_animate(p, ani.width = 600, ani.height = 800)
gg_animate_save(g = p_animate, filename = "road_accidents_causes.gif")