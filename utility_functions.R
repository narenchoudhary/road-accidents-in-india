# Modify cause column in data frame
ModifyCause <- function(x){
  x <- unlist(strsplit(x, split='(', fixed=TRUE))[1]
  return(gsub('Total', '', x) %>% trimws() %>% tolower())
}

# Return plot
GetPlot <- function(x){
  table1.causes.subset <- table1.causes %>%
    filter(cause == as.character(x))
  subset.plot <- table1.causes.subset %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0)) +
    labs(title = paste('Accidents caused by', x),
         x = 'Years',
         y = 'Deaths \n (in thousand)') +
    scale_x_continuous(breaks = seq(2000, 2013, by = 1))
  return(subset.plot)
}