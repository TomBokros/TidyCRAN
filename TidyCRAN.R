cran <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

library(tidyverse)
library(plotly)

options(scipen=999)

difference <-  cran %>% 
  mutate(waste=comment+blank) %>% 
  mutate(difference=waste-code) %>% 
  filter(code<10000)

#The only ones with more blanks than code have less than 10,000 lines of code.

wasteful <- difference %>% 
  mutate(`greater?`=blank>=code) %>% 
  filter(`greater?`==T)


map <- ggplot(data=difference,aes(x=code,y=blank, colour = "red"))+
  geom_point()+
  geom_point(data=wasteful, aes(x=code, y=blank, colour="blue", name=pkg_name))+
  coord_equal(ratio=1)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  theme(legend.position = "none")+
  labs(x="Lines of Code", y= "Blank Lines")

ggplotly(map)

