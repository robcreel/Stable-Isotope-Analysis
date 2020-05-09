library(tidyverse)
library(lubridate)

df <- read_csv("mega.csv")

df %>% mutate(TIMESTAMP = ymd_hm(TIMESTAMP)) -> df

df %>% group_by(month = floor_date(TIMESTAMP, "month")) %>% 
  summarise(Delta_18O = mean(Delta_18O), ) -> df2

df %>% 
  ggplot(aes(x = TIMESTAMP, y = Delta_18O, color = Latitude)) +
  geom_point() # + geom_smooth(method = "loess")


# df %>% group_by(Longitude, Latitude) %>% 
#   summarise(Delta_18O = )



