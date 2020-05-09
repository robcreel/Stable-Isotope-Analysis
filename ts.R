library(tidyverse)
library(lubridate)
library(graphics)
library(tsibble)
# library(simts)

df_mega <- read_csv("mega.csv")

df_mega %>% mutate(TIMESTAMP = ymd_hm(TIMESTAMP)) %>% drop_na() -> df_mega
df_mega %>% filter(Name == "NewHaven") -> df_newhaven
df_mega %>% as_tsibble(index = X1) -> ts_mega

df_gts <- gts(data = df$Delta_18O, start = 0, freq = 1, unit_ts = "year?", name_ts = "Delta Oxygen 18", data_name = "Difference from Average Oxygen 18")

plot(df_gts)

df_newhaven %>% 
  ggplot(aes(x = TIMESTAMP, y = Delta_18O)) +
  geom_point() + geom_smooth(method = "loess")

df_mega %>% 
  ggplot(aes(x = TIMESTAMP, y = Delta_18O)) +
  geom_point() + 
  geom_smooth(method = "loess") + 
  facet_wrap(Name ~ ., ncol = 6)

df_mega %>% 
  ggplot(aes(x = Delta_18O, y = Delta_D, color = Name)) +
  geom_point(position = "jitter", alpha = 0.5) +
  geom_smooth(method = "loess")
