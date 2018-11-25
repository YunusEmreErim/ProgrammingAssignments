library(tidyverse)


# Q1&Q2 ----------------------------------------------------------------------

forbes <- read_csv("forbes.csv")
forbes$rank = as.numeric(gsub("\\#", "", forbes$rank ))
forbes$age = as.numeric(gsub("\\-", "", forbes$age ))
forbes$net_worth = as.numeric(gsub("\\$|B|M", "", forbes$net_worth))
rich_list <- filter(forbes, net_worth < 100)

View(rich_list)

# Q3 ----------------------------------------------------------------------

ggplot(data = rich_list) + 
  geom_point(mapping = aes(x = age, y = net_worth)) +
  geom_smooth(mapping = aes(x = age, y = net_worth ))

ggplot(data = rich_list) + 
  geom_point(mapping = aes(x = age, y = log(net_worth))) +
  geom_smooth(mapping = aes(x = age, y = log(net_worth)))

# YES, it helps observing the positive correlation. When age plotted against net_worth, since the internak of net_worth is large, it is hard to observe the relation.

# Q4 ----------------------------------------------------------------------

Differences_Countries <-
  rich_list %>%
  group_by(country) %>%
  summarize(
    count = n(),
    max_net_worth = max(net_worth),
    min_net_worth = min(net_worth),
    dif_net_worth = max_net_worth - min_net_worth) %>%
  
  filter(count >= 6) %>%
  arrange(dif_net_worth)









