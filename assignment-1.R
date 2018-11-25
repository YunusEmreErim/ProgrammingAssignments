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

# Q5 ----------------------------------------------------------------------

bar <- ggplot(data = Differences_Countries) +
  geom_bar(mapping = aes(x = country, y = dif_net_worth, fill = dif_net_worth), stat = "identity")
bar + coord_flip()

# Q6 ----------------------------------------------------------------------

Differences_Countries$country <- factor(Differences_Countries$country, levels = Differences_Countries$country[order(Differences_Countries$dif_net_worth)])

bar <- ggplot(data = Differences_Countries) +
  geom_bar(mapping = aes(x = country, y = dif_net_worth, fill = dif_net_worth), stat = "identity")
bar + coord_flip()

# Q7 ----------------------------------------------------------------------

shared_ranks <-
  rich_list %>%
  group_by(rank) %>%
  summarize(count = n()) %>%
  filter(count > 1)

View(shared_ranks)

# Q8 ----------------------------------------------------------------------

average_rank <- shared_ranks
average_rank <-
  mutate(average_rank, other_rank = rank + count - 1)
average_rank <-
  mutate(average_rank, avrg_rank = ((rank + other_rank) / 2))

# Q9 ----------------------------------------------------------------------

library("rworldmap")

country_worth <- rich_list %>%
  group_by(country) %>%
  summarize(total_net_worth = log(sum(net_worth)))

CountryData <- joinCountryData2Map(country_worth, joinCode = "NAME", nameJoinColumn = "country" )

mapCountryData(CountryData,
               nameColumnToPlot = "total_net_worth",
               mapTitle = "Total Net Worth of Billionaires by their Countries",
               colourPalette = "heat",
               addLegend = TRUE,
               oceanCol = "lightgray", missingCountryCol = "grey")








