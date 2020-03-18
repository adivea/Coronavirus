# Coronavirus outbreak

# https://towardsdatascience.com/an-r-package-to-explore-the-novel-coronavirus-590055738ad6
# https://github.com/GuangchuangYu/nCov2019

# Install the Coronavirus package
remotes::install_github("GuangchuangYu/nCov2019")

#To get the latest data, you can load it in with get_nCov2019() .

library(nCov2019)
x <- get_nCov2019(lang='en')

# see what the situation is. This data is collected from Tencent, 
# at https://news.qq.com/zt2020/page/feiyan.htm, 
# which contains one of the most up-to-date public information of the coronavirus.
x

# show global distribution
plot(x)

# To look at the global data, you can run the code
x["global",]
x["Anhui",]
x

# use this data for visualization. So here’s a quick example for Anhui.
library(tidyverse)
library(forcats)
library(ggplot2)

source("../nCov2019/example.R")

d = x['Anhui',] # replace Anhui with any province
d = x['global',]
d$confirm=as.numeric(d$confirm)
d$name = fct_reorder(d$name, d$confirm)

ggplot(d, aes(name, confirm)) + 
  geom_col(fill='steelblue') + coord_flip() +
  geom_text(aes(y = confirm+2, label=confirm), hjust=0) +
  theme_minimal(base_size=14) + 
  scale_y_continuous(expand=c(0,10)) +
  xlab(NULL) + ylab(NULL)




# Summarize data, if you wanted to view the new daily cases, you could use the
summary(x, by="today")



# Cumulative plot of confirmed cases across the world
library(lubridate)
library(ggplot2)
ggplot(summary(x), aes(as.Date(date, "%m.%d"), as.numeric(confirm))) +
  geom_col(fill='firebrick') + theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  labs(caption = paste("accessed date:", time(x)))

# dead across the world
ggplot(summary(x), aes(as.Date(date, "%m.%d"), as.numeric(dead))) +
  geom_col(fill='firebrick') + theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  labs(caption = paste("accessed date:", time(x)))

# deathrate across the world
ggplot(summary(x), aes(as.Date(date, "%m.%d"), as.numeric(deadRate))) +
  geom_col(fill='firebrick') + theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  labs(caption = paste("accessed date:", time(x)))



############ Historical data

y <- load_nCov2019(lang='en')
y
#head(y[][c(1:10, 9:11)])
head(y['Hubei'],)
head(y)

# little check on the Czechs
e <- y['global']
tail(e)
summarize(e, by= "today")
which(e[,"country"]=="Czech Republic") # can we filter for an individual country?
e[820,] # Denmark is still in

install.packages("ggrepel")
require(ggrepel)
ggplot(e, aes(time, as.numeric(cum_confirm), group=country, color=country)) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label=country), data=na.omit(e[e$time == time(y), ]), hjust=0) +
  theme_minimal(base_size = 14) + theme(legend.position='none') +
  xlab(NULL) + ylab(NULL) +
  labs(title = "Growth curve of confirmed cases across the world")

library(ggplot2)
require(ggrepel)
install.packages("ggrepel")

head(d, n=5)

d <- y['Anhui',] # replace Anhui with any province or global dataset
d[5,]
head(d)
a <- c(1,2,3,4,5, 6, 7, 8, 9 , 10, 11, 12, 13, 14)
ggplot(d, aes(time, as.numeric(cum_confirm), group=city, color=city)) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label=city), data=d[d$time == time(y), ], hjust=1) +
  theme_minimal(base_size = 14) + theme(legend.position='none') +
  xlab(NULL) + ylab(NULL)+
  labs(title = "Growth curve of confirms in Anhui Province, China")


# Problem: Labels have disappeared due to the problme in 
#sampling time-series. It seems the data is less good/skips so sampling is tricky?
require(graphics)
?presidents
cycle(presidents)
# a simple series plot
plot(as.vector(time(presidents)), as.vector(presidents), type = "l")



d <- y['global',]
head(d)
d <- d %>% filter(cum_confirm > 100 & country != "China")
ggplot(d, aes(time, as.numeric(cum_confirm), group=country, color=country)) +
  geom_point() + geom_line() +
  geom_text_repel(aes(label=country), data=d[d$time == time(x), ], hjust=0) +
  theme_minimal(base_size = 14) + theme(legend.position='none') +
  xlab(NULL) + ylab(NULL)



# 
ggplot(summary(x, 'Hubei'), aes(time, as.numeric(cum_confirm))) +
geom_col()
# or city-specific
# ggplot(subset(x['Hubei',], city == "Huanggang"), aes(time, as.numeric(cum_confirm))) + geom_col()

ggplot(subset(x['global',], country =='Germany'), aes(time, as.numeric(cum_confirm))) +
  geom_col()
ggplot(subset(x['global',], country =='Denmark'), aes(time, as.numeric(cum_confirm))) +
  geom_col()



unique(e$country)
globaldata %>% filter(country==countries)


e %>% 
  filter(country == "Germany" | country == "United Kingdom" | country == "Denmark" | country == "Switzerland") %>% 
  ggplot(aes(time,as.numeric(cum_confirm))) + 
  geom_area() +
  facet_wrap(~country)



## Tile GGPLOT
library(ggplot2)
y <- load_nCov2019(lang = 'en')
  d <- y['global']
max_time <- max(d$time)
min_time <- max_time - 7
d <- na.omit(d[d$time >= min_time & d$time <= max_time,])
dd <- d[d$time == max(d$time, na.rm = TRUE),]
d$country <- factor(d$country, 
                    levels=dd$country[order(dd$cum_confirm)])
breaks = c(10, 100, 1000, 10000)
ggplot(d, aes(time, country)) + 
  geom_tile(aes(fill = cum_confirm), color = 'black') + 
  scale_fill_viridis_c(trans = 'log', breaks = breaks, 
                       labels = breaks) + 
  xlab(NULL) + ylab(NULL) +
  scale_x_date(date_labels = "%Y-%m-%d") + theme_minimal()

# Checking the Czechs
head(d["country"])
which(d[,"country"] == "Czech Republic")
