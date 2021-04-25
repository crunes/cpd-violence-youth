# Author: Charmaine Runes
# For South Side Weekly
# Project: Young People Killed by CPD

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(stringr)
library(here)
library(zoo)
library(ggtext)

data <- read_csv(here("data", "CPD_killings - edited.csv"), col_types=cols(.default="c"))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Add variables
data <- data %>% 
  mutate(year = as.numeric(`Year`),
         age = as.numeric(`Age at Death`),
         decade = case_when(year >= 1940 & year <=1949 ~ '1940s',
                            year >= 1950 & year <= 1959 ~ '1950s',
                            year >= 1960 & year <= 1969 ~ '1960s',
                            year >= 1970 & year <= 1979 ~ '1970s',
                            year >= 1980 & year <= 1989 ~ '1980s',
                            year >= 1990 & year <= 1999 ~ '1990s',
                            year >= 2000 & year <= 2009 ~ '2000s',
                            year >= 2010 & year <= 2019 ~ '2010s',
                            year >= 2020 ~ '2020s'),
         race = replace_na(Race, 'Unknown')
         )

# Group by year
data.by_decade <- data %>% 
  group_by(decade) %>% 
  summarise(count = n())

chart_by_decade <- data.by_decade %>% 
  ggplot(aes(x=decade, y=count)) +
  geom_bar(stat="identity", data=data.by_decade, aes(x=decade, y=count), fill='#1d6996') +
  scale_y_continuous(breaks=seq(0, 13, 2)) +
  #geom_line(aes(x=decade, y=count, group=1), color='#e28743') +
  xlab("") + ylab("Number of young people killed by CPD") + 
  theme(
    plot.title = element_markdown(color="#2b2d2f", size=14),
    plot.subtitle = element_markdown(size=12),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(size=0.5, linetype="dotted", colour ="lightgrey"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white"),
    legend.position = "none"
  )

chart_by_decade + labs(
  title = "<b><span style = 'color:#1d6996;'>Thirty-nine young people</span> killed in Chicago Police Department<br>
  interactions between 1944 and 2020</b>", 
  subtitle = "Number of young people under 16 years killed in CPD interactions, by decade")

data.by_age <- data %>% 
  group_by(age) %>% 
  summarise(count = n())

chart_by_age <- data %>% 
  ggplot(aes(x=age)) +
  geom_density(fill='#1d6996', color='#1d6996', alpha=0.7) +
  xlab("Age") + ylab("Percent of young people killed in CPD interactions") + 
  theme(
    plot.title = element_markdown(color="#2b2d2f", size=14),
    plot.subtitle = element_markdown(size=12),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(size=0.5, linetype="dotted", colour ="lightgrey"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white"),
    legend.position = "none"
  )

chart_by_age + labs(
  title = "<b>Most youth killed in interactions with CPD between 1944<br>
  and 2020 were 15 years old</b>", 
  subtitle = "Percent of young people under 16 years killed in CPD interactions, by age")

chart_by_age_race <- data %>% 
  ggplot(aes(x=age)) +
  geom_density(aes(fill=factor(data$race)), alpha=0.5) +
  scale_fill_manual(values=cbPalette) +
  xlab("Age") + ylab("Percent of young people killed in CPD interactions") + 
  theme(
    plot.title = element_markdown(color="#2b2d2f", size=14),
    plot.subtitle = element_markdown(size=12),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(size=0.5, linetype="dotted", colour ="lightgrey"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white")
  )

chart_by_age_race # Update the legend title
  

data.by_age %>% 
  ggplot(aes(x=age, y=count)) +
  geom_bar(stat="identity", data=data.by_age, aes(x=age, y=count), fill='#1d6996') +
  geom_text(aes(label=age)) +
  scale_fill_manual(values=cbPalette) +
  xlab("Age") + ylab("Percent of young people killed in CPD interactions") + 
  theme(
    plot.title = element_markdown(color="#2b2d2f", size=14),
    plot.subtitle = element_markdown(size=12),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(size=0.5, linetype="dotted", colour ="lightgrey"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white")
  )
