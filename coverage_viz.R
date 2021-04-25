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

install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)

sources <- read_csv(here("data", "cpd-coverage - sources.csv"), col_types=cols(.default="c"))

sources.clean <- sources %>% 
  mutate(year = as.numeric(year),
         decade = case_when(year >= 1940 & year <= 1949 ~ '1940s',
                            year >= 1950 & year <= 1959 ~ '1950s',
                            year >= 1960 & year <= 1969 ~ '1960s',
                            year >= 1970 & year <= 1979 ~ '1970s',
                            year >= 1980 & year <= 1989 ~ '1980s',
                            year >= 1990 & year <= 1999 ~ '1990s',
                            year >= 2000 & year <= 2009 ~ '2000s',
                            year >= 2010 & year <= 2019 ~ '2010s',
                            year >= 2020 ~ '2020')
  )

cops_as_source <- sources.clean %>% 
  group_by(source_pd) %>% 
  summarize(total = n()) %>% 
  mutate(percent = total * 100 / 101) %>% 
  select(source_pd, percent)

 # Test waffle plot on police as source
ggplot(data = cops_as_source, aes(fill = source_pd, values = percent)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "#ffffff", flip = TRUE) +
  scale_fill_manual(values = c("#eab676","#76b5c5", "#063970", "#cccccc")) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() +
  labs(title = "Half of the articles used only the police as a source",
       subtitle = "Article sources from 1940s to 2020",
       caption = "A third of the 101 articles used at least one other source besides the CPD")

cops_as_source.by_decade <- sources.clean %>% 
  group_by(source_pd, decade) %>% 
  summarize(total = n()) 

# Test facet_grid
ggplot(data = cops_as_source.by_decade, aes(fill = source_pd, values = total)) +
  geom_waffle(n_rows = 5, size = 0.5, colour = "#ffffff", flip = TRUE) +
  facet_wrap(~decade, strip.position="bottom", nrow=1) +
  scale_fill_manual(name="Sources", values = c("#eab676","#76b5c5", "#063970", "#cccccc")) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() 

# Look at how sourcing from family changed over time
fam_source <- sources.clean %>% 
  filter(source_pd != "Police only") %>% 
  group_by(source_family, decade) %>% 
  summarize(total = n()) 

# Of the articles that didn't only source from CPD, how many sourced from families?
ggplot(data = fam_source, aes(fill = source_family, values = total)) +
  geom_waffle(n_rows = 5, size = 0.5, colour = "#ffffff", flip = TRUE) +
  facet_wrap(~decade, strip.position="bottom", nrow=1) +
  scale_fill_manual(name="Used family as source", values = c("#154c79", "#e28743")) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() 

# Convert data to long format
sources.long <- sources.clean %>% 
  gather(key="non_pd_source", value="number", source_family:source_community, factor_key=TRUE) %>% 
  mutate(number = case_when(number == "Yes" ~ 1,
                            number == "No" ~ 0),
         non_pd_source = case_when(non_pd_source == "source_family" ~ "Family",
                                   non_pd_source == "source_witness" ~ "Witness",
                                   non_pd_source == "source_jurors" ~ "Jurors",
                                   non_pd_source == "source_attorneys" ~ "Attorneys",
                                   non_pd_source == "source_med" ~ "Medical examiner",
                                   non_pd_source == "source_activists" ~ "Activists and advocates",
                                   non_pd_source == "source_community" ~ "Community members",
                                   non_pd_source == "source_ipra" ~ "Other"))

non_pd_sources <- sources.long %>% 
  filter(!is.na(source_pd) & source_pd != "Police only") %>% 
  group_by(source_pd, non_pd_source) %>% 
  summarize(total = sum(number)) %>% 
  mutate(total = case_when(source_pd == "Not police" ~ -total,
                           source_pd == "Police and..." ~ total))

ggplot(data = non_pd_sources, aes(x=reorder(non_pd_source, total), y=total, fill=source_pd)) +
  geom_bar(stat="identity", width=.6) +
  coord_flip() +
  scale_y_continuous(labels=paste0(as.character(c(seq(5, 0, -5), seq(5, 20, 5))))) +
  scale_fill_manual(values=c("#873e23", "#76b5c5"), name="Source") +
  xlab("Non-police sources") + ylab("Number of articles") + 
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size=0.5, linetype="dotted", colour ="lightgrey"),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white")
  )
  
