#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis

#######################################################################
### Load data #########################################################
#######################################################################

## life expectancy data
tuesdata <- tidytuesdayR::tt_load(2023, week = 49)
le <- tuesdata$life_expectancy
names(le)[which(names(le) == "Code")] <- "iso_code"

## supplementary data
countrydata <- read.table("country_data.tsv", sep = '\t', header = TRUE)

#######################################################################
### Format data #######################################################
#######################################################################

data <- merge(le, countrydata, by = "iso_code")
data$income_group_factor <- factor(data$income_group_label, levels = c("High income",
                                                                       "Upper middle income",
                                                                       "Lower middle income",
                                                                       "Low income"))

#######################################################################
### Start recording ###################################################
#######################################################################

gg_record(
  dir = file.path("figure_versions"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

#######################################################################
### Define plot figures: colors  ######################################
#######################################################################

text_col <- "grey10"
color_palette <- c("#008837", "#a6dba0", "#c2a5cf", "#7b3294")

#######################################################################
### Make figure  ######################################################
#######################################################################

le_line <- data %>%
  filter(Year >= 1950) %>%
  filter(Year <= 2023) %>%
  filter(complete.cases(income_group_label)) %>%
  ggplot(aes(x = Year, y = LifeExpectancy, group = Entity, color = income_group_factor)) +
  geom_line() +
  scale_color_manual(values = color_palette) +
  ## specify titles and captions
  labs(title = "Life expectancy depends on country income (and always has)",
       x = "",
       y = "Average life expectancy\n(years)",
       subtitle = "Life expectency data, per country, from 1950-present",
       color = "World Bank income group", 
       caption = "Life expectancy data from Our World in Data and income group data from World Bank as of 2019") + 
  theme_minimal() +
  theme(text = element_text(colour = text_col, family = "Barlow"),
        plot.title = element_text(size = rel(1.5),  ## make title bigger
                                      face = "bold"), ## make title bold
        plot.subtitle = element_text(size = rel(1.25)),
        plot.caption = element_text(size = rel(0.75)), ## make caption slightly smaller
        legend.position = "top")  ## legend on top) 

le_line

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = le_line,
       filename = "le_line.png", 
       dpi = 350, height = 5, width = 7.5, units = "in",
       bg = 'white')

#######################################################################
### Save GIF ##########################################################
#######################################################################

gg_playback(
  name = file.path("figure_versions/figure_versions.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
