#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(ggdist) ## for dotplots with packing
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(showtext) ## for custom fonts

#######################################################################
### Load data and font ################################################
#######################################################################

## educational attainment data
tuesdata <- tidytuesdayR::tt_load(2024, week = 6)
heritage <- tuesdata$heritage
#font_add_google("Hind Siliguri")

#######################################################################
### Format data #######################################################
#######################################################################

h <- heritage %>%
  pivot_longer(!country, names_to = "year", values_to = "count")

h$country <- factor(h$country, levels = c("Denmark", "Norway", "Sweden"))

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
### Define plot figures: colors and fonts  ############################
#######################################################################

text_col <- "grey10"
color_palette <- rev(c("#5996b7", "#596fb7", "#6a59b7"))
font <- "Barlow"

#######################################################################
### Make figure  ######################################################
#######################################################################

lines <- 
  h %>%
  ggplot(aes(x = year, y = count, group = country)) +
  geom_line() +
  facet_wrap(~country, strip.position = "bottom") +
  labs(y = "Number of Sites", 
       x = "",
       title = "World Heritage Sites in Scandanavian Countries",
       caption = "Figure based on data from https://100.datavizproject.com") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.4), face = "bold"), ## center title, make it bigger and bold
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), ## center subtitle, make it bigger
    axis.text.x = element_text(size = rel(1.2)),
    axis.text.y = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    strip.text = element_text(size = rel(1.2), color = "white"),
    strip.background = element_rect(fill = "#375D7C"),
    legend.position = "none",
    text = element_text(colour = text_col,
                        family = font)) 

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = lines,
       filename = "heritage_lines.png", 
       dpi = 350, height = 3.5, width = 7.5, units = "in",
       bg = 'white')

#######################################################################
### Stop recording and save GIF #######################################
#######################################################################

gg_stop_recording()

gg_playback(
  name = file.path("figure_versions/figure_versions.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
