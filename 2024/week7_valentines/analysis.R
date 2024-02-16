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
library(geomtextpath) ## for adding label next to line

#######################################################################
### Load data and font ################################################
#######################################################################

## valentines data spending data
historical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')
gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')
gifts_gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_gender.csv')

#######################################################################
### Format data #######################################################
#######################################################################


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
color_palette <- rev(c("#a50b5e", "#4e6ac8", "#00b0a9"))
font <- "Barlow"

#######################################################################
### Make figure  ######################################################
#######################################################################

lines <- historical_spending %>%
  reshape2::melt(id.vars = "Year") %>%
  filter(variable != "PercentCelebrating") %>%
  filter(variable != "PerPerson") %>%
  filter(variable != "Jewelry") %>%
  filter(variable != "EveningOut") %>%
  filter(variable != "GiftCards") %>%
  filter(variable != "Clothing") %>%
  mutate(variable = recode(variable, GreetingCards = "Greeting cards")) %>%
  ggplot(aes(x = Year, y = value, color = variable, group = variable)) +
  labs(y = "Average amount spent ($ USD)", 
       x = "",
       title = "How much money do Americans spend on Valentines Day?",
       subtitle = "Average Valentines Day spending over time",
       caption = "Based on data from the US National Retail Federation") +
  theme_minimal() +
  scale_color_manual(values = color_palette, guide = "none") +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.1), face = "bold"), ## center title, make it bigger and bold
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), ## center subtitle, make it bigger
    axis.text.x = element_text(size = rel(1)),
    axis.text.y = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1)),
    strip.text = element_text(size = rel(1), color = "white"),
    strip.background = element_rect(fill = "#f7eef7"),
    text = element_text(colour = text_col,
                        family = font)) +
    scale_x_continuous(breaks = pretty_breaks()) +
    geom_textline(aes(label = variable, hjust = variable), spacing = 60, family = font) +
    scale_hjust_manual(values = c(.6, .6, .81)) 

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = lines,
       filename = "valentines_day_spend.png", 
       dpi = 350, height = 3.5, width = 5, units = "in",
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
