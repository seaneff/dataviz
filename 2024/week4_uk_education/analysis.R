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

#######################################################################
### Load data #########################################################
#######################################################################

## educational attainment data
tuesdata <- tidytuesdayR::tt_load(2024, week = 4)
edu <- tuesdata$english_education

#######################################################################
### Format data #######################################################
#######################################################################


arrow_positions <- 
  tibble(
    x1 = c(-3, 3.5),
    x2 = c(3, 0.116),
    y1 = c(2, 5), 
    y2 = c(2, 5)
  )

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
color_palette <- rev(c("#5996b7", "#596fb7", "#6a59b7"))

#######################################################################
### Make figure  ######################################################
#######################################################################

set.seed(0418)

arrows_left <- 
  tibble(
    x1 = c(edu[which(edu$town11nm == "Thurnscoe BUASD"),]$education_score - .5,
           edu[which(edu$town11nm == "Great Yarmouth BUASD"),]$education_score - 1,
           edu[which(edu$town11nm == "Basildon BUASD"),]$education_score - 1),

    x2 = c(edu[which(edu$town11nm == "Thurnscoe BUASD"),]$education_score - .1,
           edu[which(edu$town11nm == "Great Yarmouth BUASD"),]$education_score - .1,
           edu[which(edu$town11nm == "Basildon BUASD"),]$education_score - .1),
    
    y1 = c(3.4, 2.25, 1.25),
    y2 = c(3.1, 2.1, 1.1),
  )

arrows_right <- 
  tibble(
    x1 = c(edu[which(edu$town11nm == "Northwood BUASD"),]$education_score + .5,
           edu[which(edu$town11nm == "Harpenden BUA"),]$education_score + 1,
           edu[which(edu$town11nm == "Sutton Coldfield BUASD"),]$education_score + 1),
    
    x2 = c(edu[which(edu$town11nm == "Northwood BUASD"),]$education_score - .1,
           edu[which(edu$town11nm == "Harpenden BUA"),]$education_score - .1,
           edu[which(edu$town11nm == "Sutton Coldfield BUASD"),]$education_score - .1),
    y1 = c(3.4, 2.25, 1.2),
    y2 = c(3.1, 2.1, 1.1),
  )



edu %>%
  filter(income_flag != "Cities") %>%
  filter(complete.cases(income_flag)) %>%
  mutate(income_factor = factor(income_flag, 
                                levels = rev(c("Lower deprivation towns",
                                           "Mid deprivation towns",
                                           "Higher deprivation towns")))) %>%
  ggplot(aes(y = size_flag, x = education_score, fill = income_factor, group = NA)) +
  geom_weave(position = "dodge", linewidth = 0, alpha = 0.8) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.7) +
  geom_curve(
    data = arrows_left, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.8,
    color = "gray20", curvature = -0.2,
    inherit.aes = FALSE) +
  geom_curve(
    data = arrows_right, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.8,
    color = "gray20", curvature = 0.2,
    inherit.aes = FALSE) +
  annotate("text", x = edu[which(edu$town11nm == "Thurnscoe BUASD"),]$education_score - .2, 
           y = 3.53, label = "Thurnscoe") +
  annotate("text", x = edu[which(edu$town11nm == "Great Yarmouth BUASD"),]$education_score - .6, 
           y = 2.39, label = "Great Yarmouth") +
  annotate("text", x = edu[which(edu$town11nm == "Basildon BUASD"),]$education_score - .8, 
           y = 1.4, label = "Basildon") +
  annotate("text", x = edu[which(edu$town11nm == "Northwood BUASD"),]$education_score + .2, 
           y = 3.53, label = "Northwood") +
  annotate("text", x = edu[which(edu$town11nm == "Harpenden BUA"),]$education_score + .6, 
           y = 2.39, label = "Harpenden") +
  annotate("text", x = edu[which(edu$town11nm == "Sutton Coldfield BUASD"),]$education_score + 1.5, 
           y = 1.35, label = "Sutton Coldfield") +
  labs(x = "Educational attainment", 
       y = "",
       title = "Smaller, higher income towns have higher educational attainment",
       subtitle = "Town size and income impact the educational attainment\nof pupils in English towns",
       caption = "Figure based on data from the UK Office for National Statistics (2021)\nvertical bar represents median for each group",
       fill = "Income Level") +
  scale_fill_manual(values = color_palette) + 
  theme_minimal() +
  scale_x_continuous(limits = c(-11, 13)) +
  theme(
    plot.title = element_text(hjust = 0.5), ## center title
    plot.subtitle = element_text(hjust = 0.5), ## center subtitle
    legend.position = "bottom")

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = le_line,
       filename = "le_line.png", 
       dpi = 350, height = 5, width = 7.5, units = "in",
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
