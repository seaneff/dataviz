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
tuesdata <- tidytuesdayR::tt_load(2024, week = 4)
edu <- tuesdata$english_education
font_add_google("Hind Siliguri")

#######################################################################
### Format data #######################################################
#######################################################################

## specify arrow postions (left side of plot)
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

## specify arrow postions (right side of plot)
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

edu$income_label <- case_match(edu$income_flag, 
                               "Cities" ~ "Cities", 
                               "Higher deprivation towns" ~ "Lower income", 
                               "Mid deprivation towns" ~ "Medium income", 
                               "Lower deprivation towns" ~ "Higher income", 
                               .default = NA)

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
font <- "Hind Siliguri"

#######################################################################
### Make figure  ######################################################
#######################################################################

set.seed(0418)

#edu_dot <- 
edu %>%
  filter(income_flag != "Cities") %>%
  filter(complete.cases(income_label)) %>%
  mutate(income_factor = factor(income_label, 
                                levels = c("Lower income",
                                           "Medium income",
                                           "Higher income"))) %>%
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
           y = 3.53, label = "Thurnscoe", colour = text_col, family = font, size = 3.7) +
  annotate("text", x = edu[which(edu$town11nm == "Great Yarmouth BUASD"),]$education_score - .6, 
           y = 2.39, label = "Great Yarmouth", colour = text_col, family = font, size = 3.7) +
  annotate("text", x = edu[which(edu$town11nm == "Basildon BUASD"),]$education_score - .8, 
           y = 1.4, label = "Basildon", colour = text_col, family = font, size = 3.7) +
  annotate("text", x = edu[which(edu$town11nm == "Northwood BUASD"),]$education_score + .2, 
           y = 3.53, label = "Northwood", colour = text_col, family = font, size = 3.7) +
  annotate("text", x = edu[which(edu$town11nm == "Harpenden BUA"),]$education_score + .6, 
           y = 2.39, label = "Harpenden", colour = text_col, family = font, size = 3.7) +
  annotate("text", x = edu[which(edu$town11nm == "Sutton Coldfield BUASD"),]$education_score + 1.5, 
           y = 1.35, label = "Sutton Coldfield", colour = text_col, family = font, size = 3.7) +
  labs(x = "Educational attainment", 
       y = "",
       title = "Smaller, higher income towns have higher educational attainment",
       subtitle = "Town size and income impact the educational attainment\nof pupils in English towns",
       caption = "Figure based on data from the UK Office for National Statistics (2021)\nvertical bar represents median for each group\ndeprivation levels reported by UK dataset translated into income categories for the purpose of data visualization",
       fill = "Income Level") +
  scale_fill_manual(values = color_palette) + 
  theme_minimal() +
  scale_x_continuous(limits = c(-11, 13)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.25), face = "bold"), ## center title, make it bigger and bold
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), ## center subtitle, make it bigger
    axis.text.x = element_text(size = rel(1.2)),
    axis.text.y = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)),
    plot.caption = element_text(size = rel(0.7)),
    legend.position = "bottom",
    text = element_text(colour = text_col,
                        family = font)) 

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = edu_dot,
       filename = "edu_dot.png", 
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
