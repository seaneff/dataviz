#######################################################################
### Background ########################################################
#######################################################################

# Edouard Mathieu, Fiona Spooner, Saloni Dattani, Hannah Ritchie and Max Roser (2022) - "Mpox (monkeypox)". 
# Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/monkeypox' 
# [Online Resource]

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("camcorder"), etc
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(directlabels)

#######################################################################
### Load data #########################################################
#######################################################################

mpox <- read.csv("https://raw.githubusercontent.com/owid/monkeypox/main/owid-monkeypox-data.csv")

#######################################################################
### Format data #######################################################
#######################################################################

mpox$date <- as.Date(mpox$date)

## create artifacts to make plot of cumulative confirmed caseload
mpox$color_cumulative <- "gray75"
mpox$label_cumulative <- ""

mpox[which(mpox$location == "Gibraltar"),]$color_cumulative <- "#172869"
mpox[which(mpox$location == "Spain"),]$color_cumulative <- "#0076BB"
mpox[which(mpox$location == "Peru"),]$color_cumulative <- "#1BB6AF"
mpox[which(mpox$location == "Gibraltar"),]$label_cumulative <- "Gibraltar"
mpox[which(mpox$location == "Spain"),]$label_cumulative <- "Spain"
mpox[which(mpox$location == "Peru"),]$label_cumulative <- "Peru"

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

text_col <- "grey1"

#######################################################################
### Make figure #######################################################
#######################################################################

cumulative <- ggplot(mpox, aes(x = date, y = total_cases_per_million, group = location, color = color_cumulative)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_color_identity() +
  theme_minimal() +
  labs(title = "Confirmed mpox cases by country",
       subtitle = "Gibraltar, Spain, and Peru have the highest\ncumulative per capita confirmed caseloads",
       caption = "based on confirmed mpox cases as of November 2023 based on data reported to WHO",
       x = "",
       y = "Confirmed mpox cases\nper million population (cumulative)\n") +
  scale_x_date(date_labels = "%b %Y", 
               breaks = c(as.Date("2022-07-01"),
                          as.Date("2022-10-01"),
                          as.Date("2023-01-01"),
                          as.Date("2023-04-01"),
                          as.Date("2023-07-01"),
                          as.Date("2023-10-01")),
               expand = c(0.15, 0)) + 
theme(text = element_text(family = "Barlow", colour = text_col), ## fonts should be Barlow, other than title
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5), ## make title bold, centered, and larger
        plot.subtitle = element_text(size = rel(1.1), hjust = 0.5),
        axis.text = element_text(size = rel(1)), 
        strip.text = element_text(size = rel(1.1), face = "bold"),
        axis.title = element_text(size = rel(1.2)),  ## make axis title larger 
        plot.caption = element_text(size = rel(0.7)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_dl(aes(label = label_cumulative), 
          method = "last.points") 

cumulative

#######################################################################
### Save Images #######################################################
#######################################################################

ggsave(plot = cumulative,
       filename = "cumulative_monkeypox.png", 
       dpi = 350, height = 5, width = 7, units = "in",
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
