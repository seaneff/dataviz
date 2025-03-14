#######################################################################
### Background ########################################################
#######################################################################

## Data from UN Standard Projections
## https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used
## Downloaded 11 March 2025

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("readxl"), etc
library(tidyverse) ## to format/restructure/plot data
library(scales) ## for commas on the x axis
library(directlabels) ## for labels to right of lines
library(ggtext) ## for bold text in ggplot2
library(readxl) ## to read in Excel files
library(geomtextpath) ## to add curved text in ggplot2 plots

#######################################################################
### Load data #########################################################
#######################################################################

data <- read_excel("data/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.xlsx", skip = 16)

#######################################################################
### Format data #######################################################
#######################################################################

clean_data_regional <- data |>
  filter(Type == "Region") |>
  rename("year" = "Year",
         "region" = "Region, subregion, country or area *",
         "fertility_rate" = "Total Fertility Rate (live births per woman)") |>
  mutate(fertility_rate = as.numeric(fertility_rate)) |>
  mutate(region = ifelse(region == "Latin America and the Caribbean", "Latin America", region)) |>
  select(region, year, fertility_rate)

clean_data_world <-  data |>
  filter(Type == "World") |>
  rename("year" = "Year",
         "fertility_rate" = "Total Fertility Rate (live births per woman)") |>
  mutate(fertility_rate = as.numeric(fertility_rate)) |>
  select(year, fertility_rate)

#######################################################################
### Define plot figures: colors  ######################################
#######################################################################

text_col <- "grey1"
line_col <- "grey1"
font_family <- "Crimson"

#######################################################################
### Design figure: overall data #######################################
#######################################################################

world_figure <- clean_data_world |>
  ggplot(aes(x = year, y = fertility_rate)) +
  labs(x = "", y = "**Average Lifetime Children**<br>*(Total Fertility Rate)*",
       title = "Women are having fewer and fewer children",
       subtitle = "Global fertility rates have steadiliy declined since the 1960s",
       caption = "**Source: UN World Population Prospects 2024**<br>*The total fertility rate for a year is calculated based on the total number of children that would be born to each woman<br>if she were to live to the end of her child-bearing years and give birth based on current age-specific fertility rates.*") +
  ## baby boomers
  geom_rect(aes(xmin = 1950, xmax = 1966, ymin = 0, ymax = 7), fill = "#d7533c") +
  annotate("text", x = 1950 + 1, y = 0.2, label = "Baby Boomers", hjust = 0, vjust = 0, size = 3.2) +
  ## generation X
  geom_rect(aes(xmin = 1966, xmax = 1981, ymin = 0, ymax = 7), fill = "#e37b5f") +
  annotate("text", x = 1966 + 1, y = 0.2, label = "Generation X", hjust = 0, vjust = 0, size = 3.2) +
  ## millenials
  geom_rect(aes(xmin = 1981, xmax = 1997, ymin = 0, ymax = 7), fill = "#ec9f86") +
  annotate("text", x = 1981 + 1, y = 0.2, label = "Millenials", hjust = 0, vjust = 0, size = 3.2) +
  ## generation Z
  geom_rect(aes(xmin = 1997, xmax = 2011, ymin = 0, ymax = 7), fill = "#f3c2b1") +
  annotate("text", x = 1997 + 1, y = 0.2, label = "Generation Z", hjust = 0, vjust = 0, size = 3.2) +
  ## generation alpha
  geom_rect(aes(xmin = 2011, xmax = 2023, ymin = 0, ymax = 7), fill = "#f7e5de") +
  annotate("text", x = 2011 + 1, y = 0.2, label = "Generation\nAlpha", hjust = 0, vjust = 0, size = 3.2) +
  geom_line(lwd = 1.2, col = line_col) +
  scale_x_continuous(breaks = seq(min(clean_data_world$year), max(clean_data_world$year), by = 10)) +
  theme(legend.position = "none",
        text = element_text(family = font_family, colour = text_col), ## fonts should be Barlow, other than title
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5), ## make title bold, centered, and larger
        plot.subtitle = element_text(face = "italic", size = rel(1.1), hjust = 0.5),
        axis.title.y = element_markdown(lineheight = 1.2), # Enable Markdown for y-axis title
        plot.caption = element_markdown(size = 8), # Enable Markdown for caption
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.background = element_blank()) 

world_figure

ggsave(plot = world_figure,
       filename = "world_figure.png",
       dpi = 350, height = 4, width = 6, units = "in",
       bg = 'white')

#######################################################################
### Design figure: regional data ######################################
#######################################################################

regional_figure <- clean_data_regional |>
  ggplot(aes(x = year, y = fertility_rate)) +
  labs(x = "", y = "**Average Lifetime Children**<br>*(Total Fertility Rate)*",
       title = "Women are having fewer and fewer children",
       subtitle = "Global fertility rates have steadiliy declined since the 1960s",
       caption = "**Source: UN World Population Prospects 2024**<br>*The total fertility rate for a year is calculated based on the total number of children that would be born to each woman<br>if she were to live to the end of her child-bearing years and give birth based on current age-specific fertility rates.*") +
  ## baby boomers
  geom_rect(aes(xmin = 1950, xmax = 1966, ymin = 0, ymax = 7), fill = "#d7533c") +
  annotate("text", x = 1950 + 1, y = 0.2, label = "Baby Boomers", hjust = 0, vjust = 0, size = 3.2) +
  ## generation X
  geom_rect(aes(xmin = 1966, xmax = 1981, ymin = 0, ymax = 7), fill = "#e37b5f") +
  annotate("text", x = 1966 + 1, y = 0.2, label = "Generation X", hjust = 0, vjust = 0, size = 3.2) +
  ## millenials
  geom_rect(aes(xmin = 1981, xmax = 1997, ymin = 0, ymax = 7), fill = "#ec9f86") +
  annotate("text", x = 1981 + 1, y = 0.2, label = "Millenials", hjust = 0, vjust = 0, size = 3.2) +
  ## generation Z
  geom_rect(aes(xmin = 1997, xmax = 2011, ymin = 0, ymax = 7), fill = "#f3c2b1") +
  annotate("text", x = 1997 + 1, y = 0.2, label = "Generation Z", hjust = 0, vjust = 0, size = 3.2) +
  ## generation alpha
  geom_rect(aes(xmin = 2011, xmax = 2023, ymin = 0, ymax = 7), fill = "#f7e5de") +
  annotate("text", x = 2011 + 1, y = 0.2, label = "Generation\nAlpha", hjust = 0, vjust = 0, size = 3.2) +
  geom_textpath(data = clean_data_regional[-which(clean_data_regional$region %in% c("Asia", "Latin America", "Oceania", "Europe")),], 
                aes(label = region, hjust = 0.7), size = 2.8) +
  geom_textpath(data = clean_data_regional[which(clean_data_regional$region == "Asia"),], 
                aes(label = region, hjust = 0.35), size = 2.8) +
  geom_textpath(data = clean_data_regional[which(clean_data_regional$region == "Latin America"),], 
                aes(label = region, hjust = 0.4), size = 2.8) +
  geom_textpath(data = clean_data_regional[which(clean_data_regional$region == "Oceania"),], 
                aes(label = region, hjust = 0.45), size = 2.8) +
  geom_textpath(data = clean_data_regional[which(clean_data_regional$region == "Europe"),], 
                aes(label = region, hjust = 0.72), size = 2.8) +
  scale_x_continuous(breaks = seq(min(clean_data_regional$year), max(clean_data_regional$year), by = 10)) +
  theme(legend.position = "none",
        text = element_text(family = font_family, colour = text_col), ## fonts should be Barlow, other than title
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5), ## make title bold, centered, and larger
        plot.subtitle = element_text(face = "italic", size = rel(1.1), hjust = 0.5),
        axis.title.y = element_markdown(lineheight = 1.2), # Enable Markdown for y-axis title
        plot.caption = element_markdown(size = 8), # Enable Markdown for caption
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.background = element_blank()) 


regional_figure

ggsave(plot = regional_figure,
       filename = "regional_figure.png",
       dpi = 350, height = 4, width = 6, units = "in",
       bg = 'white')

#######################################################################
### Save GIF ##########################################################
#######################################################################

#gg_stop_recording()

# gg_playback(
#   name = file.path("figure_versions/figure_versions.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )