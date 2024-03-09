#######################################################################
### Background ########################################################
#######################################################################


#######################################################################
### Load required libraries, fonts, and keys ##########################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("camcorder"), etc
library(readxl) ## to read in Excel files
library(tidyverse) ## to format/restructure/plot data
library(tidycensus) ## to access census data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(colorspace) ## for helping with colors
library(keyring) ## for securely tracking API Key

census_api_key(key_get("census-api-key"))

options(scipen = 10000)

#######################################################################
### Load data #########################################################
#######################################################################

## urban/rural designations from USDA
urban_rural <- read_excel("data/Ruralurbancontinuumcodes2023.xlsx")
names(urban_rural) <- c("fips", "state","county", "population", "rucc", "description")

## Presence and Types of Internet Subscriptions in Household (any broadband)
any_internet <- get_acs(geography = "county", 
                       variables = c(internet = "B28002_002"), 
                       year = 2022)
names(any_internet) <- c("geoid", "name","variable", "any_internet", "moe_internet")

total_households <- get_acs(geography = "county", 
                        variables = c(households = "B28002_001"), 
                        year = 2022)
names(total_households) <- c("geoid", "name","variable", "total_households", "moe_households")

## Median income
median_income <- get_acs(geography = "county", 
                            variables = c(income = "B07011_001"), 
                            year = 2022)
names(median_income) <- c("geoid", "name","variable", "median_income", "moe_income")

## merge these datasets together
county_full <- any_internet[,c(1,2,4)] %>%
                left_join(total_households[,c(1,2,4)], 
                          join_by(geoid == geoid,
                                  name == name)) %>%
                left_join(median_income[,c(1,2,4)], 
                          join_by(geoid == geoid,
                          name == name)) %>%
                left_join(urban_rural[,c(1,5,6)],
                          join_by(geoid == fips))

#######################################################################
### Process data ######################################################
#######################################################################

## estimate % of population with any internet
county_full$pct <- county_full$any_internet/county_full$total_households

## metro/nonmetro designation
county_full$color_category <- ifelse(county_full$rucc <= 3, "Metro area", "Nonmetro")

#######################################################################
### Specify colors ####################################################
#######################################################################

palette <- rev(c("#c7522a", "#d68a58", "#e5c185", "#f0daa5", "#fbf2c4", "#b8cdab",
             "#74a892", "#3a978c", "#008585"))
text_col <- "grey5"

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
### Generate figure ###################################################
#######################################################################

urban_rural <- ggplot(data = county_full,
       aes(x = median_income, y = pct, color = rucc)) +
  geom_point(alpha = 0.8, size = 0.5) +
  labs(title = "Rural, low-income counties have the lowest rates\nof broadband internet access",
       subtitle = "County broadband internet access by median household income",
       x = "Median annual income (USD)",
       y = "% households with\nbroadband internet access",
       #size = "Total households",
       color = "Rural-Urban Continuum Code (RUCC)") +
  guides(title.hjust = 0.5,
         #size = guide_legend(title.position = "top", label.position = "bottom"),
         color = guide_colourbar(direction = "horizontal", title.position = "top", reverse=TRUE)) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(0.65), face = "bold"),
        plot.sub = element_text(hjust = 0.5, size = rel(0.58)),
        text = element_text(colour = text_col, family = "Barlow"),
        axis.title = element_text(size = rel(0.6)),
        axis.text = element_text(size = rel(0.6)),
        legend.box = "vertical", 
        legend.position = "bottom",
        legend.title = element_text(size = rel(0.5), hjust = 0.55),
        legend.text = element_text(size = rel(0.4)),
        legend.key.width = unit(1,"cm")) +
  scale_x_continuous(labels = label_number(prefix = "$", big.mark = ",")) +
  scale_y_continuous(labels = scales::percent) +
  #scale_size_continuous(breaks = c(50000, 500000, 1000000),
  #                      labels = c(paste(c(50, 500), "thousand"), paste(c(1), "million")),
  #                      range = c(0, 3)) +
  scale_color_gradientn(colours = palette,
                       limits = c(1, 9), breaks = c(1, 9),
                       labels = c("most urban", "most rural"))

#######################################################################
### Save figure #######################################################
#######################################################################

ggsave(plot = urban_rural,
       filename = "rural_income_broadband.png", 
       dpi = 350, height = 1080, width = 1080, units = "px",
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

