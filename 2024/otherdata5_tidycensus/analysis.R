#######################################################################
### Background ########################################################
#######################################################################


#######################################################################
### Load required libraries, fonts, and keys ##########################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("camcorder"), etc
library(tidyverse) ## to format/restructure/plot data
library(tidycensus) ## to access census data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(colorspace) ## for helping with colors
library(keyring) ## for securely tracking API Key
library(socviz) ## for supplemental county-level mapping data
library(mosaic) ## for mapping theme

census_api_key(key_get("census-api-key"))

#######################################################################
### Load data #########################################################
#######################################################################

## Presence and Types of Internet Subscriptions in Household (any broadband)
broadband22 <- get_acs(geography = "county", 
                       variables = c(broadband = "B28002_004"), 
                       year = 2022)

names(broadband22)[which(names(broadband22) == "estimate")] <- "broadband"

## Total Population
totalpop <- get_acs(geography = "county", 
                       variables = c(totalpop = "B01003_001"), 
                       year = 2022)

names(totalpop)[which(names(totalpop) == "estimate")] <- "totalpop"


## merge in supplemental mapping info from socviz package based on FIPS codes
county_full <- left_join(broadband22, county_map, 
                         join_by(GEOID == id)) %>%
                left_join(totalpop, 
                          join_by(GEOID == GEOID,
                                  NAME == NAME))

#######################################################################
### Process data ######################################################
#######################################################################

## estimate % of population with broadband
county_full$pct <- county_full$broadband/county_full$totalpop

## bin data for plotting
county_full$pct_bins <- cut(round(county_full$pct, 2) , breaks = c(0.1, 0.2, 0.3, 0.4, 1))

#######################################################################
### Specify colors ####################################################
#######################################################################

palette <- sequential_hcl(5, "Mint")
text_col <- "grey5"

#######################################################################
### Generate figure ###################################################
#######################################################################

broadband_map <- 
ggplot(data = county_full,
       mapping = aes(x = long, y = lat, fill = pct_bins, group = group)) +
  geom_polygon(color = "gray90", linewidth = 0.05) + 
  coord_equal() +
  theme_map() +
  scale_fill_manual(values = rev(palette)[-1],
                    labels = c("<20%", "20-29%", "30-39%", ">40%")) +
  labs(title = "Broadband internet access (2022)",
       fill = "% population with broadband internet") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5),
        text = element_text(colour = text_col, family = "Barlow")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

#######################################################################
### Save figure #######################################################
#######################################################################

ggsave(plot = broadband_map,
       filename = "broadband.png", 
       dpi = 350, height = 4, width = 6.4, units = "in",
       bg = 'white')

