#######################################################################
### Background ########################################################
#######################################################################

## Definitions of key data points: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IG0UN2
## Summary: https://electionlab.mit.edu/articles/new-report-how-we-voted-2022

## Idea: What percent of the population votes democratic
  
## Inspiration sources: 
## https://github.com/gkaramanis/tidytuesday/tree/master/2023/2023-week_36
## https://stackoverflow.com/questions/42422669/arrange-ggplot-facets-in-the-shape-of-the-us

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(ggrepel) ## for help positioning scatterplot labels
library(showtext) ## for custom fonts
library(ggtext) ## for combining bold and not bold fonts on axis label
library(camcorder) ## to record R images
library(stringr) ## for nice title case

## to review current list of options, if needed
#font_families_google()

font_add_google("Barlow")

#######################################################################
### Load data #########################################################
#######################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 45)
house <- tuesdata$house

#######################################################################
### Define user-generated functions ###################################
#######################################################################

## Return second highest number in a vector #
secondHighest <-  function(x) {
  sorted <- unique(x)
  sort(sorted, decreasing = TRUE)[2L]
}

#######################################################################
### Format data #######################################################
#######################################################################

data_long <- house %>%
  filter(state != "ALASKA") %>% ## exclude Alaska and Hawaii for now
  filter(state != "HAWAII") %>% ## exclude Alaska and Hawaii for now
  filter(runoff == FALSE) %>% ## exclude runoff elections
  filter(unofficial == FALSE) %>% ##exclude unofficial results // from data source: TRUE/FALSE indicator for unofficial result (to be updated later); this appears only for 2018 data in some cases
  filter(special == FALSE) %>% ## exclude special elections
  filter(stage == "GEN") %>% ## just look at general elections (not primaries, limited data there anyway)
  mutate(state_label = str_to_title(state)) %>% ## pretty formatted state name
  mutate(state_label = recode(state_label, "DISTRICT OF COLUMBIA" = "Washington DC")) %>%
  group_by(year, state_label) %>% ## gone row per year and state
  summarize(total_votes = sum(candidatevotes, na.rm = TRUE),
            republican_votes = sum(ifelse(party == "REPUBLICAN", candidatevotes, 0), na.rm = TRUE),
            democratic_votes = sum(ifelse(party %in% c("DEMOCRAT", "DEMOCRATIC"), candidatevotes, 0), na.rm = TRUE),
            other_votes = sum(ifelse(!party %in% c("DEMOCRAT", "DEMOCRATIC", "REPUBLICAN"), candidatevotes, 0)),
            pct_republican = republican_votes/total_votes,
            pct_democratic = democratic_votes/total_votes,
            pct_other = other_votes/total_votes) %>%
  ## weird gap between 2006 and 2020
  filter(year <= 2006)

data_wide <- pivot_longer(data_long,
                          cols = c(pct_republican:pct_other),
                          names_to = "party_raw",
                          values_to = "percent") %>%
  mutate(label = factor(recode(party_raw, 
                               pct_democratic = "Democratic",
                               pct_republican  = "Republican",
                               pct_other = "Other party"),
    levels = c("Democratic", "Republican",  "Other party")))

#######################################################################
### Make graphic ######################################################
#######################################################################

## thanks to https://stackoverflow.com/questions/42422669/arrange-ggplot-facets-in-the-shape-of-the-us
# Create unique blank strip labels for empty facets
bl = sapply(1:37, function(n) paste(rep(" ", n),collapse = ""))

data_wide$state_reorder <- factor(data_wide$state_label,
                                  levels = c(bl[1:10], "Maine",
                                             bl[11:19], "Vermont", "New Hampshire",
                                             "Washington", "Idaho", "Montana", "North Dakota", "Minnesota", "Illinois", "Wisconsin", "Michigan", "New York", "Massachusetts", "Rhode Island",
                                             "Oregon", "Nevada", "Wyoming", "South Dakota", "Iowa", "Indiana", "Ohio", "Pennsylvania", "New Jersey", "Connecticut", bl[20],
                                             "California", "Utah", "Colorado", "Nebraska", "Missouri", "Kentucky", "West Virginia", "Virginia", "Maryland", "Washington DC", bl[21],
                                             bl[22], "Arizona", "New Mexico", "Kansas", "Arkansas", "Tennessee", "North Carolina", "South Carolina", "Delaware", bl[23:24],
                                             bl[25:27], "Oklahoma", "Louisiana", "Mississippi", "Alabama", "Georgia", bl[28:29],
                                             bl[30:33], "Texas", bl[34:37], "Florida"))


# to record versions of the plot, see https://github.com/thebioengineer/camcorder
# didn't do this, save for next time
# gg_record(
#   dir = file.path(getwd()), # where to save the recording
#   device = "png", # device to use to save images
#   width = 12,      # width of saved image
#   height = 8,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )

us_facet <- data_wide %>%
  filter(state_reorder != "Minnesota") %>%
  ggplot(aes(x = year, y = percent, color = label)) +
  geom_line() +
  facet_wrap(~state_reorder, ncol = 11, drop = F, strip.position = "bottom") +
  theme_classic() +
  theme(text = element_text(colour = "grey10", family = "Barlow"),
        axis.text = element_blank(), ## no text along axis
        strip.background = element_blank(), ## no grey background
        axis.line = element_blank(), ## no axis line
        axis.ticks = element_blank(), ## no axis ticks
        plot.title = element_markdown(hjust = 0.5, ## center the title,
                                      size = rel(2),  ## make it bigger
                                      face = "bold"), ## make it bold
        plot.subtitle = element_markdown(hjust = 0.5, ## center the subtitle
                                         size = rel(1.5)), ## make it bigger
        plot.caption = element_text(size = rel(0.75)),## make caption comparably smaller
        legend.direction = "horizontal", ## legend options listed horizontally
        legend.position = "top" ## legend on top
        ) +
  scale_color_manual(values = c( "#013364","#d30b0d","gray50")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "",
       color = "Party affiliation",
       title = "Votes for the US House of Representatives",
       subtitle = "% of annual statewide votes for the house, by party (1976-2006)",
       caption = "Data from Minnesota and Washington DC not available\nNote that not all parties have candidates in each district or each election year. Percentages reflect totals of statewide votes\nand not specific margins, given that elections occur at the district level") 
us_facet

ggsave(plot = us_facet,
       filename = "pct_votes_by_state.png", 
       dpi = 350, height = 8, width = 12, units = "in",
       bg = 'white')

# didn't do this, save for next time
# gg_playback(
#   name = file.path(tempdir(), "recording", "image_versions.gif"),
#   first_image_duration = 5,
#   last_image_duration = 15,
#   frame_duration = .4,  
#   image_resize = 800)
#gg_stop_recording()
