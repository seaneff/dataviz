#######################################################################
### Background ########################################################
#######################################################################

## Definitions of key data points, see https://taylor.wjakethompson.com/reference/taylor_songs
## Dancibility: How suitable a track is for dancing. 0.0 = least danceable, 1.0 = most danceable (based on spotify API)
## Valence: Musical positiveness conveyed by the track. 0.0 = low valence (e.g., sad, depressed, angry), 1.0 = high valence (e.g., happy, cheerful, euphoric).

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(ggrepel) ## for help positioning scatterplot labels
library(taylor) ## for custom taylor swift palettes
library(showtext) ## for custom fonts
library(ggtext) ## for combining bold and not bold fonts on axis label

## to review current list of options, if needed
#font_families_google()

font_add_google("Barlow")

#######################################################################
### Load data #########################################################
#######################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 42)

taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

#######################################################################
### Generate Figure ###################################################
#######################################################################

taylor_album_songs$label <- 
  ifelse(taylor_album_songs$track_name %in% 
           c("Shake It Off", "Stay Stay Stay", "Paper Rings", "Hey Stephen (Taylor's Version)",
             "You Need to Calm Down", "How You Get the Girl", "Gorgeous", "I Think He Knows",
             "Cornelia Street", "Clean", "Vigilante Shit", "Delicate", "Maroon", "closure",
             "This Love (Taylor's Version)", "epiphany", "The Archer", "the lakes", "Bigger Than The Whole Sky",
             "willow", "Better Than Revenge", "Mean", "Sweet Nothing", "How You Get The Girl"),
         taylor_album_songs$track_name, "")

valence_vs_danceability <- taylor_album_songs %>% 
  filter(complete.cases(danceability)) %>%
  filter(complete.cases(valence)) %>%
  ggplot(aes(x = danceability, 
             y = valence, 
             label = label)) +
  geom_point(aes(alpha = valence,
                 color = danceability)) +
  geom_point(shape = 21, color = "grey40", stroke = 0.2) +
  labs(x = "", 
       y = "",
       title = "Which Taylor Swift songs are most danceable?",
       subtitle = "Exploring happier vs. sadder TSwift songs based on their danceability",
       caption = "Valence and Danceability assessed based on Spotify API\nVisualization by Steph Eaneff") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(colour = "grey20", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = rel(0.95)),
        axis.text.x = element_markdown(size = rel(1.1)),
        axis.text.y = element_markdown(size = rel(1.1)),
        axis.title = element_text(size = rel(1.1)),
        plot.caption = element_text(size = rel(0.6))) +
  scale_x_continuous(breaks = c(0.4, .8),
                     labels = c("**Less danceable**<br>(per Spotify)", 
                                "**More danceable**<br>(per Spotify)")) +
  scale_y_continuous(breaks = c(0.2, 0.9),
                     labels = c("**Lower valence**<br>(e.g., sad, depressed, angry)", 
                                "**Higher valence**<br>(e.g., happy, cheerful, euphoric)")) +
  scale_alpha(range = c(0.3, 1)) +
  scale_color_gradient(low = "#0000FF", high = "#EA5F94") +
  geom_text_repel(size = 2.8, min.segment.length = 0.1, family = "Barlow")
valence_vs_danceability

ggsave(plot = valence_vs_danceability,
       filename = "valence_vs_danceability.png", 
       dpi = 350, height = 4, width = 6.5, units = "in",
       bg = 'white')

