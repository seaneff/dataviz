#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have dplyr downloaded, start by running:
## install.packages("dplyr"), etc
library(showtext) ## for custom fonts
library(syuzhet)
library(tidyr)
library(ggplot2)

font_add_google("Barlow", "Barlow")

#######################################################################
### Load data  ########################################################
#######################################################################

hamlet <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/hamlet.csv')
macbeth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/macbeth.csv')
romeo_juliet <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/romeo_juliet.csv')

######################################################################
### Format data #######################################################
#######################################################################

combined <- rbind.data.frame(
  cbind.data.frame(play = "Hamlet", hamlet[-which(hamlet$character == "[stage direction]"),]),
  cbind.data.frame(play = "Macbeth", macbeth[-which(macbeth$character == "[stage direction]"),]),
  cbind.data.frame(play = "Romeo and Juliet", romeo_juliet[-which(romeo_juliet$character == "[stage direction]"),]))

## calculate sentiment (syuzhet)
combined$sentiment_syuzhet <- get_sentiment(combined$dialogue, method = "syuzhet")

## NRC sentiment and emotions
combined$nrc_sentiment <- get_nrc_sentiment(combined$dialogue)

######################################################################
### Summarize and aggregate data  #####################################
#######################################################################

play_act <- combined %>%
  group_by(play, act) %>%
  summarize(play_act = unique(paste(play, act)),
            n_lines = n(),
            avg_sentiment = mean(sentiment_syuzhet),
            
            ## counts
            count_anger = sum(nrc_sentiment["anger"]),
            count_anticipation = sum(nrc_sentiment["anticipation"]),
            count_disgust = sum(nrc_sentiment["disgust"]),
            count_fear = sum(nrc_sentiment["fear"]),
            count_joy = sum(nrc_sentiment["joy"]),
            count_sadness= sum(nrc_sentiment["sadness"]),
            count_surprise = sum(nrc_sentiment["surprise"]),
            count_trust = sum(nrc_sentiment["trust"]),
            
            ## percentages (mean of 0 and 1 is percent of 1)
            pct_anger = mean(unlist(nrc_sentiment["anger"])),
            pct_anticipation = mean(unlist(nrc_sentiment["anticipation"])),
            pct_disgust = mean(unlist(nrc_sentiment["disgust"])),
            pct_fear = mean(unlist(nrc_sentiment["fear"])),
            pct_joy = mean(unlist(nrc_sentiment["joy"])),
            pct_sadness= mean(unlist(nrc_sentiment["sadness"])),
            pct_surprise = mean(unlist(nrc_sentiment["surprise"])),
            pct_trust = mean(unlist(nrc_sentiment["trust"])))


play_act_character <- combined %>%
  group_by(play, act, character) %>%
  summarize(play_act = unique(paste(play, act)),
            n_lines = n(),
            avg_sentiment = mean(sentiment_syuzhet),
            
            ## counts
            count_anger = sum(nrc_sentiment["anger"]),
            count_anticipation = sum(nrc_sentiment["anticipation"]),
            count_disgust = sum(nrc_sentiment["disgust"]),
            count_fear = sum(nrc_sentiment["fear"]),
            count_joy = sum(nrc_sentiment["joy"]),
            count_sadness= sum(nrc_sentiment["sadness"]),
            count_surprise = sum(nrc_sentiment["surprise"]),
            count_trust = sum(nrc_sentiment["trust"]),
            
            ## percentages (mean of 0 and 1 is percent of 1)
            pct_anger = mean(unlist(nrc_sentiment["anger"])),
            pct_anticipation = mean(unlist(nrc_sentiment["anticipation"])),
            pct_disgust = mean(unlist(nrc_sentiment["disgust"])),
            pct_fear = mean(unlist(nrc_sentiment["fear"])),
            pct_joy = mean(unlist(nrc_sentiment["joy"])),
            pct_sadness= mean(unlist(nrc_sentiment["sadness"])),
            pct_surprise = mean(unlist(nrc_sentiment["surprise"])),
            pct_trust = mean(unlist(nrc_sentiment["trust"])))
            
#######################################################################
### Visualize data ####################################################
#######################################################################

# get the name and the y position of each label
label_data <- counts
angle <- 90 - 360 * (label_data$id-0.5) /nrow(label_data)     # substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1.05, -0.05)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

## generate figure
filled_plot <- play_act_character %>%
  filter(character %in% c("Romeo", "Juliet")) %>%
  filter(play == "Romeo and Juliet") %>%
  # Reshape to long format
  pivot_longer(cols = starts_with("pct_"), 
               names_to = "emotion", 
               values_to = "pct_value") %>%
  # rank emotions by percentage per play, act, character
  group_by(play, act, character) %>%
  mutate(rank = rank(-pct_value)) %>%  # Negative sign to rank from highest to lowest
  # Create an indicator for the top two emotions
  mutate(top_emotion_alpha = ifelse(rank <= 2, 1, 0.4)) %>%
  mutate(emotion_factor = 
           factor(gsub("pct_", "", emotion),
                  levels = c("anger", "sadness",
                             "disgust", "fear", 
                             "surprise", "anticipation",
                             "trust", "joy"))) %>%
  mutate(angle = 90 - 360 * (as.numeric(emotion_factor)-0.5) /8) %>%
  mutate(angle_adj = ifelse(angle < -90, angle+180, angle)) %>%
  ggplot(aes(y = pct_value, 
             x = emotion_factor, 
             fill = emotion_factor, 
             group = emotion_factor,
             alpha = top_emotion_alpha)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = emotion_factor,  
                x = emotion_factor, y = 0.3,
                angle = angle_adj),
            position = position_dodge(width = 0.9),
            vjust = .3) + 
  facet_grid(character ~ act) +
  coord_polar() +
  labs(title = "From Trust to Tragedy: Emotional Shifts in Romeo and Juliet") +
  ## specify theme
  theme_minimal() +
  theme(legend.position = "none", ## no legend
        text = element_text(family = "Barlow"), ## fonts should be Barlow
        axis.ticks = element_blank(), ## no ticks on axis
        plot.title = element_text(hjust = 0.5, size = 22), ## center the title and make it larger
        strip.text = element_text(size = 18), ## make facet labels larger
        strip.background = element_rect(fill = "#2E4C58"), ## fill the facet grid elements with dark teal
        axis.text = element_blank(), ## avoid weird polar axis
        axis.title = element_blank()) + ## avoid x and y axis labels
  scale_alpha_identity() +
  scale_fill_manual(values = c("#D1495B", "#6A79A6", "#8A9A5B", "#508CA4",
                               "#FF6B6B", "#F4A261", "#5DADE2", "#FFC857"))
  
  
filled_plot

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = filled_plot,
       filename = "romeo_juliet.png", 
       dpi = 350, height = 3, width = 4, units = "in",
       bg = 'white')
