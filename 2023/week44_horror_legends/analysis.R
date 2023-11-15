#######################################################################
### Background ########################################################
#######################################################################

## Horror legends from https://www.snopes.com/
## From TidyTuesday:
## Since urban legends are often a means of expressing our fears about the dangers that 
## ripple just beneath the surface of our seemingly calm and untroubled world, 
## it should come as no surprise that horror legends are one of urban folklore's richest veins. 
## We worry about the terrible accidents we're powerless to prevent, and we fear 
## anonymous killers who choose victims at random. We cannot protect ourselves from the 
## venomous animals who slither undetected into the places where we work, play, and shop, 
## nor can we stop the onslaught of insects who invade our homes and our bodies. 
## We're repulsed by the contaminants that may lurk in our food. 
## We're afraid of foreigners and foreign places. We fear for our childrens' safety 
## in a world full of drugs, kidnappers, and poisons. We never know what gruesome 
## discovery may be waiting around the next corner. And even if we somehow escape all 
## of these horrors, our own vanities may do us in.

## Idea: What are people most afraid of in Snopes legends?
  
## Inspiration sources: 
## https://github.com/BlakeRMills/TidyTuesday/blob/main/2021/Emmys%20(21%20Sept%202021)/Emmys%20(21%20Sept%202021).R
## https://joseph-pope.netlify.app/blog/endangered-plants/

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(showtext) ## for custom fonts
library(ggtext) ## for combining bold and not bold fonts on axis label
library(readxl) ## to read in excel files
library(colorspace) ## to lighten colors

## to review current list of options, if needed
#font_families_google()

font_add_google("Barlow")

#######################################################################
### Load data #########################################################
#######################################################################

## read in the data I coded, assume working directory is source file location
#tuesdata <- tidytuesdayR::tt_load(2023, week = 44)
#horror <- tuesdata$horror_articles
horror <- read_excel("horror.xlsx")

horror$year <-  format(as.Date(horror$published, format="%d/%m/%Y"),"%Y")

#######################################################################
### Generate color palette ############################################
#######################################################################

category_palette <- c("#af5909", "#be780c", "#608357", "#055D66", "#052c66", "#66055D", "#660513")
#lighten_amount <- seq(0, 1, length = 7)[1:6] ## instead make a custom range
lighten_amount <- c(0, 0.4, 0.8, 0.8, 0.8)

#######################################################################
### Generate count data ###############################################
#######################################################################

counts <- pivot_longer(horror, 
                       cols = starts_with("involves_"),
                       names_to = "name",
                       values_to = "value") %>%
  select(name, value) %>%
  group_by(name) %>%
  summarize(numerator = sum(value),
            denominator = n(),
            pct = mean(value)) %>%
  arrange(desc(pct)) %>%
  mutate(label = gsub("_", " ", sub("involves_", "", name))) %>%
  filter(label != "children") %>%
  filter(label != "police") %>%
  ## make everything plural, fix typo in original data
  mutate(label = recode(label, "abduction" = "abductions",
                        "disappearance" = "disappearances",
                        "intruder" = "intruders",
                        "razor bladers" = "razor blades")) %>%
  filter(numerator > 1) %>%
  mutate(label_factor = factor(label, levels = rev(unique(label)))) %>%
  mutate(category = factor(recode(label, 
                           'death' = "disease,\ndisfigurement,\nand death",
                           'food' = "food\nand drugs",
                           'technology' = "technology",
                           'disease' = "disease,\ndisfigurement,\nand death",
                           'insects' = 'natural\nworld',
                           'abductions' = "other scary\nthings",
                           'disappearances' = "other scary\nthings",
                           'substance use' = "food\nand drugs",
                           'body parts' = 'disease,\ndisfigurement,\nand death',
                           'ghosts or apparations' = 'supernatural',
                           'weapons' = 'violence\nand weapons',
                           'electrocution' = 'technology',
                           'wild animals' = 'natural\nworld',
                           'cannibalism'  = "food\nand drugs",
                           'fire' = 'natural\nworld',
                           'creatures' = 'supernatural',
                           'intruders' = 'other scary\nthings',
                           'razor blades' = 'violence\nand weapons',
                           'terrorism' = 'violence\nand weapons',
                           'projectiles' = 'violence\nand weapons',
                           'religion' = 'other scary\nthings',
                           'social media' = "technology",
                           'natural disasters' = 'natural\nworld',
                           'amputation' = "disease,\ndisfigurement,\nand death",
                           "clowns" = "other scary\nthings",
                           "graveyard" = "disease,\ndisfigurement,\nand death",
                           "rats" = 'natural\nworld',
                           "weather" = 'natural\nworld'))) %>%
  group_by(category) %>%
  mutate(category_id = cur_group_id()) %>%
  mutate(category_ranking = row_number(category)) %>%
  mutate(category_color = category_palette[category_id]) %>%
  ungroup() %>%
  mutate(bar_color = lighten(category_color, lighten_amount[category_ranking]))

## archive count data before we do weird stuff to it for the graphic to work
data_counts <- counts

#######################################################################
### Generate data needed for graphic ##################################
#######################################################################

## add padding by creating artificial blank data
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
padding_data <- data.frame( matrix(NA, empty_bar*nlevels(counts$category), ncol(counts)) )
colnames(padding_data) = colnames(counts)
padding_data$category = rep(levels(counts$category), each = empty_bar)
counts <- rbind(counts, padding_data)

## sort and give numbers 
counts <- counts %>% arrange(category)
counts$id = seq(1, nrow(counts))

# get the name and the y position of each label
label_data <- counts
angle <- 90 - 360 * (label_data$id-0.5) /nrow(label_data)     # substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1.05, -0.05)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# data for base lines
base_data <- counts %>% 
  group_by(category) %>% 
  summarize(start = min(id), end = max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end)))

# data for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

#######################################################################
### Generate graphic ##################################################
#######################################################################

circle_bar <- counts %>%
  ggplot() +
  
  # add ticks for 10 - 50%,
  geom_segment(data = grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "gray90", linewidth = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 30, xend = start, yend = 30), colour = "gray90", linewidth = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "gray90", linewidth = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "gray90", linewidth = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0),   colour = "gray90", linewidth = 0.3 , inherit.aes = FALSE ) +
  
  # add text showing the value of each line plotted above
  ggplot2::annotate("text", x = rep(max(counts$id), 5), y = seq(from = 0, to = 40, by = 10), 
                    label = paste(seq(from = 0, to = 40, by = 10), "%", sep = ""),
                    color = "gray50", size = 2 , angle = 0, hjust = 1.2) +
  ## plot bars
  geom_bar(aes(x = id, y = pct*100, fill = bar_color), stat = "identity", alpha = 1) +
  
  ## specify dimensions and make it a circle 
  ylim(-60 , 40) +
  coord_polar() + 
  
  ## specify titles and captions
  labs(title = "We are terrified of death, disease, creepy food, and insects",
       subtitle = "Frequently mentioned creepy topics in urban legends (1997-2023)",
       caption = "Based on analysis of data from Snopes horror legends (1997-2023)\npercents calculated as percent of 253 horror urban legends that mentioned a given topic based on inductive coding") + 
  ## specify theme
  theme_minimal() +
   theme(legend.position = "none", ## no legend
         text = element_text(family = "Barlow"), ## fonts should be Barlow
         axis.text = element_blank(), ## avoid weird polar axis
         axis.title = element_blank(), ## avoid x and y axis labels
         panel.grid = element_blank(), ## no grid
         plot.title = element_text(hjust = 0.5, face = "bold"), ## center title, make it bold
         plot.subtitle = element_text(hjust = 0.5), ## center subtitle
         plot.caption = element_text(size = rel(0.5))) + ## make caption comparably smaller 
  
  ## add labels on top of the bars
  geom_text(data = label_data, aes(x = id, y = pct*100, label = label_factor, hjust = hjust, color = category), 
            size = 2, 
            angle = label_data$angle, 
            inherit.aes = FALSE ) +
  
  # Add base line information
   geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5, color = category), 
                alpha = 0.8, 
                size = 0.6, inherit.aes = FALSE)  +
   geom_text(data = base_data, aes(x = title, y = -18, label = category, color = category),
             alpha = 0.8, size = 2, 
             inherit.aes = FALSE) +
  
  ## allow colors to be input as part of the data
  scale_fill_identity() +
  ## text colors can align to category palette 
  scale_color_manual(values = category_palette)

ggsave(plot = circle_bar,
       filename = "scary_snopes.png", 
       dpi = 350, height = 6, width = 8, units = "in",
       bg = 'white')
