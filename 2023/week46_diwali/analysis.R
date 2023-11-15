#######################################################################
### Background ########################################################
#######################################################################

## Definitions of key data points: =
## Summary: https://www.kaggle.com/datasets/saadharoon27/diwali-sales-dataset
## "The data this week comes from sales data for a retail store during the Diwali festival period in India."

## Idea: What percent of the population votes democratic
  
## Inspiration sources: 
## 
#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis

## to review current list of options, if needed
#font_families_google()

font_add_google("Tajawal")

#######################################################################
### Load data #########################################################
#######################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 46)
sales <- tuesdata$diwali_sales_data

#######################################################################
### Format data #######################################################
#######################################################################

names(sales)[which(names(sales) == "Age Group")] <- "Age_Group"

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

background_col <- "#fafafa"
text_col <- "grey10"
bar_col <- "grey10"

color_palette <- rev(c("#4B1D91", "#8E0F9C", "#BF219A", "#E54787", "#F27B68", "#F1AA60", "#E7D39A"))

#######################################################################
### Make figure  ######################################################
#######################################################################

totals <- sales %>%
  group_by(Product_Category) %>%
  summarize(total_spending = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(total_spending))

totals_by_age <- sales %>%
  group_by(Age_Group, Product_Category) %>%
  summarize(total_spending = sum(Amount, na.rm = TRUE)) %>%
  mutate(category_factor = factor(Product_Category, levels = rev(totals$Product_Category))) %>%
  mutate(age_factor = factor(Age_Group, levels = c("55+", "51-55", "46-50", "36-45", "26-35", "18-25", "0-17")))

stacked_bar <- totals_by_age %>%
  ggplot(aes(y = category_factor, x = total_spending, fill = age_factor)) +
  geom_bar(stat = "identity", color = bar_col, size = 0.1) +
  labs(title = "Who spends money on what, during Diwali?",
       x = "Total spending (Indian rupees)",
       y = "",
       fill = "Age group",
       caption = "Retail spending data from a single retail store in India") +
  theme_minimal() +
  theme(text = element_text(family = 'Tajawal', color = text_col),
        axis.title.x = element_text(size = rel(1.2), margin = margin(t = 10)),
        axis.text = element_text(size = rel(1.2)),
        legend.position = c(.85, .3), legend.background = element_rect(fill = 'white', color = "grey80"),
        legend.title = element_text(size = rel(1.1)), 
        legend.text = element_text(size = rel(1)), legend.key.size = unit(.4, 'cm'),
        strip.text.y = element_blank(),
        plot.title = element_text(size = rel(1.5)),
        plot.caption = element_text(margin = margin(t = 25), hjust = 0)) +
  scale_x_continuous(label = scales::comma) +
  scale_fill_manual(values = color_palette) +
  guides(fill = guide_legend(reverse = TRUE))

#######################################################################
### Save Image ##########################################################
#######################################################################

ggsave(plot = stacked_bar,
       filename = "diwali_stacked_bar.png", 
       dpi = 350, height = 5, width = 8, units = "in",
       bg = 'white')

#######################################################################
### Save GIF ##########################################################
#######################################################################

gg_playback(
  name = file.path("figure_versions/figure_versions.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = background_col
)
