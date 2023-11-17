#######################################################################
### Background ########################################################
#######################################################################

# Holt M, MacGibbon J, Smith A, Broady T, Davis M, Newman C. Trust in Digital Health dataset. Zenodo; 2022. 

# "The Trust in Digital Health project was conducted by the Centre for Social Research in Health, 
# UNSW Sydney in collaboration with community organisations to assess views of digital health systems 
# in Australia, particularly among communities affected by bloodborne viruses and sexually transmissible 
# infections. We conducted a national, online survey of Australians' attitudes to digital health in 
# April–June 2020. The sample (N=2,240) was recruited from the general population and four priority 
# populations affected by HIV and other sexually transmissible infections: gay and bisexual men, 
# people living with HIV, sex workers, and trans and gender-diverse people. The deidentified dataset 
# and syntax provided here were used for an analysis of factors associated with greater knowledge of My 
# Health Record and the likelihood of opting out of the system. My Health Record is Australia's national, 
# digital, personal health record system."

## Look at responses to the question: What types of information in My Health Record are useful to you?

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("camcorder"), etc
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(haven) ## to read in Stata DTA files

#######################################################################
### Load data #########################################################
#######################################################################

data <- read_dta("TIDH2022_dataset_October22.dta")

#######################################################################
### Format data #######################################################
#######################################################################

data_long <- data %>%
  mutate(across(c(mhr_useful_1, mhr_useful_2, mhr_useful_3,
                  mhr_useful_4, mhr_useful_5, mhr_useful_6), factor)) %>%
  pivot_longer(cols = c(mhr_useful_1, mhr_useful_2, mhr_useful_3,
                        mhr_useful_4, mhr_useful_5, mhr_useful_6),
               names_to = "metric",
               values_to = "value") %>%
  select(c(age_merge, gender_r4, prioritypop_hiv, prioritypop_sexwork, prioritypop_tgd,
           prioritypop_idu, prioritypop_gbm, prioritypop_hepb,
           value, metric)) %>%
  ## clarity labels
  mutate(label = recode(metric, 
                        "mhr_useful_1" = "Test results",
                        "mhr_useful_2" = "Physician notes",
                        "mhr_useful_3" = "Prescription medication history",
                        "mhr_useful_4" = "Immunisation history",
                        "mhr_useful_5" = "Personal information",
                        "mhr_useful_6" = "Billing history"))

#######################################################################
### Start recording ###################################################
#######################################################################

# gg_record(
#   dir = file.path("figure_versions"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 300
# )

#######################################################################
### Define plot figures: colors  ######################################
#######################################################################

text_col <- "grey1"
bar_col <- "grey1"

#######################################################################
### Make figure: overall  #############################################
#######################################################################

overall_figure <- data_long %>%
  group_by(label) %>%
  filter(complete.cases(value)) %>%
  summarize( n = n(),
             pct = mean(value == 1)) %>%
  arrange(desc(pct)) %>%
  mutate(label = factor(label, levels = rev(label))) %>%
  ggplot(aes(y = label, x = pct)) +
  geom_bar(stat = "identity", color = bar_col, fill = "#5D709C") +
  ## specify titles and captions
  labs(title = "What do Australians find helpful about their digital health records?",
       subtitle = "Results from a  national survey of the Australian general population\nand communities affected by HIV and sexually transmissible infections",
       caption = "",
       x = "\nPercent of respondents",
       y = "") +
  scale_x_continuous(labels = scales::percent) +
  ## specify theme
  theme_minimal() +
  theme(text = element_text(family = "Barlow", colour = text_col), ## fonts should be Barlow
        plot.title = element_text(face = "bold", size = rel(1.1)), ## make title bold and somewhat bigger
        axis.text = element_text(size = rel(1)), 
        axis.title = element_text(size = rel(1.2)), ## make axis title larger
        plot.subtitle = element_text(size = rel(1.1))) ## make subtitle larger

#######################################################################
### Save Images #######################################################
#######################################################################

ggsave(plot = overall_figure,
       filename = "digital_health_interest_overall.png", 
       dpi = 350, height = 5, width = 8, units = "in",
       bg = 'white')

#######################################################################
### Save GIF ##########################################################
#######################################################################

# gg_playback(
#   name = file.path("figure_versions/figure_versions.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = background_col
# )
