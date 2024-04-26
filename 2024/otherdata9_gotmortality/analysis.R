#######################################################################
### Background ########################################################
#######################################################################

# Original publication
## Lystad RP, Brown BT. “Death is certain, the time is not”: 
## mortality and survival in Game of Thrones. Injury Epidemiology 2018;5:44.
## Data Accessed via figshare: https://figshare.com/articles/dataset/Game_of_Thrones_mortality_and_survival_dataset/8259680

## Note that results are different than published results
## The version of the dataset used in the original research article included data from Game of Thrones Seasons 1–7 only, 
## whereas the present version of the dataset includes data from Game of Thrones Seasons 1–8.

## Additional reading:
## https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

## Code for ggforest (used to make my own forest plot)
## https://github.com/kassambara/survminer/blob/master/R/ggforest.R

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have these downloaded, start by running:
## install.packages("tidyverse"), etc
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the y axis
library(survival) ## for survival analysis
library(ggsurvfit) ## to make Kaplan-Meier plots
library(gtsummary) ## for nicely formatted summary tables
library(survminer) ## for nicely formatted forest plots for Cox proportional hazards models
library(ggtext) ## to use markdown in ggplot for labels

#######################################################################
### Read in data ######################################################
#######################################################################

## based on data from table 3 in the publication
mortality <- read.csv(file = "GoT_dataset/character_data_S01-S08.csv", header = TRUE)

#######################################################################
### Fit survival curve for full cohort ################################
#######################################################################

## this function creates survival curves using the Kaplan-Meier method
## generate the overall survival curve for the entire cohort
cohort_survival_fit <- survfit(Surv(time = censor_time_hrs, event = dth_flag) ~ 1, data = mortality)

survfit2(Surv(censor_time_hrs, dth_flag) ~ 1, data = mortality) %>%
  ggsurvfit(type = "survival", color = "#224E73") +
  #scale_ggsurvfit() +
  add_confidence_interval(fill = "#224E73") +
  scale_ggsurvfit() +
  labs(title = "Survival in Game of Thrones\n(Seasons 1-8) ",
       caption = "",
       y = "Survival Probability",
       x = "Time\n(in episode hours)") +
  ## update plot themes
  theme(text = element_text(family = "Barlow", colour = "gray5"), ## fonts should be Barlow
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5), ## make title bold and somewhat bigger
        axis.text = element_text(size = rel(1)), 
        strip.text = element_text(size = rel(1.1), face = "bold"),
        axis.title = element_text(size = rel(1.2)), ## make axis title larger
        plot.subtitle = element_text(size = rel(1.1), hjust = 0.5))  ## make subtitle larger
  ## unused additional options
  #add_censor_mark() +
  #add_quantile() +
  #add_risktable() 

#######################################################################
### Fit survival curve by sex #########################################
#######################################################################

figure <- survfit2(Surv(censor_time_hrs, dth_flag) ~ ifelse(sex == 1, "male", "female"), data = mortality) %>%
  ggsurvfit(type = "survival") +
  #scale_ggsurvfit() +
  add_confidence_interval() +
  scale_ggsurvfit() +
  theme_minimal() + 
  labs(title = "Survival in Game of Thrones",
       subtitle = "Characters from Seasons 1-8",
       caption = "",
       y = "Survival Probability",
       x = "**Time**<br>(in episode hours)") +
  ## update plot themes
  theme(text = element_text(family = "Barlow", colour = "gray5"), ## fonts should be Barlow
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5), ## make title bold, bigger, and centered
        plot.subtitle = element_text(size = rel(1.3), hjust = 0.5),  ## make subtitle bigger and centered
        axis.text = element_markdown(size = rel(1.1)), 
        axis.title.x = element_markdown(size = rel(1.3)),
        axis.title.y = element_markdown(size = rel(1.3)),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
        ## position legend within plot -- todo
        #legend.position = c(0.2,0.2), legend.background = element_rect(fill = 'white', color = "grey80"),
        #legend.title = element_text(size = rel(1.1)), 
        #legend.text = element_text(size = rel(1)), legend.key.size = unit(1.5, 'cm'))

figure
## unused additional options
#add_censor_mark() +
#add_quantile() +
#add_risktable() 

#######################################################################
### Fit more complete Cox proportional hazards regression model #######
#######################################################################

mortality$occupation <- factor(mortality$occupation)
## fit Cox proportional hazards regression model
cox_model <- coxph(Surv(censor_time_hrs, dth_flag) ~ sex + occupation, data = mortality)

## generate formatted summary table
cox_model %>%
  tbl_regression(exp = TRUE) 

## extract data from model
cox_model_results <- as.data.frame(tidy(cox_model, conf.int = TRUE))

ggforest(model = cox_model) +
  ## update plot themes
  theme(text = element_text(family = "Barlow", colour = "gray5"), ## fonts should be Barlow
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5), ## make title bold and somewhat bigger
        axis.text = element_text(size = rel(1)), 
        strip.text = element_text(size = rel(1.1), face = "bold"),
        axis.title = element_text(size = rel(1.2)), ## make axis title larger
        plot.subtitle = element_text(size = rel(1.1), hjust = 0.5))  ## make subtitle larger

#######################################################################
### Save figure #######################################################
#######################################################################

ggsave(plot = figure,
       filename = "survival_curve.png", 
       dpi = 350, height = 4, width = 5, units = "in",
       bg = 'white')
