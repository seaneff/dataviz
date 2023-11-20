#######################################################################
### Background ########################################################
#######################################################################

# Kumar A, Ross JS, Patel NA, Rathi V, Redberg RF, Dhruva SS. 
## Studies Of Prescription Digital Therapeutics Often Lack Rigor And Inclusivity: 
## Study examines prescription digital therapeutics standards. Health Affairs. 2023 Nov 1;42(11):1559-67.

## "Little is known about the evidence to support prescription digital therapeutics, which are 
## digital tools that rely primarily on software for diagnosis or treatment that have indications 
## for use regulated by the Food and Drug Administration (FDA) and require a clinician’s prescription. 
## We conducted the first retrospective cross-sectional analysis of clinical studies of twenty
## prescription digital therapeutics authorized by the FDA and available on the market as of November 
## 2022. Our analysis found that just two prescription digital therapeutics had been evaluated in at 
## least one study that was randomized and blinded and that used other rigorous standards of evidence. 
## Two-thirds of clinical studies of prescription digital therapeutics were conducted on a 
## postmarket basis, with less rigorous standards of evidence than the standards used in premarket 
## studies. More than half of studies did not report data on participants’ race, and more than 80
## percent did not report their ethnicity. More than one-third required English proficiency, and 
## nearly half of nonpediatric studies had an upper age limit. These results suggest the need 
## for a more rigorous and inclusive approach to clinical research supporting FDA-authorized 
## prescription digital therapeutics. A stronger evidence base would increase confidence in these 
## technologies’ effectiveness and would enable more informed decision making about their clinical use 
## and coverage."

## Look at responses to the question: What types of information in My Health Record are useful to you?

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("camcorder"), etc
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(packcircles) ## to make packed circle plot

#######################################################################
### Load data #########################################################
#######################################################################

study_types <- rbind.data.frame(
cbind.data.frame(timing = "Premarket studies (n=39)", type = "Randomized controlled trial", pct = 0.41),
cbind.data.frame(timing = "Premarket studies (n=39)", type = "Single group assignment", pct = 0.385),
cbind.data.frame(timing = "Premarket studies (n=39)", type = "Crossover assignment", pct = 0.026),
cbind.data.frame(timing = "Premarket studies (n=39)", type = "Parallel assignment", pct = 0.051),
cbind.data.frame(timing = "Premarket studies (n=39)", type = "Observational cohort", pct = 0.128),

cbind.data.frame(timing = "Postmarket studies (n=78)", type = "Randomized controlled trial", pct = 0.372),
cbind.data.frame(timing = "Postmarket studies (n=78)", type = "Single group assignment", pct = 0.321),
cbind.data.frame(timing = "Postmarket studies (n=78)", type = "Crossover assignment", pct = 0.038),
cbind.data.frame(timing = "Postmarket studies (n=78)", type = "Parallel assignment", pct = 0.038),
cbind.data.frame(timing = "Postmarket studies (n=78)", type = "Observational cohort", pct = 0.231)
)

study_types$timing <- factor(study_types$timing, levels = c("Premarket studies (n=39)",
                                                            "Postmarket studies (n=78)"))

study_types$type <- factor(study_types$type, levels = rev(c("Randomized controlled trial",
                                                        "Single group assignment",
                                                        "Parallel assignment",
                                                        "Crossover assignment",
                                                        "Observational cohort")))

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
### Make figure #######################################################
#######################################################################

annotation_text_pre <- data.frame(x = 0.73, y = 1.75, lab = "Interventional studies", type = "Premarket studies (n=39)")
annotation_text_post <- data.frame(x = 0.73, y = 1.75, lab = "Interventional studies", type = "Postmarket studies (n=78)")
          
bar <- study_types %>%
  ggplot(aes(y = type, x = pct, group = timing)) +
  geom_bar(stat = "identity", alpha = 0) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 1.5, ymax = 5.5, fill = "#b6c9db", alpha = 0.5) +
  geom_segment(aes(x = 0, y = 1.5, xend = 1, yend = 1.5), colour = "#343a3d", size = 0.3, alpha = 0.3, linetype = "dashed") + 
  geom_segment(aes(x = 0, y = 5.5, xend = 1, yend = 5.5), colour = "#343a3d", size = 0.3, alpha = 0.3, linetype = "dashed") + 
  geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 5.5), colour = "#343a3d", size = 0.3, alpha = 0.3, linetype = "dashed") + 
  geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 5.5), colour = "#343a3d", size = 0.3, alpha = 0.3, linetype = "dashed") + 
  geom_bar(stat = "identity", fill = "#083692", color = "#121415", size = 0.3) +
  theme_minimal() +
  labs(title = "Fewer than 50% of studies for FDA-authorized health apps were\ndesigned as randomized control trials",
       subtitle = "Characteristics of clinical studies supporting FDA-authorized\n prescription digital therapeutics (PDTs)",
       caption = "",
       x = "\nPercent of respondents",
       y = "") +
  scale_x_continuous(labels = scales::percent) +
  theme(text = element_text(family = "Barlow", colour = text_col), ## fonts should be Barlow
        plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5), ## make title bold and somewhat bigger
        axis.text = element_text(size = rel(1)), 
        strip.text = element_text(size = rel(1.1), face = "bold"),
        axis.title = element_text(size = rel(1.2)), ## make axis title larger
        plot.subtitle = element_text(size = rel(1.1), hjust = 0.5)) + ## make subtitle larger
  geom_text(data = annotation_text_pre, mapping = aes(x = x, y = y, label = lab, group = type),  color = "#121415", family = "Barlow", size = 4) +
  facet_wrap(~timing) 

bar

#######################################################################
### Save Images #######################################################
#######################################################################

ggsave(plot = bar,
       filename = "RCT_stats.png", 
       dpi = 350, height = 5, width = 9, units = "in",
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
