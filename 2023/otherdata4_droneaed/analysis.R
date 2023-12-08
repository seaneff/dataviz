#######################################################################
### Background ########################################################
#######################################################################

# Edouard Mathieu, Fiona Spooner, Saloni Dattani, Hannah Ritchie and Max Roser (2022) - "Mpox (monkeypox)". 
# Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/monkeypox' 
# [Online Resource]

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("camcorder"), etc
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the x axis
library(colorspace) ## for helping with colors
library(directlabels) ## for labels to right of lines

#######################################################################
### Load data #########################################################
#######################################################################

data <- cbind.data.frame(group = factor(c("Drone AED\nDelivery", "Ambulance AED\nDelivery"), levels = c("Ambulance AED\nDelivery", "Drone AED\nDelivery")),
                         median_label = c("7 minutes 11 seconds", "9 minutes 43 seconds"),
                         median = c(7.1833333, 9.7166667),
                         lower = c(5.6, 7.1666667),
                         upper = c(9.9833333, 12.6666667))

#######################################################################
### Specify colors ####################################################
#######################################################################

palette <- c("#434279", "#42795e")
  
#######################################################################
### Generate figure ###################################################
#######################################################################

drone_box <- ggplot(data) +
  geom_boxplot(aes(x = group,
                   lower = lower,
                   middle = median,
                   upper = upper, 
                   color = group,
                   fill = group,
                   ymax = lower, ymin = upper),
               stat = "identity", width = 0.3,
               outlier.shape = NA) +
  geom_text(aes(label = median_label, 
                x = group, 
                y = median),
            family = "Barlow",
            color = "gray5",
            hjust = 0,
            nudge_y = 0.1,
            size = 3) +
  coord_flip() +
  scale_color_manual(values = palette) + 
  scale_fill_manual(values = lighten(palette, amount = 0.4)) + 
  ## specify titles and captions
  labs(title = "Drones may expedite AED delivery as compared to ambulances",
       y = "Total response time\nin minutes",
       x = "",
       subtitle = "Results from a prospective observational study in Sweden",
       caption = "Data from Schierbeck S et al. Drone delivery of automated external defibrillators compared with ambulance\narrival in real-life suspected out-of-hospital cardiac arrests: a prospective observational study in Sweden.\nThe Lancet Digital Health. 2023 Dec 1. Boxplots represent IQR and median of total response time,\nprinted number corresponds to published median.") + 
  theme_minimal() +
  theme(text = element_text(colour = "gray5", family = "Barlow"),
        plot.title = element_text(size = rel(1.25)),
        plot.caption = element_text(size = rel(0.75)), ## make caption slightly smaller
        legend.position = "none")  ## legend on top) 

drone_box

#######################################################################
### Save figure #######################################################
#######################################################################

ggsave(plot = drone_box,
       filename = "drone_box.png", 
       dpi = 350, height = 4, width = 6.2, units = "in",
       bg = 'white')
