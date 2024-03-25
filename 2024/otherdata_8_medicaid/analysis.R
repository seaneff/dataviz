
### Background ########################################################
#######################################################################
# Original publication
# Levy BE, Mangino AA, Castle JT, Stephens WA, McDonald HG, Patel JA, Beck SJ, 
# Bhakta AS. Effect of Medicaid expansion on inflammatory bowel disease and 
# healthcare utilization. The American Journal of Surgery. 2024 Jan 18.

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have these downloaded, start by running:
## install.packages("tidyverse"), etc
library(tidyverse) ## to format/restructure/plot data
library(camcorder) ## for making gif of figure development
library(scales) ## for commas on the y axis
library(patchwork) ## to combine plots

#######################################################################
### Create data #######################################################
#######################################################################

## based on data from table 3 in the publication
admission_type <- data.frame(
  type = factor(c("elective", "urgent", "other", "elective", "urgent", "other"), levels = c("elective", "urgent", "other")),
  time = factor(c("pre-expansion\n(2009-2013)", "pre-expansion\n(2009-2013)", "pre-expansion\n(2009-2013)", "post-expansion\n(2014-2020)", "post-expansion\n(2014-2020)", "post-expansion\n(2014-2020)"), levels = c("pre-expansion\n(2009-2013)", "post-expansion\n(2014-2020)")),
  count = c(1604, 1480, 302, 18427, 3874, 1954),
  pct = c(.474, .437, .09, .76, .16, .08)
)

## based on data from table 3 in the publication
## error in last percent in the original table
admission_source <- data.frame(
  type = factor(c("clinic", "emergency room", "other", "clinic", "emergency room", "other"), levels = c("clinic", "emergency room", "other")),
  time = factor(c("pre-expansion\n(2009-2013)", "pre-expansion\n(2009-2013)", "pre-expansion\n(2009-2013)", "post-expansion\n(2014-2020)", "post-expansion\n(2014-2020)", "post-expansion\n(2014-2020)"), levels = c("pre-expansion\n(2009-2013)", "post-expansion\n(2014-2020)")),
  count = c(2540, 442, 404, 21935, 106, 2214),
  pct = c(.75, .131, .12, .904, .004, .091)
)

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
### Generate figure: admission type ###################################
#######################################################################

admission_type_fig <- ggplot(admission_type, aes(x = time, y = pct, fill = type, label = type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(size = 2.5, position = position_stack(vjust = 0.5),
            family = "Barlow", 
            color = c("gray5", "white", "white", "gray5", "white", "white")) +
  labs(title = "Medicaid expansion\nand inflammatory bowel disease (IBD)",
       subtitle = "Admission types for IBD encounters\npre- and post- Medicaid expansion in Kentucky",
       x = "",
       y = "% encounters") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#be96a1", "#72466e", "#31242f")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = rel(0.8), face = "bold"),
        plot.sub = element_text(hjust = 0.5, size = rel(0.8)),
        text = element_text(colour = "grey5", family = "Barlow"),
        axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)),
        legend.position = "none")

admission_type_fig

#######################################################################
### Save figure #######################################################
#######################################################################

ggsave(plot = admission_type_fig,
       filename = "admission_type.png", 
       dpi = 350, height = 3, width = 3.1, units = "in",
       bg = 'white')

#######################################################################
### Stop recording and save GIF #######################################
#######################################################################

gg_stop_recording()

gg_playback(
  name = file.path("figure_versions/figure_versions.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
