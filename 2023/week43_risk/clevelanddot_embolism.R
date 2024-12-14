#######################################################################
### Background ########################################################
#######################################################################

## From TidyTuesday:
## This dataset contains 100 simulated patient's medical history features and the 
## predicted 1-year risk of 14 outcomes based on each patient's medical history features. 
## The predictions used real logistic regression models developed on a large real world healthcare dataset.
## Data dictionary available online here: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-24/readme.md

## Inspiration:
## https://uc-r.github.io/cleveland-dot-plots

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
## install.packages("tidytuesdayR"), etc
library(tidytuesdayR) ## to read in data for tidytuesday
library(tidyverse) ## to format/restructure/plot data
library(colorspace) ## to lighten colors
library(ggtext) ## to use markdown in ggplot
library(sysfonts) ## to manage fonts

## to review current list of options, if needed
#font_families_google()

font_add_google(name = "Barlow")
font_add_google(name = "Source Sans 3")

#######################################################################
### Load data #########################################################
#######################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 43)
risk <- tuesdata$patient_risk_profiles

#######################################################################
### Just look at people ages 20-64 ####################################
#######################################################################

risk <- risk %>%
  filter(`age group:  20 -  24` == 1 | `age group:  25 -  29` == 1 |
         `age group:  30 -  34` == 1 | `age group:  35 -  39` == 1 |
         `age group:  40 -  44` == 1 | `age group:  45 -  49` == 1 |
         `age group:  50 -  54` == 1 | `age group:  55 -  59` == 1 |
         `age group:  60 -  64` == 1)

#######################################################################
### Calculate average risk difference #################################
#######################################################################

features <- c(
  ## drug exposures
  #"Acetaminophen exposures in prior year", 
  #"Aspirin exposures in prior year",
  
  #"Antibiotics Carbapenems in prior year",
  #"Antibiotics Fluoroquinolones in prior year",
  #"Antibiotics Glycopeptides and lipoglycopeptides in prior year",
  #"Antibiotics Macrolides in prior year",
  #"Antibiotics Monobactams in prior year", exclude, see https://www.tandfonline.com/doi/full/10.1080/13543776.2021.1865919?casa_token=JzgXbcgOkt0AAAAA%3AG7Xm6TFM6JHQkTTn1UiSevk7HXhcxymgr63cyswecdfX95II8uGOZ9tfTZUUaaw8ePBDIys5XkW4Vw 
  #"Antibiotics Oxazolidinones in prior year",
  #"Antibiotics Penicillins in prior year",
  #"Antibiotics Polypeptides in prior year",
  #"Antibiotics Rifamycins in prior year",
  #"Antibiotics Sulfonamides in prior year",
  #"Antibiotics Streptogramins in prior year",
  #"Antibiotics Tetracyclines in prior year",

  ## medical events or diagnoses
  "Occurrence of Alcoholism in prior year",
  "Anemia in prior year",
  "Angina events in prior year",
  "Occurrence of Anxiety in prior year",
  "Occurrence of Asthma in prior year",
  "Atrial Fibrillation, incident in prior year",
  "Acute Kidney Injury (AKI) in prior year",
  "Heart failure in prior year",
  "Major depressive disorder, with NO occurrence of certain psychiatric disorder in prior year",
  "Type 1 diabetes and no prior specific non-T1DM diabetes in prior year",
  "Type 2 Diabetes Mellitus (DM), with no type 1 or secondary DM in prior year",
  "Deep Vein Thrombosis (DVT) in prior year",
  "Edema in prior year",
  "Gastroesophageal reflux disease in prior year",
  "Acute gastrointestinal (GI) bleeding in prior year",
  "Chronic hepatitis in prior year",
  "Hyperlipidemia in prior year",
  "Hypertension in prior year",
  "Hypothyroidism in prior year",
  "Inflammatory Bowel Disease in prior year",
  "Low back pain in prior year",
  "Occurrence of neuropathy in prior year",
  "Obesity in prior year",
  "Opioids in prior year",
  "Peripheral vascular disease in prior year",
  "Pneumonia in prior year",
  "Psychotic disorder in prior year",
  "Rheumatoid Arthritis in prior year",
  "Smoking in prior year")

outcomes <- c(
  "predicted risk of Pulmonary Embolism",
  "predicted risk of Multiple Sclerosis", 
  "predicted risk of Parkinson's disease, inpatient or with 2nd diagnosis",
  "predicted risk of Sudden Hearing Loss, No congenital anomaly or middle or inner ear conditions",
  "predicted risk of Muscle weakness or injury",
  "predicted risk of Ankylosing Spondylitis",
  "predicted risk of Autoimmune hepatitis",
  "predicted risk of Migraine",
  "predicted risk of Restless Leg Syndrome",
  "predicted risk of  Treatment resistant depression (TRD)")

average_risk_info <- data.frame(exposure = character(),
                                outcome = character(),
                                n_exposed = numeric(),
                                n_unexposed = numeric(),
                                mean_risk_exposed = numeric(),
                                mean_risk_unexposed = numeric(),
                                stringsAsFactors = FALSE)

for (f in features){
  for(o in outcomes){
    exposure_info <- risk[,f] == 1
    exposed_average_risk <- mean(unlist(risk[,o][which(exposure_info == TRUE),]))
    unexposed_average_risk <- mean(unlist(risk[,o][which(exposure_info == FALSE),]))
    average_risk_info <- rbind(average_risk_info,
                               cbind.data.frame(
                                 "exposure" = f,
                                 "outcome" = o,
                                 "n_exposed" = sum(exposure_info),
                                 "n_unexposed" = sum(exposure_info == FALSE),
                                 "mean_risk_exposed" = exposed_average_risk,
                                 "mean_risk_unexposed" = unexposed_average_risk))
  }
}

average_risk_info$average_risk_difference <- average_risk_info$mean_risk_exposed -  average_risk_info$mean_risk_unexposed

## fix labels for plot
average_risk_info$risk_label <- trimws(gsub("predicted risk of", "", average_risk_info$outcome))
average_risk_info$exposure_label <- trimws(gsub(" in prior year", "", average_risk_info$exposure))
average_risk_info$exposure_label[which(average_risk_info$exposure_label == "Major depressive disorder, with NO occurrence of certain psychiatric disorder")] <- "Major Depressive Disorder" ## we can specify this detail in the footnotes
average_risk_info$exposure_label[which(average_risk_info$exposure_label == "Type 1 diabetes and no prior specific non-T1DM diabetes")] <- "Type 1 Diabetes" ## we can specify this detail in the footnotes
average_risk_info$exposure_label[which(average_risk_info$exposure_label == "Type 2 Diabetes Mellitus (DM), with no type 1 or secondary DM")] <- "Type 2 Diabetes Mellitus" ## we can specify this detail in the footnotes
average_risk_info$exposure_label[which(average_risk_info$exposure_label == "Atrial Fibrillation, incident")] <- "Atrial Fibrillation" ## we can specify this detail in the footnotes
average_risk_info$exposure_label[which(average_risk_info$exposure_label == "Psychotic disorder")] <- "Psychotic Disorder" ## we can specify this detail in the footnotes

average_risk_info$exposure_label <- factor(average_risk_info$exposure_label,
  levels = rev(c(
  "Psychotic Disorder",
  "Rheumatoid Arthritis",
  "Anemia",
  "Obesity",
  "Atrial Fibrillation",
  "Deep Vein Thrombosis (DVT)",
  "Hypertension",
  "Edema",
  "Opioids",
  "Type 1 Diabetes",
  "Occurrence of Anxiety",
  "Occurrence of neuropathy",
  "Inflammatory Bowel Disease",
  "Acute gastrointestinal (GI) bleeding",
  "Major Depressive Disorder",
  "Acetaminophen exposures",
  "Aspirin exposures",
  "Occurrence of Alcoholism",
  "Angina events",
  "Occurrence of Asthma",
  "Low back pain",
  "Acute Kidney Injury (AKI)",
  "Heart failure",
  "Type 2 Diabetes Mellitus",
  "Gastroesophageal reflux disease",
  "Chronic hepatitis",
  "Hyperlipidemia",
  "Hypothyroidism",
  "Peripheral vascular disease",
  "Pneumonia",
  "Smoking")))
  
#######################################################################
### Generate figure ###################################################
#######################################################################

cleveland <- average_risk_info %>%
  filter(risk_label == "Pulmonary Embolism") %>%
  arrange(desc(mean_risk_exposed-mean_risk_unexposed)) %>%
  mutate(exposure_label = factor(exposure_label, levels = rev(unique(exposure_label)))) %>%
  ggplot() + 
  ## plot the segments first so that the dots cover the endpoints
  geom_segment(aes(y = exposure_label, yend = exposure_label, x = mean_risk_exposed, xend = mean_risk_unexposed), color = "grey80") +
  
  ## the plot the dots
  geom_point(aes(y = exposure_label, x = mean_risk_exposed, fill = "Patients with\nrisk factors"), size = 3, pch = 21, color = "grey5") +
  geom_point(aes(y = exposure_label, x = mean_risk_unexposed, fill = "Patients without\nrisk factors"), size = 3, pch = 21, color = "grey5") +
 
  ## specify titles and captions
  labs(title = "Patients with vascular and kidney disease are at<br>increased risk of experiencing pulmonary embolism",
       subtitle = "Average risk of pulmonary embolism in a synthetic dataset",
       fill = "", ## No legend title
       caption = "Based on synthetic data among adults ages 20-64 years of age\nrisk scores calculated based on logistic regression models developed on a large real world healthcare dataset\nVisualization by Steph Eaneff") + 
  xlab("Average risk of pulmonary embolism (next year)") +
  ylab("") +
  theme_minimal() +
  theme(text = element_text(colour = "grey5", family = "Source Sans 3"),
        plot.title = element_markdown(hjust = 0.5, ## center the title,
                                      size = rel(1.3),  ## make it bigger
                                      family = "Barlow",
                                      face = "bold"), ## make it bold
        plot.subtitle = element_markdown(hjust = 0.5, ## center the subtitle
                                         size = rel(1.1)), ## placeholder -- doesn't change size
        plot.caption = element_text(size = rel(0.6)),
        legend.position = "top") + ## legend on top) +
  scale_fill_manual(values = c("#427D9D", lighten("#427D9D", amount = 0.5))) +
  scale_x_continuous(labels = scales::percent_format(scale = 1))

ggsave(plot = cleveland,
       filename = "pe_risk.png", 
       dpi = 350, height = 6, width = 8, units = "in",
       bg = 'white')

