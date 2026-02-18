## Loading the data ##
library(tidyverse)
library(gganimate)
library(readr)
library(ggplot2)
library(dplyr)

dat <- read_csv("Data/BioLog_Plate_Data.csv")
glimpse(dat)
head(dat)

## Tidy the data ##
tidy_dat <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    names_prefix = "Hr_",
    values_to = "Absorbance"
   ) %>%
  mutate(
    Time = as.numeric(Time),
    SourceType = if_else(str_detect(`Sample ID`, "Soil"), "Soil", "Water")
  )

tidy_dat
colnames(dat)

## Filter for Dilution 0.1 & Make Static Plot ##
plot_dat <- tidy_dat %>%
  filter(Dilution == 0.1)
plot_dat

plot_dil_0.1 <- ggplot(tidy_dat %>% filter(Dilution == 0.1),
    aes(x = Time, y = Absorbance, color = SourceType)) +
  geom_line(aes(group = `Sample ID`), alpha = 0.5, linewidth = 0.8) +
  geom_smooth(aes(group = SourceType), method = "loess", se = FALSE, linewidth = 1.2) +
  facet_wrap(~Substrate) +
  theme_minimal() +
  labs(
    title = "Absorbance Over Time (Dilution 0.1)",
    x = "Time (hrs)",
    y = "Absorbance"
  )

plot_dil_0.1

ggsave("plot_dil_0.1.png", plot = plot_dil_0.1, width = 12, height = 8)

## Prepare data for animation ##
itaconic_data <- tidy_dat %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(SourceType, Time) %>%
  summarize(MeanAbs = mean(Absorbance, na.rm = TRUE), .groups = "drop")

itaconic_data

## Create Animated Plot for "Itaconic Acid" ##
itaconic_anim <- ggplot(itaconic_data, aes(x = Time, y = MeanAbs, color = SourceType)) +
  geom_line(aes(group = SourceType), linewidth = 1.5) +
  geom_point(size = 3) +
  labs(
    title = "Mean Absorbance for Itaconic Acid",
    x = "Time (hrs)",
    y = "Mean Absorbance",
    color = "Sample Type"
  ) +
  theme_minimal() +
  transition_reveal(Time)

animate(itaconic_anim, nframes = 100, fps = 10)

anim_save("Assignment_6_itaconic.gif", animation = itaconic_anim)