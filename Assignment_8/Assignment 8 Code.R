### 1. Load packages & data ###

library(tidyverse)
library(modelr)
library(broom)

mushroom <- read_csv("mushroom_growth.csv")
glimpse(mushroom)

### 2. Explore Relationships using ggplot ###

ggplot(mushroom, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(mushroom, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(mushroom, aes(x = Species, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(mushroom, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

### 3. Define 4 Models that Explain GrowthRate ###

mod1 <- lm(GrowthRate ~ Temperature, data = mushroom)
mod1

mod2 <- lm(GrowthRate ~ Humidity, data = mushroom)
mod2

mod3 <- lm(GrowthRate ~ Species, data = mushroom)
mod3

mod4 <- lm(GrowthRate ~ Light, data = mushroom)
mod4

### 4. Mean Square Error ###

mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)

### 5. Choose Best Model ###

best_mod <- mod4
summary(best_mod)

### 6. Add Predictions to Data ###

mushroom_pred <- mushroom %>%
  add_predictions(best_mod)
mushroom_pred

### 6. Create Hypothetical Data ###

hyp_data <- data.frame(
  Light = c(0, 5, 10, 15, 20)
)
hyp_data

new_pred <- hyp_data %>%
  mutate(pred = predict(mod4, newdata = hyp_data))
new_pred

new_pred$Type <- "Predicted"
mushroom_pred$Type <- "Observed"
full_data <- bind_rows(mushroom_pred, new_pred)
full_data

### 7. Plot Predictions ###

ggplot(full_data, aes(x = Light, y = pred, color = Type)) +
  geom_point(aes(y = GrowthRate)) +  # now mapped via Type
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  scale_color_manual(values = c("Observed" = "lightpink", "Predicted" = "lightblue")) +
  theme_minimal() +
  labs(y = "Growth Rate", title = "Observed vs Predicted GrowthRate by Light")