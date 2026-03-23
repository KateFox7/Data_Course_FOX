### 1. Read in the Data ###

library(tidyverse)                        #Load tidyverse
unicef <- read_csv("unicef-u5mr.csv")     #Rename as variable
glimpse(unicef)                           #Glimpse the data

### 2. Make it Tidy ###

unicef_tidy <- unicef %>%                 #Tidy/long format
  pivot_longer(
    cols = starts_with("U5MR"),
    names_to = "Year",                    #One column for Year
    values_to = "U5MR"                    #One column for U5MR
  )

unicef_tidy <- unicef_tidy %>%            #Clean the year column
  mutate(Year = as.numeric(str_remove(Year, "U5MR.")))

glimpse(unicef_tidy)                      #Glimpse the data

### 3. Plot Each Country's U5MR Over Time ###

plot1 <- ggplot(unicef_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.3) +
  facet_wrap (~ Continent) +
  labs(
    title = "U5MR Over Time by Country",
    y= "Under-5 Mortality Rate",
    x = "Year"
  ) +
  theme_minimal()

plot1                                     #Plot it

### 4. Save plot1 ###

ggsave("Fox_Plot_1.png", plot1, width = 10, height = 6)

### 5. Mean U5MR per Continent per Year ###

continent_avg <- unicef_tidy %>%          #Summarize
  group_by(Continent, Year) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

plot2 <- ggplot(continent_avg, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line() +
  labs(
    title = "Mean U5MR by Continent Over Time",
    y = "Mean U5MR",
    x = "Year"
  ) +
  theme_minimal()

plot2                                     #Plot it

### 6. Save plot2 ###

ggsave("Fox_Plot_2.png", plot2, width = 10, height = 6)

### 7. Build 3 Models of U5MR ###
                                          #Linear models
mod1 <- lm(U5MR ~ Year, data = unicef_tidy)
mod1

mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)
mod2

mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)
mod3

### 8. Compare the 3 Models ###

summary(mod1)                             #Use "summary" to compare models
summary(mod2)
summary(mod3)
                                          #OR...AIC, test of goodness of fit
AIC(mod1, mod2, mod3)                     #mod3 has the lowest AIC, meaning it fits
                                          #the data the best while accounting for
                                          #complexity. So it's the preferred model.
### 9. Plot Model Predictions ###

unicef_tidy <- unicef_tidy %>%            #Add predictions to data set
  mutate(
    pred1_mod1 = predict(mod1, newdata = unicef_tidy),
    pred2_mod2 = predict(mod2, newdata = unicef_tidy),
    pred3_mod3 = predict(mod3, newdata = unicef_tidy)
  )

preds_long <- unicef_tidy %>%             #Reshape for plotting
  select(Continent, Year, CountryName, pred1_mod1, pred2_mod2, pred3_mod3) %>%
  pivot_longer(
    cols = c(pred1_mod1, pred2_mod2, pred3_mod3),
    names_to = "Model",
    values_to = "pred",
    names_prefix = "pred_"
  )
                                          #Plot it
plot3 <- ggplot(preds_long, aes(x = Year, y = pred, color = Continent, group = interaction(Continent, CountryName))) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Model) +
  labs(
    title = "Model Predictions",
    y = "Predicted U5MR",
    x = "Year"
  ) +
  theme_minimal()

plot3

### 10. BONUS: Predict Ecuador 2020 ###

ecuador_2020 <- data.frame(               #Create new data
  Year = 2020,
  Continent = "Americas",
  CountryName = "Ecuador"
)
                                          #Predict using best mod (mod3)
pred_value <- predict(mod3, newdata = ecuador_2020)
pred_value

result <- data.frame(                     #Display as tidy table matching expected output
  Continent = "Americas",
  Year = 2020,
  CountryName = "Ecuador",
  pred = pred_value
)

print(result)

difference <- abs(pred_value - 13)
difference

### BONUS: Log Transformation Model to Improve Prediction ###

mod4 <- lm(log(U5MR) ~ Year * Continent, data = unicef_tidy %>% filter(U5MR > 0))

pred_log <- predict(mod4, newdata = ecuador_2020)
pred_backtransformed <- exp(pred_log)     #Back transform from log scale

bonus_result <- data.frame(
  Model = "mod4",
  Prediction = round(pred_backtransformed, 5),
  Reality = 13
)
print(bonus_result)                       #My prediction was 11.99908 and the actual
                                          #value was 13. I wasn't that far off!
