### Part I: Read the cleaned_covid_data.csv file into an R data frame ###
# Check working directory
getwd()

library(tidyverse)

# Read the data
covid_data <- read_csv("cleaned_covid_data.csv")

# Optional: Take a quick look
glimpse(covid_data)

### Part II: Subset states starting with 'A' ###
# Using grepl to select states starting with "A"
A_states <- covid_data %>%
  filter(grepl("^A", Province_State))
A_states

# Quick check
head(A_states)

### Part III: Plot Deaths over Time for A_states ###
library(ggplot2)

ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = "free") +
  theme_minimal() +
  labs(title = "COVID Death Over Time in States Starting with 'A'")

### Part IV: Find the peak of Case_Fatality_Ratio per state ###
state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

# Preview
head(state_max_fatality_rate)

### Part V: Bar Plot of Maximum_Fatality_Ratio per state ###
ggplot(state_max_fatality_rate,
       aes(x = fct_reorder(Province_State, Maximum_Fatality_Ratio),
           y = Maximum_Fatality_Ratio)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "State",
       y = "Maximum Case Fatality Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Part VI BONUS: Using FULL data set, plot Cumulative US deaths over Time ###
us_cumulative <- covid_data %>%
  group_by(Last_Update) %>%
  summarize(Cumulative_Deaths = sum(Deaths, na.rm = TRUE))

us_cumulative

ggplot(us_cumulative, aes(x = Last_Update, y = Cumulative_Deaths)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Cumulative COVID Deaths in the US Over Time",
       x = "Date",
       y = "Cumulative Deaths")
