## Load packages & .csv file ##
library(tidyverse)
utah_data <- read_csv("Utah_Religions_by_County.csv") #Relative to Assignment 7
glimpse(utah_data)

## Tidy the data ##
utah_long <- utah_data %>%
  pivot_longer(                  #Pivoting all religion columns into long format
    cols = -c(County, Pop_2010), #Now each row represents one county-related pair
    names_to = "Religion",
    values_to = "Proportion"
    
)
glimpse(utah_long)     #Should now have County, Pop_2010, Religion, & Proportion

## Exploring the cleaned data set ##
summary(utah_long)

utah_long %>%          # Which religions have highest/lowest prop. across counties
  group_by(Religion) %>%
  summarize(
    mean_prop = mean(Proportion),
    max_prop = max (Proportion),
    min_prop = min(Proportion)
  )

utah_long %>%          #Identifies most dominant religion in each county
  group_by(County) %>%
  slice_max(Proportion, n = 1)

utah_long %>%        #Correlation between population and proportion
  group_by(Religion) %>%
  summarize(
    cor_pop = cor(Pop_2010, Proportion, use = "complete.obs")
  )

## Creating easy labels for ggplots ##
nonreligious <- utah_long %>%           #Creating labels for first ggplot
  filter(Religion == "Non-Religious") %>%
  select(County, NonReligious = Proportion)

utah_joined <- utah_long %>%
  left_join(nonreligious, by = "County")  #Also creating lebels for first ggplot

## Figure's ##
ggplot(utah_joined %>%                   #This is used in Question 2
         filter(Religion != "Non-Religious") %>%
         filter(!is.na(NonReligious)),
       aes(x = NonReligious, y = Proportion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(title = "Religion vs Non-Religious Proportion")

ggplot(utah_long, aes(x = Proportion)) +   #Spread out each religion's proportion
  geom_histogram(bins = 20) +              #This is used in Question 1
  facet_wrap(~ Religion, scales = "free_y") +
  labs(title = "Distribution of Religious Proportions by County")

ggplot(utah_long, aes(x = Pop_2010, y = Proportion)) +  #Looking at patterns
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(title = "County Population vs Religious Proportion")

## Question 1: Does population of counties correlate with proportions 
## of any specific religious groups in that county? ##

### Reference to Question 1 Comment above in "Figure's" ###

#County populations are a hard way of measuring the amount of individuals within
#a certain denomination. Most correlations are weak or moderate, indicating that
#it's hard to tell based off of county population alone. From this figure we only 
#see Catholic, Evangelical, LDS, Non-Religious, & Religious with helpful distributions.
#Question 2 gives a better insight into interpreting the data.

## Question 2: Does proportion of any specific religion in a given county 
## correlate with the proportion of non-religious people? ##

#### Reference to Question 2 Comment above in "Figure's" ###

#The ggplot shows more negative correlations than positive trends, indicating that
#some religions are more prevalent in counties with lower non-religious proportions.
#A negative correlation suggests that counties with more non-religious individuals
#tend to have a lower proportion of religious groups. This suggests that LDS
#communities are concentrated in low non-religious areas. A positive correlation
#suggests that those denominations (Assemblies of God, Catholic, Evangelical, etc.)
#appear more prevalent in areas with more non-religious people.