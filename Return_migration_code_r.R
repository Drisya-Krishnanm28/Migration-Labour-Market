# Author: Drisya Krishnan
# Project: Reintegration of Return Migrants into Kerala's Labour Market

# 1. Load Packages

library(tidyverse)
library(ggplot2)
library(dplyr)
library(pscl)
library(stargazer)
library(corrplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 2. Import Dataset


migration_data <- read.csv("C:/Users/HP/Downloads/final_data.csv")


# 3. Data Cleaning

migration_data <- migration_data %>%
  filter(!is.na(wage)) %>%
  mutate(
    gender = as.factor(gender),
    education = as.factor(education),
    govt_support = as.factor(govt_support)
  )

# 4. Construct Skill Index


migration_data$skill_index <- rowMeans(
  migration_data[, c(
    "technical_skill",
    "communication_skill",
    "managerial_skill"
  )],
  na.rm = TRUE
)


# 5. Construct Skill Utilization Index


migration_data$skill_utilization <- rowMeans(
  migration_data[, c(
    "skill_usage",
    "job_skill_match"
  )],
  na.rm = TRUE
)


# 6. Summary Statistics

summary(migration_data)


# 7. Correlation Matrix

cor_matrix <- cor(
  migration_data[, c(
    "skill_index",
    "wage",
    "experience_abroad",
    "skill_utilization"
  )],
  use = "complete.obs"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper"
)

# 8. Skill Index Distribution

ggplot(
  migration_data,
  aes(x = skill_index)
) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "white"
  ) +
  labs(title = "Skill Index Distribution") +
  theme_minimal()

# 9. Wage Distribution

ggplot(
  migration_data,
  aes(x = wage)
) +
  geom_histogram(
    fill = "darkblue",
    color = "white"
  ) +
  labs(title = "Wage Distribution") +
  theme_minimal()
  
# 10. Scatter Plot: Skill Index and Wage


ggplot(
  migration_data,
  aes(
    x = skill_index,
    y = wage
  )
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Skill Index and Wage") +
  theme_minimal()


# 11. World Map Visualization


world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

ggplot(world) +
  geom_sf(
    fill = "lightblue",
    color = "white"
  ) +
  theme_minimal() +
  labs(title = "Migration Destinations")


# 12. Logit Model


employment_model <- glm(
  formal_employment ~ skill_index +
    age + education + govt_support +
    experience_abroad,
  data = migration_data,
  family = binomial()
)

summary(employment_model)


# 13. Entrepreneurship Model


entrepreneurship_model <- glm(
  entrepreneurship ~ skill_index +
    age + education + govt_support +
    experience_abroad,
  data = migration_data,
  family = binomial()
)

summary(entrepreneurship_model)


# 14. OLS Wage Model


wage_model <- lm(
  wage ~ skill_index +
    education +
    experience_abroad +
    govt_support,
  data = migration_data
)

summary(wage_model)


# 15. Skill Utilization Model


utilization_model <- lm(
  skill_utilization ~ skill_index +
    age +
    education +
    govt_support,
  data = migration_data
)

summary(utilization_model)


# 16. Export Regression Results


stargazer(
  employment_model,
  entrepreneurship_model,
  wage_model,
  utilization_model,
  type = "text"
)
