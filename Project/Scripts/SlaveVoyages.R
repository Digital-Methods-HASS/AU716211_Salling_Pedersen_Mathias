library(tidyverse)
library(ggplot2)

library(readr)
trans_atlantic <- read_csv("Data/trans-atlantic.csv")
View(trans_atlantic)

slave_data <- trans_atlantic %>%
  select(
    year = `Year arrived with captives`,
    origin = `Imputed region where voyage began`,
    destination = `Broad region of captive disembarkation (IMP)`,
    embarked = `Total embarked (IMP)`,
    disembarked = `Total disembarked (IMP)`
  )
slave_data <- slave_data %>%
  mutate(
    deaths = embarked - disembarked,
    mortality_rate = deaths / embarked,
    mortality_percent = mortality_rate * 100
  )
slave_data <- slave_data %>%
  filter(
    embarked > 0,
    deaths >= 0
  )

slave_data <- slave_data %>%
  mutate(decade = floor(year / 10) * 10)
summary(slave_data$mortality_percent)

origin_stats <- slave_data %>%
  count(origin, sort = TRUE)

origin_stats

destination_stats <- slave_data %>%
  count(destination, sort = TRUE)

destination_stats

origin_stats %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(origin, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Voyage Origin Regions (1750–1800)",
    subtitle = "Based on recorded embarkation regions",
    x = NULL,
    y = "Number of Voyages"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

destination_stats %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(destination, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top 10 Voyage Destination Regions (1750–1800)",
    subtitle = "Based on recorded disembarkation regions",
    x = NULL,
    y = "Number of Voyages"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

voyages_by_decade <- slave_data %>%
  count(decade)

ggplot(voyages_by_decade, aes(x = decade, y = n)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Slave Voyages by Decade (1750–1800)",
    subtitle = "Number of recorded voyages in the dataset",
    x = "Decade",
    y = "Number of Voyages"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

mortality_by_decade <- slave_data %>%
  group_by(decade) %>%
  summarise(
    avg_mortality = mean(mortality_percent, na.rm = TRUE)
  )

ggplot(mortality_by_decade, aes(x = decade, y = avg_mortality)) +
  geom_line(linewidth = 1.2, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  labs(
    title = "Average Mortality Rate by Decade (1750–1800)",
    subtitle = "Calculated from embarked and disembarked captives",
    x = "Decade",
    y = "Average Mortality (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )