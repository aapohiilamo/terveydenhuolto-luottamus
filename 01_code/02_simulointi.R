# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggrepel)

custom_colors <- c(
  rgb(196, 0, 60, maxColorValue = 255),    # Dark Red
  rgb(226, 127, 159, maxColorValue = 255), # Pink
  rgb(0, 123, 255, maxColorValue = 255),   # Vibrant Blue
  rgb(0, 150, 0, maxColorValue = 255),     # Strong Green
  rgb(255, 165, 0, maxColorValue = 255),   # Orange
  rgb(75, 0, 130, maxColorValue = 255),    # Indigo
  rgb(255, 20, 147, maxColorValue = 255),  # Deep Pink
  rgb(139, 69, 19, maxColorValue = 255),   # Saddle Brown
  rgb(184, 134, 11, maxColorValue = 255),  # Dark Goldenrod (replacing gold)
  rgb(70, 130, 180, maxColorValue = 255),  # Steel Blue
  rgb(255, 99, 71, maxColorValue = 255),   # Tomato (a brighter red-orange)
  rgb(255, 140, 0, maxColorValue = 255),   # Dark Orange
  rgb(0, 206, 209, maxColorValue = 255),   # Dark Turquoise
  rgb(218, 112, 214, maxColorValue = 255), # Orchid (a vibrant pinkish-purple)
  rgb(154, 205, 50, maxColorValue = 255),  # Yellow Green (a bright, light green)
  rgb(240, 128, 128, maxColorValue = 255), # Light Coral (a lighter red)
  rgb(147, 112, 219, maxColorValue = 255), # Medium Purple (a light purple)
  rgb(255, 105, 180, maxColorValue = 255), # Hot Pink (a bright pink)
  rgb(186, 85, 211, maxColorValue = 255),  # Medium Orchid (a vibrant purple)
  rgb(0, 191, 255, maxColorValue = 255)    # Deep Sky Blue (a bright, light blue)
)

cleaned_data <- read_csv("00_data/02_processed/yhdistetty_kansalaispulssit.csv") %>%
  select(-luottamus_eu, -luottamus_puolustusvoimat) %>%
  rename(healthcare_trust = luottamus_terveydenhuolto) %>%
  mutate(
    mean_trust_indicators = rowMeans(select(., starts_with("luottamus_")), na.rm = TRUE),
    healthcare_trust = case_when(
      healthcare_trust >= 0 & healthcare_trust <= 4 ~ 0,
      healthcare_trust >= 5 & healthcare_trust <= 10 ~ 1,
      TRUE ~ healthcare_trust  # Retain original value if outside 0-10 range
    ),
    time = droplevels(as.factor(time))
  ) %>%
  filter(!is.na(mean_trust_indicators) & !is.na(q6) & !is.na(healthcare_trust) & !is.na(paino))

cleaned_data$time <- droplevels(cleaned_data$time)

# regressiomallit
model_basic <- lm(healthcare_trust ~ factor(time)*mean_trust_indicators,
                  data = cleaned_data,
                  weights = paino)

model_extended <- lm(healthcare_trust ~ factor(time)*(mean_trust_indicators + factor(q6)),
                     data = cleaned_data,
                     weights = paino)

simulated_jan2021 <- cleaned_data %>%
  filter(time == "2021-01-18")

collected_predictions <- data.frame()

for (t in levels(cleaned_data$time)) {
  temp_data <- simulated_jan2021 %>% mutate(time = t)
  collected_predictions <- bind_rows(collected_predictions, temp_data)
}

collected_predictions <- collected_predictions %>%
  mutate(
    predicted_means_scenario1 = predict(model_basic, newdata = collected_predictions),
    predicted_means_scenario2 = predict(model_extended, newdata = collected_predictions)
  )

#
scenario1_means <- collected_predictions %>%
  group_by(time) %>%
  summarise(luottamus = weighted.mean(predicted_means_scenario1*100, paino, na.rm = TRUE)) %>%
  mutate(scenario = "Simuloitu yleinen luottamustaso 2021 alku")

scenario2_means <- collected_predictions %>%
  group_by(time) %>%
  summarise(luottamus = weighted.mean(predicted_means_scenario2*100, paino, na.rm = TRUE)) %>%
  mutate(scenario = "+ taloushuolien taso 2021 alku")

means_real <- instituutiot_long %>% filter(instituutio =="Terveydenhuolto") %>%
  mutate(scenario ="Todellinen kehitys")
means_real$time <- as.character(means_real$time)
yhteen <- bind_rows(means_real,scenario2_means,scenario1_means)  %>%
  mutate(
    time = as.Date(time),  # Convert 'time' to Date format
    year_decimal = as.numeric(format(time, "%Y")) + (as.numeric(format(time, "%j")) / 365)
  )

max_year_decimal <- max(yhteen$year_decimal)

plot <- ggplot(yhteen, aes(x = year_decimal, y = luottamus, color = scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = luottamus_low, ymax = luottamus_upp, fill = scenario), alpha = 0.2, color = NA) +
  geom_point(data = yhteen %>% group_by(scenario) %>% filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = yhteen %>% group_by(scenario) %>% filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +

    theme(
      strip.text = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      text = element_text(family = "raleway"),
      plot.title = element_text(size = 14, family = "raleway", face = "bold"),
      axis.title = element_text(size = 12, family = "raleway"),
      axis.text = element_text(size = 10, family = "raleway"),
      legend.text = element_text(size = 10, family = "raleway"),
      legend.title = element_text(size = 12, family = "raleway", face = "bold")
    )  +
  labs(title = "",
       x = "",
       y = "",
       caption = "Lähde: Kansalaispulssit") +
  geom_text_repel(
    data = yhteen %>% group_by(scenario) %>% filter(year_decimal == max(year_decimal)),
    aes(label = scenario),
    family = "Lato",
    fontface = "bold",
    size = 3,
    direction = "y",
    nudge_x = 0.1,
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  expand_limits(x = max_year_decimal + 2)   +
  scale_x_continuous(
    breaks = seq(2021, 2024, by = 1),
    labels = seq(2021, 2024, by = 1)
  )

file <- here("02_output", "simulointi_d.svg")
ggsave(filename = file, plot = plot, device = "svg", width = 6, height = 4)


##############jatkuvien simulointi - huom aja jatkuvien kuviot ensin

cleaned_data <- read_csv("00_data/02_processed/yhdistetty_kansalaispulssit.csv") %>%
  select(-luottamus_eu, -luottamus_puolustusvoimat) %>%
  rename(healthcare_trust = luottamus_terveydenhuolto) %>%
  mutate(
    mean_trust_indicators = rowMeans(select(., starts_with("luottamus_")), na.rm = TRUE),
    healthcare_trust =  healthcare_trust,
    time = droplevels(as.factor(time))
  ) %>%
  filter(!is.na(mean_trust_indicators) & !is.na(q6) & !is.na(healthcare_trust) & !is.na(paino))
cleaned_data$time <- droplevels(cleaned_data$time)

model_basic <- lm(healthcare_trust ~ factor(time)*mean_trust_indicators,
                  data = cleaned_data,
                  weights = paino)

model_extended <- lm(healthcare_trust ~ factor(time)*(mean_trust_indicators + factor(q6)),
                     data = cleaned_data,
                     weights = paino)

simulated_jan2021 <- cleaned_data %>%
  filter(time == "2021-01-18")

collected_predictions <- data.frame()
for (t in levels(cleaned_data$time)) {
  temp_data <- simulated_jan2021 %>% mutate(time = t)
  collected_predictions <- bind_rows(collected_predictions, temp_data)
}

collected_predictions <- collected_predictions %>%
  mutate(
    predicted_means_scenario1 = predict(model_basic, newdata = collected_predictions),
    predicted_means_scenario2 = predict(model_extended, newdata = collected_predictions)
  )

scenario1_means <- collected_predictions %>%
  group_by(time) %>%
  summarise(luottamus = weighted.mean(predicted_means_scenario1, paino, na.rm = TRUE)) %>%
  mutate(scenario = "Yleisen luottamustaso muuttumattoma")

scenario2_means <- collected_predictions %>%
  group_by(time) %>%
  summarise(luottamus = weighted.mean(predicted_means_scenario2, paino, na.rm = TRUE)) %>%
  mutate(scenario = "+ taloushuolien taso muuttumattoma")

means_real <- instituutiot_long %>% filter(instituutio =="Terveydenhuolto") %>%
  mutate(scenario ="Todellinen kehitys")
means_real$time <- as.character(means_real$time)
yhteen <- bind_rows(means_real,scenario2_means,scenario1_means)  %>%
  mutate(
    time = as.Date(time),  # Convert 'time' to Date format
    year_decimal = as.numeric(format(time, "%Y")) + (as.numeric(format(time, "%j")) / 365)
  )

max_year_decimal <- max(yhteen$year_decimal)

plot <- ggplot(yhteen, aes(x = year_decimal, y = luottamus, color = scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = luottamus_ci_lower, ymax = luottamus_ci_upper, fill = scenario), alpha = 0.2, color = NA) +
  geom_point(data = yhteen %>% group_by(scenario) %>% filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = yhteen %>% group_by(scenario) %>% filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),  # Remove facet titles
    legend.position = "none",      # Remove legends
    panel.grid = element_blank()   # Remove all grid lines
  ) +
  labs(title = "",
       x = "",
       y = "") +
  geom_text_repel(
    data = yhteen %>% group_by(scenario) %>% filter(year_decimal == max(year_decimal)),
    aes(label = scenario),
    family = "Lato",
    fontface = "bold",
    size = 3,
    direction = "y",
    nudge_x = 0.1,  # Slightly nudge the labels to the right
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  expand_limits(x = max_year_decimal + 2)   +  # Expand x-axis to make room for labels
  scale_x_continuous(
    breaks = seq(2021, 2024, by = 1),        # Set x-axis breaks only up to 2024
    labels = seq(2021, 2024, by = 1)         # Set labels only for the specified breaks
  )

file <- here("02_output", "simulointi_j.svg")
ggsave(filename = file, plot = plot, device = "svg", width = 6, height = 4)



