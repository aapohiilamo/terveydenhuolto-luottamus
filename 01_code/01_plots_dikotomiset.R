library(here)
library(tidyverse)
library(ggplot2)
library(geomtextpath)
library(diagis)
library(tidyverse)
library(geomtextpath)
library(readr)
library(broom)
library(survey) # for survey analysis
library(srvyr) # for tidy survey analysis
library(showtext)

# Add Raleway font
font_add_google("Raleway", "raleway")
showtext_auto()

custom_colors <- c(
  rgb(196, 0, 60, maxColorValue = 255),    # Dark Red
  rgb(226, 127, 159, maxColorValue = 255), # Pink
  rgb(0, 123, 255, maxColorValue = 255),   # Vibrant Blue
  rgb(0, 150, 0, maxColorValue = 255),     # Strong Green
  rgb(255, 165, 0, maxColorValue = 255),   # Orange
  rgb(75, 0, 130, maxColorValue = 255),    # Indigo
  rgb(255, 20, 147, maxColorValue = 255),  # Deep Pink
  rgb(139, 69, 19, maxColorValue = 255),   # Saddle Brown
  rgb(54, 49, 68, maxColorValue = 255),  # Dark Goldenrod (replacing gold)
  rgb(0, 0, 0, maxColorValue = 255),  # Steel Blue
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
combined_data_final <- read_csv("00_data/02_processed/yhdistetty_kansalaispulssit.csv") |>
  select(-c(luottamus_eu, luottamus_puolustusvoimat))

long <- combined_data_final |>
  mutate(id = row_number()) |>
  select(starts_with("T"), paino, id, time, starts_with("luottamus")) |>
  pivot_longer(
    cols = -c(starts_with("T"), paino,id, time),
    names_to = "instituutio",
    values_to = "arvo"
  )

long_ <- long |>
  filter(instituutio!="luottamus_terveydenhuolto") |>
  group_by(across(starts_with("T")), paino, id, time) |>
  summarise(arvo=mean(arvo, na.rm = TRUE)) |>
  mutate(instituutio="luottamus_yleinen")

cmobined<-rbind(long,long_)

instituutiot_long <- cmobined |>
  group_by(time, instituutio) |>
  filter(!all(is.na(arvo))) |>
  mutate(arvo = case_when(
    arvo >= 0 & arvo <= 4 ~ 0,
    arvo >= 5 & arvo <= 10 ~ 1,
    TRUE ~ arvo
  )) |>
  filter(instituutio!="luottamus_yleinen") |>
  as_survey_design(weights = paino) |>
  summarise(observations=n(),
            luottamus=100*survey_mean(arvo, proportion = TRUE, prop_method="logit", vartype="ci")  ) |>
  mutate(year_decimal = as.numeric(format(time, "%Y")) + (as.numeric(format(time, "%j")) / 365))

meta_data <- instituutiot_long |> select(time,observations) |>
  group_by(time) |>
  slice(1) |>
  ungroup()

instituutiot_long <- instituutiot_long |>
  mutate(instituutio = recode(instituutio,
                              "luottamus_hallitus" = "Hallitus",
                              "luottamus_terveydenhuolto" = "Terveydenhuolto",
                              "luottamus_koulutus" = "Koulutus",
                              "luottamus_eduskunta" = "Eduskunta",
                              "luottamus_kuntahallinto" = "Kuntahallinto",
                              "luottamus_valtionhallinto" = "Valtionhallinto",
                              "luottamus_yleinen" = "Yleinen luottamus",
                              "luottamus_2yleinen" = "Yleinen 2",
                              "luottamus_puolueet" = "Puolueet",
                              "luottamus_tuomioistuimet" = "Tuomioistuimet",
                              "luottamus_poliisi" = "Poliisi",
                              "luottamus_tiedotusvalineet" = "Tiedotusvälineet"
  ))

#jarjestetaan luottamuksen mukaan
last_time_order <- instituutiot_long |>
  group_by(instituutio) |>
  filter(year_decimal == max(year_decimal)) |>
  arrange(desc(luottamus)) |>
  pull(instituutio)

instituutiot_long <- instituutiot_long |>
  mutate(instituutio = factor(instituutio, levels = last_time_order))

plot <- ggplot(instituutiot_long, aes(x = year_decimal, y = luottamus, color = instituutio)) +
  geom_textline(aes(label = instituutio), size = 4, text_smoothing = 80, vjust = -1.2, hjust = 0) +
  geom_ribbon(aes(ymin = luottamus_low, ymax = luottamus_upp, fill = instituutio), alpha = 0.2, color = NA) +
  geom_point(data = instituutiot_long |> group_by(instituutio) |> filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = instituutiot_long |> group_by(instituutio) |> filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~instituutio, nrow = 2) +
  scale_y_continuous(limits = c(50, 120), breaks = seq(60, 100, by = 20)) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    text = element_text(family = "raleway"),
    plot.title = element_text(size = 14, family = "raleway", face = "bold"),
    axis.title = element_text(size = 12, family = "raleway"),
    axis.text = element_text(size = 10, family = "raleway"),
    legend.text = element_text(size = 10, family = "raleway"),
    legend.title = element_text(size = 12, family = "raleway", face = "bold")
  )  +
  labs(title = "Luottavien osuus (%)",
       x = "",
       y = "",
       caption = "Lähde: Kansalaispulssit")

file <- here("02_output", "luottamus_instituutiot_d.svg")
ggsave(filename = file, plot = plot, device = "svg", width = 10, height = 4)

custom_colors2 <- c(
  rgb(75, 0, 130, maxColorValue = 255))

terveyden_long <-  filter(instituutiot_long, instituutio=="Terveydenhuolto")
plot <- ggplot(terveyden_long, aes(x = year_decimal, y = luottamus, color = instituutio)) +
  geom_textline(aes(label = instituutio), size = 4, text_smoothing = 80, vjust = -1.2, hjust = 0) +
  geom_ribbon(aes(ymin = luottamus_low, ymax = luottamus_upp, fill = instituutio), alpha = 0.2, color = NA) +
  geom_point(data = terveyden_long |> group_by(instituutio) |> filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = terveyden_long |> group_by(instituutio) |> filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  scale_color_manual(values = custom_colors2) +
  scale_fill_manual(values = custom_colors2) +
  facet_wrap(~instituutio, nrow = 2) +
  scale_y_continuous(limits = c(75, 105), breaks = seq(80, 100, by = 10)) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    text = element_text(family = "raleway"),
    plot.title = element_text(size = 14, family = "raleway", face = "bold"),
    axis.title = element_text(size = 12, family = "raleway"),
    axis.text = element_text(size = 10, family = "raleway"),
    legend.text = element_text(size = 10, family = "raleway"),
    legend.title = element_text(size = 12, family = "raleway", face = "bold")
  )  +
  labs(title = "Luottavien osuus (%)",
       x = "",
       y = "")

file <- here("02_output", "luottamus_terveydenhuolto_d.svg")
ggsave(filename = file, plot = plot, device = "svg", width = 4, height = 4)

################################################

#Maakuntien mukaan

################################################
long_format_data <- read_csv("00_data/02_processed/yhdistetty_kansalaispulssit.csv") |>
  select(-luottamus_eu, -luottamus_puolustusvoimat, -t_education_group, -t_age_group, -t_sukupuoli, -t_asumismuoto,
         -t_varallisuus) |>
  rename(healthcare_trust = luottamus_terveydenhuolto) |>
  pivot_longer(
    cols = starts_with("t_"),
    names_to = "covariate",
    values_to = "covariate_value"
  ) |>
  select(time, covariate, covariate_value, paino, healthcare_trust)

long_format_data <- long_format_data |>
  group_by(time, covariate_value) |>
  filter(!all(is.na(healthcare_trust))) |>
  mutate(arvo = case_when(
    healthcare_trust >= 0 & healthcare_trust <= 4 ~ 0,
    healthcare_trust >= 5 & healthcare_trust <= 10 ~ 1,
    TRUE ~ healthcare_trust
  )) |>
  as_survey_design(weights = paino) |>
  summarise(observations=n(),
            luottamus=100*survey_mean(arvo, proportion = TRUE, prop_method="logit", vartype="ci")
  ) |>
  mutate(
    year_decimal = as.numeric(format(time, "%Y")) + (as.numeric(format(time, "%j")) / 365)) |>
  filter(!(covariate_value %in% c("Muun sukupuoliset", "Joku muu", " ",
                                  "En osaa sanoa", NA)))

#jarjestetaan luottamuksen mukaan
last_time_order <- long_format_data |>
  group_by(covariate_value) |>
  filter(year_decimal == max(year_decimal)) |>
  arrange(desc(-luottamus)) |>
  pull(covariate_value)

long_format_data <- long_format_data |>
  mutate(covariate_value = factor(covariate_value, levels = last_time_order))


plot <- ggplot(long_format_data, aes(x = year_decimal, y = luottamus, color = covariate_value)) +
  geom_textline(aes(label = covariate_value), size = 3, text_smoothing = 90, vjust = -2, hjust = 0) +
  geom_ribbon(aes(ymin = luottamus_low, ymax = luottamus_upp, fill = covariate_value), alpha = 0.2, color = NA) +
  geom_point(data = long_format_data |> group_by(covariate_value) |> filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = long_format_data |> group_by(covariate_value) |> filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  facet_wrap(~covariate_value, nrow = 2) +
  scale_y_continuous(limits = c(50, 120), breaks = seq(60, 100, by = 20)) +
  scale_x_continuous(breaks = c(2021, 2022, 2023, 2024), labels = c("2021", "", "", "2024")) +  # Grid lines for each year, labels only for 2021 and 2024
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    text = element_text(family = "raleway"),
    plot.title = element_text(size = 14, family = "raleway", face = "bold"),
    axis.title = element_text(size = 12, family = "raleway"),
    axis.text = element_text(size = 8, family = "raleway"),  # Smaller x-axis labels
    legend.text = element_text(size = 10, family = "raleway"),
    legend.title = element_text(size = 12, family = "raleway", face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", size = 0.5),  # Lighter and thinner grid lines
    plot.caption = element_text(hjust = 1, size = 8, family = "raleway")  # Caption styling, aligned to the right
  ) +
  labs(title = "Terveydenhuoltojärjestelmään luottavien osuus (%)",
       x = "",
       y = "",
       caption = "Lähde: Kansalaispulssit")  # Note in the bottom right corner

file <- here("02_output", "luottamus_maakunnat_d.svg")
ggsave(filename = file, plot = plot, device = "svg", width = 12, height = 6)

################################################

#väestöryhmien mukaan

################################################

cleaned_data <- read_csv("00_data/02_processed/yhdistetty_kansalaispulssit.csv") |>
  select(-luottamus_eu, -luottamus_puolustusvoimat, -t_maakunta) |>
  rename(healthcare_trust = luottamus_terveydenhuolto)

long_format_data <- cleaned_data |>
  pivot_longer(
    cols = starts_with("t_"),
    names_to = "covariate",
    values_to = "covariate_value"
  ) |>
  select(time, covariate, covariate_value, paino, healthcare_trust) |>
  group_by(time, covariate_value) |>
  filter(!all(is.na(healthcare_trust))) |>
  mutate(arvo = case_when(
    healthcare_trust >= 0 & healthcare_trust <= 4 ~ 0,
    healthcare_trust >= 5 & healthcare_trust <= 10 ~ 1,
    TRUE ~ healthcare_trust
  )) |>
  as_survey_design(weights = paino) |>
  summarise(observations=n(),
            luottamus=100*survey_mean(arvo, proportion = TRUE, prop_method="logit", vartype="ci")) |>
  mutate(
    year_decimal = as.numeric(format(time, "%Y")) + (as.numeric(format(time, "%j")) / 365))  |>
  filter(!(covariate_value %in% c("Muun sukupuoliset", "Joku muu", " ",
                                  "En osaa sanoa", NA)))

#jarjestetaan luottamuksen mukaan
last_time_order <- long_format_data |>
  group_by(covariate_value) |>
  filter(year_decimal == max(year_decimal)) |>
  arrange(desc(-luottamus)) |>
  pull(covariate_value)

long_format_data <- long_format_data |>
  mutate(covariate_value = factor(covariate_value, levels = last_time_order))

plot <- ggplot(long_format_data, aes(x = year_decimal, y = luottamus, color = covariate_value)) +
  geom_textline(aes(label = covariate_value), size = 3, text_smoothing = 90, vjust = -2, hjust = 0) +
  geom_ribbon(aes(ymin = luottamus_low, ymax = luottamus_upp, fill = covariate_value), alpha = 0.2, color = NA) +
  geom_point(data = long_format_data |> group_by(covariate_value) |> filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = long_format_data |> group_by(covariate_value) |> filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  facet_wrap(~covariate_value, nrow = 2) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  scale_y_continuous(limits = c(70, 110), breaks = seq(80, 100, by = 20)) +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    text = element_text(family = "raleway"),
    plot.title = element_text(size = 14, family = "raleway", face = "bold"),
    axis.title = element_text(size = 12, family = "raleway"),
    axis.text = element_text(size = 10, family = "raleway"),
    legend.text = element_text(size = 10, family = "raleway"),
    legend.title = element_text(size = 12, family = "raleway", face = "bold")
  )  +
  labs(title = "Terveydenhuoltojärjestelmään luottavien osuus (%)",
       x = "",
       y = "")


plot <- ggplot(long_format_data, aes(x = year_decimal, y = luottamus, color = covariate_value)) +
  geom_textline(aes(label = covariate_value), size = 3, text_smoothing = 90, vjust = -2, hjust = 0) +
  geom_ribbon(aes(ymin = luottamus_low, ymax = luottamus_upp, fill = covariate_value), alpha = 0.2, color = NA) +
  geom_point(data = long_format_data |> group_by(covariate_value) |> filter(year_decimal == min(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  geom_point(data = long_format_data |> group_by(covariate_value) |> filter(year_decimal == max(year_decimal)), aes(x = year_decimal, y = luottamus), size = 2) +
  facet_wrap(~covariate_value, nrow = 2) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  scale_y_continuous(limits = c(70, 110), breaks = seq(80, 100, by = 20)) +
  scale_x_continuous(breaks = c(2021, 2022, 2023, 2024), labels = c("2021", "", "", "2024")) +  # Grid lines for each year, labels only for 2021 and 2024
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    text = element_text(family = "raleway"),
    plot.title = element_text(size = 14, family = "raleway", face = "bold"),
    axis.title = element_text(size = 12, family = "raleway"),
    axis.text = element_text(size = 8, family = "raleway"),  # Smaller x-axis labels
    legend.text = element_text(size = 10, family = "raleway"),
    legend.title = element_text(size = 12, family = "raleway", face = "bold"),
    panel.grid.major.x = element_line(color = "gray90", size = 0.5),  # Lighter and thinner grid lines
    plot.caption = element_text(hjust = 1, size = 8, family = "raleway")  # Caption styling, aligned to the right
  ) +
  labs(title = "Terveydenhuoltojärjestelmään luottavien osuus (%)",
       x = "",
       y = "",
       caption = "Lähde: Kansalaispulssit")  # Note in the bottom right corner


file <- here("02_output", "luottamus_ryhmat_d.svg")
ggsave(filename = file, plot = plot, device = "svg", width = 12, height = 6)
