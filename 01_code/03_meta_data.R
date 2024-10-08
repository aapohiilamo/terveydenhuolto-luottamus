library(tibble)
library(dplyr)
library(lubridate)

data_2021 <- tibble(
  Kierros = c("1. kierros", "2. kierros", "3. kierros", "4. kierros", "5. kierros",
              "6. kierros", "7. kierros", "8. kierros", "9. kierros", "10. kierros",
              "11. kierros", "12. kierros", "13. kierros", "14. kierros", "15. kierros", "16. kierros"),
  Kenttäaika = c("13. - 18.1.", "3. - 8.2.", "24.2. - 1.3.", "17.3. - 22.3.", "7. - 12.4.",
                 "28.4. - 3.5.", "19. - 24.5.", "9. - 14.6.", "30.6. - 5.7.", "4. - 9.8.",
                 "25. - 30.8.", "15. - 20.9.", "6. - 11.10.", "27.10. - 1.11.", "17.11. - 22.11.", "8.12. - 13.12."),
  Otos = c(2428, 2578, 2840, 2850, 2625, 2768, 2756, 2749, 2779, 2686, 2633, 2633, 2596, 2627, 2735, 2833),
  Hyväksyttyjä_vastauksia = c(1187, 1343, 1510, 1465, 1287, 1292, 1261, 1285, 1246, 1288, 1274, 1192, 1216, 1154, 1305, 1200),
  Lopullinen_vastausosuus = c("48,9 %", "52,1 %", "53,2 %", "51,4 %", "49,0 %",
                              "46,7 %", "45,8 %", "46,7 %", "44,8 %", "48,0 %",
                              "48,4 %", "45,3 %", "46,8 %", "43,9 %", "47,7 %", "42,4 %"),
  Year = rep(2021, 16),
  LastDate = ymd(c("2021-01-18", "2021-02-08", "2021-03-01", "2021-03-22", "2021-04-12",
                   "2021-05-03", "2021-05-24", "2021-06-14", "2021-07-05", "2021-08-09",
                   "2021-08-30", "2021-09-20", "2021-10-11", "2021-11-01", "2021-11-22", "2021-12-13"))
)

data_2022 <- tibble(
  Kierros = c("1. kierros", "2. kierros", "3. kierros", "4. kierros", "5. kierros",
              "6. kierros", "7. kierros", "8. kierros", "9. kierros", "10. kierros", "11. kierros"),
  Kenttäaika = c("19. - 24.1.", "16. - 21.2.", "9. - 14.3.", "6. - 11.4.", "11. - 16.5.",
                 "8. - 13.6.", "10. – 15.8.", "7. – 12.9.", "5. – 10.10.", "9. – 14.11.", "7. - 12.12."),
  Otos = c(2842, 2858, 2495, 2521, 2589, 2545, 2616, 2546, 2631, 2567, 2680),
  Hyväksyttyjä_vastauksia = c(1435, 1309, 1131, 1056, 1148, 1080, 1110, 1082, 1105, 1114, 1220),
  Lopullinen_vastausosuus = c("50,5 %", "45,8 %", "45,3 %", "41,9 %", "44,3 %",
                              "42,4 %", "42,4 %", "42,5 %", "42,0 %", "43,4 %", "45,5 %"),
  Year = rep(2022, 11),
  LastDate = ymd(c("2022-01-24", "2022-02-21", "2022-03-14", "2022-04-11", "2022-05-16",
                   "2022-06-13", "2022-08-15", "2022-09-12", "2022-10-10", "2022-11-14", "2022-12-12"))
)

data_2023 <- tibble(
  Kierros = c("1. kierros", "2. kierros", "3. kierros", "4. kierros", "5. kierros",
              "6. kierros", "7. kierros", "8. kierros", "9. kierros", "10. kierros"),
  Kenttäaika = c("18.–23.1.", "15.–20.2.", "22.–27.3.", "26.4.–2.5.", "7.–12.6.",
                 "16.–21.8.", "13.–18.9.", "11.–16.10.", "8.–13.11.", "5.–11.12."),
  Otos = c(2737, 2622, 2640, 2728, 2669, 2735, 2692, 2640, 2972, 2913),
  Hyväksyttyjä_vastauksia = c(1231, 1231, 1101, 1123, 1024, 1047, 1078, 1085, 1159, 1132),
  Lopullinen_vastausosuus = c("45,0 %", "46,9 %", "41,7 %", "41,2 %", "38,4 %",
                              "38,3 %", "40,0 %", "41,1 %", "39,0 %", "38,9 %"),
  Year = rep(2023, 10),
  LastDate = ymd(c("2023-01-23", "2023-02-20", "2023-03-27", "2023-05-02", "2023-06-12",
                   "2023-08-21", "2023-09-18", "2023-10-16", "2023-11-13", "2023-12-11"))
)

data_2024 <- tibble(
  Kierros = c("1. kierros", "2. kierros", "3. kierros", "4. kierros", "5. kierros"),
  Kenttäaika = c("17.-22.1.2024", "14.-19.2.2024", "13.-18.3.2024", "24.-29.4.2024", "5.-10.6.2024"),
  Otos = c(2805, 2732, 2841, 2832, 3005),
  Hyväksyttyjä_vastauksia = c(977, 1106, 1104, 1123, 1082),
  Lopullinen_vastausosuus = c("34.8 %", "40.5 %", "38.9 %", "39.7 %", "36.0 %"),
  Year = rep(2024, 5),
  LastDate = ymd(c("2024-01-22", "2024-02-19", "2024-03-18", "2024-04-29", "2024-06-10"))
)

combined_data <- bind_rows(data_2021, data_2022, data_2023, data_2024)

print(combined_data)

metaaa <- combined_data %>%
  full_join(meta_data, by = c("LastDate" = "time"))
metaaa$Hyväksyttyjä_vastauksia-metaaa$observations

