library(here)
library(utils)
library(readr)
library(lubridate)
library(tidyverse)
library(haven)

file_info <- data.frame(
  code = c("FSD3682", "FSD3783", "FSD3523", "FSD3517", "FSD3527", "FSD3518", "FSD3840", "FSD3758",
           "FSD3579", "FSD3782", "FSD3555", "FSD3859", "FSD3656", "FSD3736", "FSD3826", "FSD3537",
           "FSD3711", "FSD3592", "FSD3683", "FSD3839", "FSD3677", "FSD3773", "FSD3548", "FSD3821",
           "FSD3718", "FSD3530", "FSD3851", "FSD3599", "FSD3757", "FSD3664", "FSD3808", "FSD3694",
           "FSD3654", "FSD3646", "FSD3725", "FSD3562", "FSD3544", "FSD3570", "FSD3790", "FSD3613",
           "FSD3574", "FSD3896", "FSD3886", "FSD3866"),
  month = c(NA, NA, 3, 1, 4, 2, 10, 2, 13, 4, 9, 2, 3, 11, 8, 6, 8, 14, 6, 9, 5, 3, 8, 7, 9, 5, 1, 15, 1, 4, 6, 7, 2, 1, 10, 10, 7, 11, 5, 16, 12, 5, 4, 3),
  year = c(2021, 2022, 2021, 2021, 2021, 2021, 2023, 2023, 2021, 2023, 2021, 2024, 2022, 2022, 2023, 2021,
           2022, 2021, 2022, 2023, 2022, 2023, 2021, 2023, 2022, 2021, 2024, 2021, 2023, 2022, 2023, 2022,
           2022, 2022, 2022, 2021, 2021, 2021, 2023, 2021, 2021, 2024, 2024, 2024)
)

file_info_filtered <- file_info %>% filter(year %in% c(2023, 2024))
file_list <- split(file_info_filtered, seq(nrow(file_info_filtered)))

unzip_files <- function(file_list, folder = "00_data/01_raw") {
  for (file in file_list) {
    zip_file <- here(folder, paste0(file$code, ".zip"))
    dest_dir <- here(folder, paste0(file$code, "_unzipped"))

    if (file.exists(zip_file)) {
      unzip(zip_file, exdir = dest_dir)
      message("Unzipped: ", zip_file, " to ", dest_dir)
    } else {
      message("File not found: ", zip_file)
    }
  }
}

zip_file <- here("00_data/01_raw", paste0("FSD3682", ".zip"))
dest_dir <- here("00_data/01_raw", paste0("FSD3682", "_unzipped"))
unzip(zip_file, exdir = dest_dir)


zip_file <- here("00_data/01_raw", paste0("FSD3783", ".zip"))
dest_dir <- here("00_data/01_raw", paste0("FSD3783", "_unzipped"))
unzip(zip_file, exdir = dest_dir)
unzip_files(file_list)

file21 <- read_sav("00_data/01_raw/FSD3682_unzipped/FSD3682/Study/Data/daF3682_fin.sav") %>%
  select(starts_with("T"), q6, fsd_no, starts_with("q11"), contains("paino"), starts_with("kierros")) %>%
  rename(paino=kierrospaino,
         kierros=kierros21) %>%
  mutate(year=2021) %>%
  mutate(
    time = case_when(
      kierros == 1 ~ ymd("2021-01-18"),
      kierros == 2 ~ ymd("2021-02-08"),
      kierros == 3 ~ ymd("2021-03-01"),
      kierros == 4 ~ ymd("2021-03-22"),
      kierros == 5 ~ ymd("2021-04-12"),
      kierros == 6 ~ ymd("2021-05-03"),
      kierros == 7 ~ ymd("2021-05-24"),
      kierros == 8 ~ ymd("2021-06-14"),
      kierros == 9 ~ ymd("2021-07-05"),
      kierros == 10 ~ ymd("2021-08-09"),
      kierros == 11 ~ ymd("2021-08-30"),
      kierros == 12 ~ ymd("2021-09-20"),
      kierros == 13 ~ ymd("2021-10-11"),
      kierros == 14 ~ ymd("2021-11-01"),
      kierros == 15 ~ ymd("2021-11-22"),
      kierros == 16 ~ ymd("2021-12-13"),
      TRUE ~ NA_Date_
    )
  )

file22 <- read_sav("./00_data/01_raw/FSD3783_unzipped/FSD3783/Study/Data/daF3783_fin.sav") %>%
  select(starts_with("T"), q6, fsd_no, starts_with("q11"), contains("paino"), starts_with("kierros")) %>%
  rename(paino=kierrospaino,
         kierros=kierros22) %>%
  mutate(year=2022)  %>%
  mutate(
    time = case_when(
      kierros == 1 ~ ymd("2022-01-24"),
      kierros == 2 ~ ymd("2022-02-21"),
      kierros == 3 ~ ymd("2022-03-14"),
      kierros == 4 ~ ymd("2022-04-11"),
      kierros == 5 ~ ymd("2022-05-16"),
      kierros == 6 ~ ymd("2022-06-13"),
      kierros == 7 ~ ymd("2022-08-15"),
      kierros == 8 ~ ymd("2022-09-12"),
      kierros == 9 ~ ymd("2022-10-10"),
      kierros == 10 ~ ymd("2022-11-14"),
      kierros == 11 ~ ymd("2022-12-12"),
      TRUE ~ NA_Date_
    )
  )

import_and_filter_data <- function(file_list, folder = "00_data/01_raw") {
  data_list <- list()

  for (file in file_list) {
    csv_file_path <- here(folder, paste0(file$code, "_unzipped"), file$code, "Study", "Data", paste0("daF", substr(file$code, 4, nchar(file$code)), "_fin.sav"))

    if (file.exists(csv_file_path)) {
      data <- read_sav(csv_file_path)
      message("Imported: ", csv_file_path)

      filtered_data <- data %>% select(starts_with("T"), q6, fsd_no, starts_with("q11"), contains("paino"), starts_with("kierros"))

      data_list[[file$code]] <- filtered_data
    } else {
      message("File not found: ", csv_file_path)
    }
  }

  return(data_list)
}

imported_data <- import_and_filter_data(file_list)
combined_data <- bind_rows(imported_data)

file_info_filtered_new <- file_info_filtered %>%
  mutate(fsd_no = as.numeric(gsub("[^0-9]", "", code)))

merged_data <- combined_data %>% left_join(file_info_filtered_new, by = "fsd_no") %>%
  mutate(
    time = case_when(
      # Dates for 2023
      year == 2023 & month == 1 ~ ymd("2023-01-23"),
      year == 2023 & month == 2 ~ ymd("2023-02-20"),
      year == 2023 & month == 3 ~ ymd("2023-03-27"),
      year == 2023 & month == 4 ~ ymd("2023-05-02"),
      year == 2023 & month == 5 ~ ymd("2023-06-12"),
      year == 2023 & month == 6 ~ ymd("2023-08-21"),
      year == 2023 & month == 7 ~ ymd("2023-09-18"),
      year == 2023 & month == 8 ~ ymd("2023-10-16"),
      year == 2023 & month == 9 ~ ymd("2023-11-13"),
      year == 2023 & month == 10 ~ ymd("2023-12-11"),
      # Dates for 2024
      year == 2024 & month == 1 ~ ymd("2024-01-22"),
      year == 2024 & month == 2 ~ ymd("2024-02-19"),
      year == 2024 & month == 3 ~ ymd("2024-03-18"),
      year == 2024 & month == 4 ~ ymd("2024-04-29"),
      year == 2024 & month == 5 ~ ymd("2024-06-10"),
      TRUE ~ NA_Date_
    )
  )

combined_data_final <- bind_rows(merged_data,file21,file22)

combined_data_final <- combined_data_final %>%
  rename(
    luottamus_hallitus = q11k,
    luottamus_kuntahallinto = q11l,
    luottamus_eduskunta = q11m,
    luottamus_tuomioistuimet = q11n,
    luottamus_puolueet = q11o,
    luottamus_poliisi = q11p,
    luottamus_koulutus = q11r,
    luottamus_terveydenhuolto=q11q,
    luottamus_valtionhallinto = q11s,
    luottamus_tiedotusvalineet = q11t,
    luottamus_puolustusvoimat = q11x,
    luottamus_eu = q11y
  )

#https://services.fsd.tuni.fi/catalogue/FSD3896?tab=variables&study_language=fi
combined_data_final <- combined_data_final %>%
  mutate(
    t_education_group = case_when(
      t4 == 1 ~ "Enintään peruskoulu",
      t4 == 2 ~ "Toinen aste",
      t4 %in% 3:4 ~ "Korkeakoulutetut",
      t4 == 5 ~ "Joku muu",
      is.na(t4) ~ "Joku muu"
    ),
    t_age_group = case_when(
      t2 %in% 1:2 ~ "Alle 25-vuotiaat",
      t2 %in% 3:5 ~ "25-39-vuotiaat",
      t2 %in% 6:10 ~ "40-64-vuotiaat",
      t2 %in% 11:12 ~ "65-74-vuotiaat"
    ),
    t_sukupuoli = recode(as.character(t1),
                         "1" = "Miehet",
                         "2" = "Naiset",
                         "3" = "Muun sukupuoliset"),
    t_asumismuoto = case_when(
      t5 == 1 ~ "Yksin asujat",
      t5 == 2 & t6 == 1 ~ "Lapsiperheelliset",
      t5 == 2 & t6 == 2 ~ "Muu asumistyyppi",
      t5 == 3 ~ "Muu asumistyyppi",
      TRUE ~ "Muu asumistyyppi"
    ),
    t_varallisuus = recode(as.character(t7),
                           "1" = "Varakkaat",
                           "2" = "Hyvin toimeentulevat",
                           "3" = "Keskituloiset",
                           "4" = "Pienituloiset",
                           "5" = "En osaa sanoa"),
    t_maakunta = recode(as.character(t3),
                        "1" = "Etelä-Karjala",
                        "2" = "Etelä-Pohjanmaa",
                        "3" = "Etelä-Savo",
                        "4" = "Kainuu",
                        "5" = "Kanta-Häme",
                        "6" = "Keski-Pohjanmaa",
                        "7" = "Keski-Suomi",
                        "8" = "Kymenlaakso",
                        "9" = "Lappi",
                        "10" = "Pirkanmaa",
                        "11" = "Pohjanmaa",
                        "12" = "Pohjois-Karjala",
                        "13" = "Pohjois-Pohjanmaa",
                        "14" = "Pohjois-Savo",
                        "15" = "Päijät-Häme",
                        "16" = "Satakunta",
                        "17" = "Uusimaa",
                        "18" = "Varsinais-Suomi")
  )

combined_data_final <- combined_data_final %>%
  select(t_education_group,t_age_group,t_sukupuoli,t_asumismuoto,t_varallisuus,q6,
         t_maakunta,paino,time,starts_with("luottamus"))

write.csv(combined_data_final, here("00_data", "02_processed", "yhdistetty_kansalaispulssit.csv"), row.names = FALSE)
