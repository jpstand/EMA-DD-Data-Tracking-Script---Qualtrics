rm(list=ls())
library(tidyverse)
library(lubridate)
library(purrr)

#script assumes that exported data is in MT timezone (which it does by default if using qualtrics account)
#start_date contains the start date and id (participant.ID) for each participant (this is in the Master Key csv)
#current_ps will contain a csv with date participants began surveys, the participant's timezone, and their ID (will make life easier for rest of script)
current_ps <- read.csv("Master Key.csv")
ps_ids <- current_ps$participant.ID

study_days <- 28L
current_ps <- current_ps %>%
  mutate(tz_map = recode(TZ,
                         ET = "America/New_York",
                         CT = "America/Chicago",
                         MT = "America/Denver",
                         PT = "America/Los_Angeles",
                         .default = "America/Denver"),
         local_today_chr = purrr::map_chr(tz_map, ~ format(lubridate::with_tz(Sys.time(), .x), "%Y-%m-%d")),
         local_today = purrr::map2(tz_map, local_today_chr,
                                   ~ lubridate::with_tz(lubridate::ymd_hms(paste(.y, "00:00:00"), tz = .x), "UTC")) %>%
           do.call(c, .),
         yesterday_local_date = as.Date(local_today_chr) - 1L,
         yesterday_local = purrr::map2(tz_map, yesterday_local_date,
                                       ~ lubridate::with_tz(lubridate::ymd_hms(paste(.y, "23:59:59"), tz = .x), "UTC")) %>%
           do.call(c, .),
         Part3StartDate = mdy(Date_Began_Part_3),
         days_so_far = pmax(0L, pmin(study_days, as.integer(yesterday_local_date - Part3StartDate) + 1L)))


#4 surveys per day, S1 - 4 are EMA 1 - 4, S4 also contains the DD
#make sure to download csvs using labels and NOT values from qualtrics

read_svy <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
  if (nrow(df) >= 2) df <- df[-c(1, 2), , drop = FALSE] 
  df
}

#make sure to write in the correct csv name manually with most recent csv from qualtrics
S1 <- read_svy(list.files(pattern = "^EMA 1.*\\.csv$", full.names = TRUE)) %>% mutate(Survey = "EMA_1")
S2 <- read_svy(list.files(pattern = "^EMA 2.*\\.csv$", full.names = TRUE)) %>% mutate(Survey = "EMA_2")
S3 <- read_svy(list.files(pattern = "^EMA 3.*\\.csv$", full.names = TRUE)) %>% mutate(Survey = "EMA_3")
S4 <- read_svy(list.files(pattern = "^EMA 4 and DD.*\\.csv$", full.names = TRUE))  %>% mutate(Survey = "EMA_DD_4")


all_surveys <- bind_rows(S1,S2,S3,S4) %>%
  filter(Finished %in% c("True", "TRUE")) %>%
  filter(participant.ID %in% current_ps$participant.ID) %>%
  mutate(Finished = as.logical(Finished),
         EndDate = stringr::str_squish(EndDate))

all_surveys_clean <- all_surveys %>%
  filter(participant.ID %in% ps_ids) %>%
  mutate(EndDate = stringr::str_squish(EndDate),
         EndDate_ts = parse_date_time(EndDate,
                                      orders = c("mdy HMS","mdy HM",
                                                 "mdy IMS p","mdy IM p","mdy I p",
                                                 "mdy I:M:S p","mdy I:M p",
                                                 "Y-m-d H:M:S","Y-m-d H:M"),
                                      tz = "America/Denver")) %>%
  left_join(current_ps %>% select(participant.ID, TZ, local_today), by = "participant.ID") %>%
  mutate(TZ = toupper(trimws(TZ)),
         tz_map = recode(TZ,
                         ET = "America/New_York",
                         CT = "America/Chicago",
                         MT = "America/Denver",
                         PT = "America/Los_Angeles",
                         .default = "America/Denver"),
         tz_map = ifelse(tz_map %in% OlsonNames(), tz_map, "America/Denver"),
         EndDate_local = as.POSIXct(mapply(lubridate::with_tz, EndDate_ts, tz_map)),
         EndDate_day_local = as.Date(mapply(function(x, tz)
           format(x, tz = tz, format = "%Y-%m-%d"),
           EndDate_local, tz_map))) %>%
  filter(!is.na(EndDate_local), EndDate_local < local_today)

days_tbl <- current_ps %>%
  select(participant.ID, days_so_far)
#feel free to adjust the timeslots as needed to fit your study :)
get_slot <- function(dt, TZ) {
  tz_map <- c(ET = "America/New_York",
              CT = "America/Chicago",
              MT = "America/Denver",
              PT = "America/Los_Angeles")
  t_0930 <- as.numeric(hms::as_hms("09:30:00"))
  t_1100 <- as.numeric(hms::as_hms("11:00:00"))
  t_1330 <- as.numeric(hms::as_hms("13:30:00"))
  t_1500 <- as.numeric(hms::as_hms("15:00:00"))
  t_1730 <- as.numeric(hms::as_hms("17:30:00"))
  t_1900 <- as.numeric(hms::as_hms("19:00:00"))
  t_2130 <- as.numeric(hms::as_hms("21:30:00"))
  t_2300 <- as.numeric(hms::as_hms("23:00:00"))
  vapply(seq_along(dt), function(i) {
    code <- toupper(trimws(TZ[i]))
    olson <- if (code %in% names(tz_map)) tz_map[[code]] else "America/Denver"
    local_dt <- lubridate::with_tz(dt[i], olson)
    secs <- as.numeric(hms::as_hms(local_dt))
    dplyr::case_when(
      secs >= t_0930 & secs <= t_1100 ~ "EMA_1",
      secs >= t_1330 & secs <= t_1500 ~ "EMA_2",
      secs >= t_1730 & secs <= t_1900 ~ "EMA_3",
      secs >= t_2130 & secs <= t_2300 ~ "EMA_DD_4",
      TRUE ~ NA_character_
    )
  }, FUN.VALUE = character(1))
}
#this removes duplicate surveys done at the same time or surveys done within the same time slot by taking the survey that was first completed in that time slot
events_unique <- all_surveys_clean %>%
  inner_join(current_ps %>% select(participant.ID, Part3StartDate), by = "participant.ID") %>%
  mutate(slot = get_slot(EndDate_ts, TZ),
         day_num = as.integer(EndDate_day_local - Part3StartDate) + 1L ) %>%
  filter(Finished, !is.na(slot), between(day_num, 1L, study_days)) %>%
  arrange(participant.ID, day_num, slot, EndDate_ts) %>%
  distinct(participant.ID, slot, day_num, .keep_all = TRUE)

#this provides a df of each person's completed responses with the correct time slot identifier :D
events_tidy <- events_unique %>%
  mutate(label = if_else(slot == "EMA_DD_4",
                         paste0("EMA_DD_4_D", day_num),
                         paste0(slot, "_D", day_num))) %>%
  transmute(participant.ID, label, value = "1")

#this is just # of ema/dd completed
counts <- events_unique %>%
  mutate(is_dd= slot == "EMA_DD_4",
         is_ema = slot %in% c("EMA_1","EMA_2","EMA_3")) %>%
  group_by(participant.ID) %>%
  summarise(EMA_responses = sum(is_ema, na.rm = TRUE),
            DD_responses  = sum(is_dd,  na.rm = TRUE),
            .groups = "drop")

#this computes the total number of surveys completed and the adherence
info_needed <- events_unique %>%
  count(participant.ID, name = "surveys_completed") %>%
  right_join(days_tbl, by = "participant.ID") %>%
  mutate(
    surveys_completed = coalesce(surveys_completed, 0L),
    denom = pmax(1L, days_so_far * 4L),
    current_adherence = round(surveys_completed / denom, 4)) %>%
  select(participant.ID, days_so_far, surveys_completed, current_adherence)

#this gives us the order that each # EMA/DD should be completed in
per_day_order <- c("EMA_1","EMA_2","EMA_3","EMA_DD_4")
slot_names <- as.vector(outer(per_day_order, 1:study_days, function(s, d) paste0(s, "_D", d)))

#this gives us a template of what the final df should lok like in terms of ema/dd slot per day order
base_grid <- expand_grid(participant.ID = info_needed$participant.ID, label = slot_names) %>%
  left_join(info_needed %>% select(participant.ID, days_so_far), by = "participant.ID") %>%
  mutate(day_num = as.integer(str_extract(label, "(?<=_D)\\d+")),
         value = if_else(day_num > days_so_far, "", "0")) %>%
  select(participant.ID, label, value)

#this joins everything together and fills in all the slots completed by participants
final_slots <- base_grid %>%
  left_join(events_tidy, by = c("participant.ID","label"), suffix = c("_base","_comp")) %>%
  transmute(participant.ID, label, value = coalesce(value_comp, value_base)) %>%
  pivot_wider(names_from = label, values_from = value) %>%
  select(participant.ID, all_of(slot_names))

#this adds in all the info about the counts/adherence
final_df <- info_needed %>%
  left_join(final_slots, by = "participant.ID") %>%
  relocate(participant.ID, days_so_far, surveys_completed, current_adherence) %>%
  left_join(counts, by = "participant.ID") %>%
  relocate(EMA_responses, DD_responses, .after = surveys_completed) %>%
  mutate(row_order = match(participant.ID, current_ps$participant.ID)) %>%
  arrange(row_order) %>%
  select(-row_order)

#this writes it all up into one nice csv!
#write.csv(final_df, file = "Final Data Tracking.csv", row.names = FALSE)
