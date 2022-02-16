library(tabulizer)
library(tidyverse)
library(lubridate)
library(arrow)



#Some important Variables to define the overall list of sessions to process
session <- c("FP1", "FP2", "FP3" ,"FP4","Q1","Q2","WUP","RAC")
event <- c("QAT", "DOH", "POR", "SPA", "FRA", "ITA", "CAT", "GER", "NED", "STY", "AUT", "GBR", "ARA", "RSM", "AME", "EMI", "ALR", "VAL")
year <- c(2018,2019,2020,2021)

input_df = read_csv_arrow("Yearly_Circuits.csv")

year <- input_df[[1]]
event <- input_df[[2]]


AA <- c(164.88604, 53.921875, 731.85, 314.978125)
AB <- c(164.88604, 317.953125, 731.85, 576.7781250)
ZA <- c(40.90625, 53.921875, 731.85, 314.978125)
ZB <- c(40.90625, 317.9531255, 731.85, 576.778125)

entry_area <- c(134.12389, 60.92034, 444.59587, 550.22420)

# Regexps:
rexp_times <- "(\\d+\\'\\d{2}\\.\\d{3}\\*?|\\d{2,3}\\.\\d{3}\\*?)"
rexp_pos <- "(\\d+)(?=(st|nd|rd|th))"
rexp_total_laps <- "(?<=Total laps\\=)\\d+"
rexp_full_laps <- "(?<=Full laps\\=)\\d+"
rexp_runs <- "(?<=Runs\\=)\\d+"
rexp_f_tire <- "(?<=(F|f)ront\\s{0,100}Tyre\\s{0,100})(((S|s)lick|(W|w)et)-(Hard|Medium|Soft))"
rexp_r_tire <- "(?<=(R|r)ear\\s{0,100}Tyre\\s{0,100})(((S|s)lick|(W|w)et)-(Hard|Medium|Soft))"
rexp_tire_life <- "(\\d+(?= Laps at start)|New Tyre)"
rexp_speed <- "\\d{2,3}\\.\\d{1}(?!\\d)"
rexp_rider_number <- "(?<=((\\d{1,2})?(1st|2nd|3rd|\\dth))\\s{0,100})\\d+"
rexp_run_number <- "(?<=Run\\s?#\\s?)\\d+"
rexp_lap_number <- "\\d+(?=\\s)"
rexp_lap_minutes <- "^\\d+(?=\\')"
rexp_lap_seconds <- "(?<=\\')\\d+\\.\\d+"



events_df <- expand.grid(year, event, session) %>%
  as_tibble() %>%
  rename(year = Var1, event = Var2, session = Var3) %>% 
  distinct()

entries_df <- events_df %>%
  select(year, event) %>%
  unique()


master_event_list <- split(events_df, seq(nrow(events_df)))
entries_list <- split(entries_df, seq(nrow(entries_df)))



createResultsUrl <- function(row) {
  url <- c(
    paste0("http://resources.motogp.com/files/results/", row$year, "/", row$event, "/MotoGP/", row$session,"/Analysis.pdf"),
    row
  )
  url
}

createEntryUrl <- function(row) {
  url <- c(
    # https://resources.motogp.com/files/results/2006/MotoGP/SPA/entry.pdf
    paste0("https://resources.motogp.com/files/results/", row$year, "/", row$event, "/MotoGP/Entry.pdf"),
    row
  )
  url
}


results_urls <- map(master_event_list, createResultsUrl)
entries_urls <- map(entries_list, createEntryUrl)

#Get The Area Function
GetArea <- function(x) {
  y <- vector('list',x)
  for (i in seq(x)) {
    if (i == 1) { y[[i]] <- AA}
    else if (i == 2) { y[[i]] <- AB}
    else if (i %% 2 != 0) {y[[i]] <- ZA}
    else {y[[i]] <- ZB}
  }
  return(y)
}

cleanTables <- function(table_strings) {
  table_strings[ ,which(!apply(table_strings,2,FUN = function(x){all(x == "")}))] %>% 
    as_tibble() %>% 
    unite("data", sep=" ")
}

processResultsUrl <- function(url_data){
  
  url <- url_data[[1]]
  year_ <- url_data[[2]]
  event_ <- url_data[[3]]
  session_ <- url_data[[4]]
  try{
    p <- get_n_pages(url)
  }
  pages <- as.numeric(as.integer(seq(1, p + .5, .5)))
  area <- GetArea(2*p)
  
  ts <- extract_tables(url, pages = pages, guess = FALSE, area = area)

  ts <- map(ts, cleanTables)
  print(url)
  
  results <- ts %>% 
    reduce(rbind) %>% 
    as_tibble() %>% 
    mutate(
      year = year_,
      event = event_,
      session = session_
    )
  
  results
}

processEntriesUrl <- function(url_data){
  url <- url_data[[1]]
  year_ <- url_data[[2]]
  event_ <- url_data[[3]]
  riders <- extract_tables(url, pages=1, guess=FALSE, area = list(entry_area), output = "data.frame")[[1]] %>%
    as_tibble() %>% 
    mutate(
      year = year_,
      event = event_
    ) %>% 
    rename(
      rider_number = X.1,
      rider = Rider,
      nation = Nation,
      team = Team,
      motorcycle = Motorcycle
    )
  
  riders
  
}



results <- map(results_urls, processResultsUrl) %>% reduce(rbind) %>% as_tibble()

entries <- map(entries_urls, processEntriesUrl) %>% reduce(rbind) %>% as_tibble()


df <- results %>% 
  mutate(total_laps   = as.integer(str_extract(data, rexp_total_laps)),
         full_laps    = as.integer(str_extract(data, rexp_full_laps)),
         speed       = as.double(str_extract(data, rexp_speed)),
         front_tire   = str_extract(data, rexp_f_tire),
         rear_tire    = str_extract(data, rexp_r_tire),
         run_number  = as.integer(str_extract(data, rexp_run_number)),
         rider_number = as.integer(str_extract(data, rexp_rider_number)),
         pitting     = str_extract(data, 'P'),
         lap_invalidated = !is.na(str_extract(data, "\\*")),
         lap_unfinished = case_when(str_extract(data, "unfinished") == "unfinished" ~ TRUE, TRUE ~ FALSE),
         lap_time     = case_when((!is.na(lap_unfinished) & !lap_unfinished) ~ str_extract(data, rexp_times)),
         rider_position = as.integer(str_extract(data, rexp_pos))
  ) %>% 
  left_join(entries, by = c("year", "event", "rider_number")) %>% 
  fill(c("rider_number","X","rider","nation","team","motorcycle","run_number","front_tire","rear_tire","total_laps","full_laps", "rider_position")) %>%
  slice(-1) %>%
  group_by(rider, data) %>% 
  mutate(
    T1 = str_extract_all(data, rexp_times)[[1]][2],
    T2 = str_extract_all(data, rexp_times)[[1]][3],
    T3 = str_extract_all(data, rexp_times)[[1]][4],
    T4 = str_extract_all(data, rexp_times)[[1]][5]
  ) %>% 
  mutate(
    front_tire_age = str_extract_all(data, rexp_tire_life)[[1]][1],
    rear_tire_age  = str_extract_all(data, rexp_tire_life)[[1]][2],
    front_tire_age = as.integer(recode(front_tire_age, "New Tyre" = "0")),
    rear_tire_age = as.integer(recode(rear_tire_age, "New Tyre" = "0")),
    is_lap = !is.na(lap_time)
  ) %>%
  group_by(rider, is_lap, year, event, session) %>% 
  mutate(
    lap_number = case_when(is_lap ~row_number()),
    lap_type =  case_when(
      !is.na(pitting) & is_lap ~ "In",
      !is.na(lag(pitting)) & is_lap  ~ "Out",
      lap_number == 1 ~ "Out",
      TRUE ~ "Speed"
    ),
    run_number = case_when(is_lap ~ run_number)
  ) %>%
  rename(
    rider_classification = X
  ) %>% 
  select(data, is_lap, year, event, session, rider_position, rider_number, rider:motorcycle, rider_classification, total_laps, full_laps, run_number, front_tire, rear_tire, front_tire_age, rear_tire_age, lap_invalidated,lap_unfinished, lap_number, lap_type, lap_time, T1, T2, T3, T4, speed) %>%
  group_by() %>% 
  fill(c("front_tire_age", "rear_tire_age")) %>%
  group_by(rider, run_number, is_lap, year, event, session) %>%
  mutate(
    front_tire_age = min(front_tire_age, na.rm=TRUE) + row_number() - 1,
    rear_tire_age = min(rear_tire_age, na.rm=TRUE) + row_number() - 1
  ) %>%
  group_by() %>%
  filter(is_lap) %>%
  select(-is_lap) %>%
  mutate(
    invalidated_T1 = str_extract(T1, "\\*") == "*",
    invalidated_T2 = str_extract(T2, "\\*") == "*",
    invalidated_T3 = str_extract(T3, "\\*") == "*",
    invalidated_T4 = str_extract(T4, "\\*") == "*",
    T1 = str_replace(T1, "\\*", ""),
    T2 = str_replace(T2, "\\*", ""),
    T3 = str_replace(T3, "\\*", ""),
    T4 = str_replace(T4, "\\*", ""),
    lap_time_seconds = as.double(str_extract(lap_time, rexp_lap_minutes)) * 60.0 + as.double(str_extract(lap_time, rexp_lap_seconds)),
    T1 = if_else(!is.na(str_match(T1, "\\d+\\'\\d{2}\\.\\d{3}")),
                 as.double(str_extract(T1, rexp_lap_minutes)) * 60.0 + as.double(str_extract(T1, rexp_lap_seconds)),
                 as.double(T1)),
    T2 = if_else(!is.na(str_match(T2, "\\d+\\'\\d{2}\\.\\d{3}")),
                 as.double( str_extract(T2, rexp_lap_minutes)) * 60.0 + as.double(str_extract(T2, rexp_lap_seconds)),
                 as.double(T2)),
    T3 = if_else(!is.na(str_match(T3, "\\d+\\'\\d{2}\\.\\d{3}")),
                 as.double( str_extract(T3, rexp_lap_minutes)) * 60.0 + as.double(str_extract(T3, rexp_lap_seconds)),
                 as.double(T3)),
    T4 = if_else(!is.na(str_match(T4, "\\d+\\'\\d{2}\\.\\d{3}")),
                 as.double(str_extract(T4, rexp_lap_minutes)) * 60.0 + as.double(str_extract(T4, rexp_lap_seconds)),
                 as.double(T4))
  ) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  mutate(
    single_qually_session = case_when(session == 'Q1' ~ "Q", session == 'Q2' ~ "Q", TRUE~session)
  )







