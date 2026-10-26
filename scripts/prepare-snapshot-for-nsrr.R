ver <- "0.1.0.pre"

library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(tibble)

src_dir <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20260901-barger-snapshot/nsrr-prep/source-data"
out_dir <- file.path("/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20260901-barger-snapshot/nsrr-prep/_releases/0.1.0.pre")

######################################## helpers ########################################
# reformat variable names
# normalize IDs so matching ignores case and stray whitespace
norm_id <- function(x) toupper(trimws(as.character(x)))

# baseline names: lowercase, collapse space or "/" runs into "_"
clean_names_baseline <- function(x) {
  x <- tolower(x)
  x <- gsub("[ /]+", "_", x)
  x
}

# other files: lowercase, collapse +, /, space, - into "_"
clean_names <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[+/ -]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# coerce a column to POSIXct (handles dates stored as text in Excel)
to_dt <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as_datetime(x))
  parse_date_time(as.character(x), orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                  tz = "UTC", quiet = TRUE)
}

# convert character columns that hold Excel serial numbers back to POSIXct
# (only for columns whose names say they are dates/times)
fix_excel_dt <- function(df) {
  dt_name <- grepl("(^date$|_date$|_time$|_at$)", names(df))
  for (col in names(df)[dt_name]) {
    x <- df[[col]]
    if (is.character(x)) {
      num <- suppressWarnings(as.numeric(x))
      ok  <- !is.na(x) & !is.na(num)
      if (any(ok)) {
        out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
        out[ok] <- as.POSIXct(num[ok] * 86400, origin = "1899-12-30", tz = "UTC")
        df[[col]] <- out
      }
    }
  }
  df
}

# sort POSIXct columns into time-only (Excel 1899 dummy date), date-only, full datetime
classify_dt <- function(df) {
  dt_cols <- names(df)[vapply(df, function(x) inherits(x, "POSIXct"), logical(1))]
  is_time_only <- vapply(dt_cols, function(col) {
    x <- df[[col]]
    all(is.na(x) | year(x) < 1901)
  }, logical(1))
  time_only <- dt_cols[is_time_only]
  rest      <- dt_cols[!is_time_only]
  is_date_only <- vapply(rest, function(col) {
    x <- df[[col]]
    all(is.na(x) | format(x, "%H:%M:%S") == "00:00:00")
  }, logical(1))
  list(time_only = time_only,
       date_only = rest[is_date_only],
       datetime  = rest[!is_date_only])
}

# US DST switch times
# spring forward: 2nd Sunday of March, 2 am; fall back: 1st Sunday of November, 2 am
first_sunday_on_or_after <- function(d) d + ((1 - wday(d)) %% 7)

dst_switches <- tibble(year = 2012:2018) %>%
  mutate(
    spring = as_datetime(first_sunday_on_or_after(make_date(year, 3, 1)) + 7) + hours(2),
    fall   = as_datetime(first_sunday_on_or_after(make_date(year, 11, 1))) + hours(2)
  )

switch_long <- bind_rows(
  dst_switches %>% transmute(switch_dt = spring, change = 1L),
  dst_switches %>% transmute(switch_dt = fall,   change = -1L)
)

clock_secs <- function(t) hour(t) * 3600 + minute(t) * 60 + second(t)

# DST status (1/0) at a timestamp; NA if the timestamp is NA or outside 2012-2018
is_dst <- function(t) {
  i <- match(year(t), dst_switches$year)
  as.integer(t >= dst_switches$spring[i] & t < dst_switches$fall[i])
}

######################################## read: baseline + demographics ########################################

baseline <- read_excel(file.path(src_dir, "SNAPSHOT_Baseline questionniare.xlsx"), guess_max = Inf)
demo     <- read_excel(file.path(src_dir, "SNAPSHOT_Demographics GPA Chronotype DLMO.xlsx"), guess_max = Inf)

# Clean up column names / casing mismatch
names(baseline) <- trimws(names(baseline))
names(demo)     <- trimws(names(demo))
names(baseline)[tolower(names(baseline)) == "subject code"] <- "Subject Code"
names(demo)[tolower(names(demo)) == "subject code"]         <- "Subject Code"

# Split baseline: 2_ columns become follow-up rows under the matching baseline columns
v2_cols <- grep("^2_", names(baseline), value = TRUE)

baseline_v1 <- baseline %>% select(-all_of(v2_cols))

baseline_v2 <- baseline %>%
  select(`Subject Code`, all_of(v2_cols)) %>%
  rename_with(~ sub("^2_", "", .x), all_of(v2_cols)) %>%
  filter(if_any(-`Subject Code`, ~ !is.na(.x))) %>%   # keep only subjects with follow-up data
  mutate(visit = 5)                                   # 5 = follow-up

# Baseline: demo + baseline (left join, demo is base)
merged_v1 <- left_join(demo, baseline_v1, by = "Subject Code", suffix = c("_demo", "_baseline")) %>%
  mutate(visit = 1)                                   # 1 = baseline

merged <- bind_rows(merged_v1, baseline_v2) %>%
  relocate(visit, .after = `Subject Code`) %>%
  arrange(`Subject Code`, visit)

names(merged) <- clean_names_baseline(names(merged))

timestamp_cols <- grep("survey_timestamp$", names(merged), value = TRUE)
mc_date_cols   <- grep("^mc[123]_(first|last)$", names(merged), value = TRUE)

merged <- merged %>%
  mutate(
    subject_code = as.character(subject_code),
    across(all_of(c(timestamp_cols, mc_date_cols)), to_dt)
  )

######################################## read: actigraphy ########################################

actigraphy <- read_excel(file.path(src_dir, "SNAPSHOT_Actigraphy.xlsx"),
                         sheet = "naps added no inpatient", guess_max = Inf)
names(actigraphy) <- clean_names(names(actigraphy))
names(actigraphy)[names(actigraphy) == "id"] <- "subject_code"
actigraphy <- fix_excel_dt(actigraphy)

actigraphy <- actigraphy %>% rename(any_of(c(actigraphy_study_day = "study_day")))

actigraphy <- actigraphy %>%
  mutate(visit = 3, .after = subject_code)            # 3 = actigraphy

######################################## read: eDiary ########################################

ediary <- read_excel(file.path(src_dir, "SNAPSHOT_Morning and Evening eDiary.xlsx"),
                     sheet = "CollegeSleep20171110-MIT-rrclea", guess_max = Inf)
names(ediary) <- clean_names(names(ediary))
names(ediary)[names(ediary) == "id"] <- "subject_code"
ediary <- fix_excel_dt(ediary)

# drop free-text comment columns from the release
ediary <- ediary %>% select(-any_of(c("m_comments", "e_comments")))

ediary <- ediary %>% rename(any_of(c(ediary_study_day = "study_day")))

ediary <- ediary %>%
  mutate(visit = 4, .after = subject_code)            # 4 = eDiary

######################################## read: weekly questionnaire ########################################

weekly <- read_excel(file.path(src_dir, "SNAPSHOT_Weekly questionniare.xlsx"),
                     sheet = "College-Sleep-Weekly-Diary-2025", guess_max = Inf)
weekly <- fix_excel_dt(weekly)

# ID is assumed to be the first column
id_col <- names(weekly)[1]

weekly <- weekly %>%
  mutate(visit = 2, .after = 1)                       # 2 = weekly questionnaire

######################################## per-participant date offset ########################################
# Index = baseline survey date. Each participant's real year (2013-2017) is
# mapped to an earlier calendar-matching year (same Jan 1 weekday, same leap status),
# and every date for that participant (all files) is shifted by the same number of days.

year_map <- tibble(
  real_year   = c(2013, 2014, 2015, 2016, 2017),
  mapped_year = c(2002, 2003, 2009, 1988, 2006)
)

index_baseline <- merged %>%
  filter(visit == 1, !is.na(survey_timestamp)) %>%
  transmute(subject_code = norm_id(subject_code), index_real = as_date(survey_timestamp))

# fallback for subjects with no baseline survey: earliest date in any other file
earliest_other <- bind_rows(
  actigraphy %>% transmute(subject_code = norm_id(subject_code), d = as_date(to_dt(start_date))),
  ediary     %>% transmute(subject_code = norm_id(subject_code), d = as_date(to_dt(m_date))),
  ediary     %>% transmute(subject_code = norm_id(subject_code), d = as_date(to_dt(e_date))),
  weekly     %>% transmute(subject_code = norm_id(.data[[id_col]]), d = as_date(to_dt(date)))
) %>%
  filter(!is.na(subject_code), trimws(subject_code) != "", !is.na(d)) %>%
  group_by(subject_code) %>%
  summarise(index_fallback = min(d), .groups = "drop")

offsets <- full_join(index_baseline, earliest_other, by = "subject_code") %>%
  mutate(
    index_real = coalesce(index_real, index_fallback),
    real_year  = year(index_real)
  ) %>%
  left_join(year_map, by = "real_year") %>%
  mutate(
    offset_days = as.integer(make_date(real_year, 1, 1) - make_date(mapped_year, 1, 1)),   # multiple of 7
    month_start = month(index_real)                                                         # real baseline month
  ) %>%
  select(subject_code, offset_days, month_start)

# shift date/datetime columns by the participant's offset
shift_dates <- function(df, cols, id = "subject_code") {
  if (length(cols) == 0) return(df)
  df %>%
    mutate(.id = norm_id(.data[[id]])) %>%
    left_join(offsets %>% select(.id = subject_code, offset_days), by = ".id") %>%
    mutate(across(all_of(cols), ~ .x - days(offset_days))) %>%
    select(-.id, -offset_days)
}

# classify, shift, and format all POSIXct columns in a dataset
shift_and_format <- function(df, id = "subject_code") {
  cls <- classify_dt(df)
  df %>%
    shift_dates(c(cls$date_only, cls$datetime), id = id) %>%
    mutate(
      across(all_of(cls$time_only), ~ format(.x, "%H:%M:%S")),   # clock times: not shifted
      across(all_of(cls$date_only), ~ format(.x, "%Y-%m-%d")),
      across(all_of(cls$datetime),  ~ format(.x, "%Y-%m-%d %H:%M:%S"))
    )
}

######################################## write: baseline ########################################

merged <- merged %>%
  mutate(survey_dst = is_dst(survey_timestamp)) %>%      # from REAL timestamp, before shifting
  shift_dates(c(timestamp_cols, mc_date_cols)) %>%
  mutate(.id = norm_id(subject_code)) %>%
  left_join(offsets %>% select(.id = subject_code, month_start), by = ".id") %>%
  select(-.id) %>%
  relocate(month_start, .after = visit) %>%
  relocate(survey_dst, .after = survey_timestamp)

merged <- merged %>%
  mutate(across(all_of(timestamp_cols), ~ format(.x, "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(across(all_of(mc_date_cols),   ~ format(.x, "%Y-%m-%d")))

write_csv(merged, file.path(out_dir, "snapshot-dataset.0.1.0.pre.csv"), na = "")

######################################## write: actigraphy ########################################

# DST from REAL dates, before shifting
actigraphy <- actigraphy %>%
  mutate(
    .row      = row_number(),
    .start_dt = to_dt(start_date) + seconds(clock_secs(to_dt(start_time))),
    .end_dt   = to_dt(end_date)   + seconds(clock_secs(to_dt(end_time))),
    .year     = year(.start_dt)
  ) %>%
  left_join(dst_switches, by = c(".year" = "year")) %>%
  mutate(dst = as.integer(.start_dt >= spring & .start_dt < fall)) %>%   # DST status at interval start
  select(-spring, -fall)

# dst_change: +1 spring forward, -1 fall back
#   (1) interval that spans the switch, or
#   (2) if none spans it, the first interval starting after the switch
#   only for subjects whose recording covers the switch
dst_flags <- actigraphy %>%
  filter(!is.na(.start_dt)) %>%
  select(.row, subject_code, .start_dt, .end_dt) %>%
  cross_join(switch_long) %>%
  arrange(subject_code, switch_dt, .start_dt) %>%
  group_by(subject_code, switch_dt) %>%
  filter(any(.start_dt < switch_dt), any(coalesce(.end_dt, .start_dt) >= switch_dt)) %>%
  mutate(
    spans       = .start_dt < switch_dt & !is.na(.end_dt) & .end_dt >= switch_dt,
    after       = .start_dt >= switch_dt,
    first_after = after & cumsum(after) == 1
  ) %>%
  filter(if (any(spans)) spans else first_after) %>%
  ungroup() %>%
  group_by(.row) %>%
  summarise(dst_change = first(change), .groups = "drop")

actigraphy <- actigraphy %>%
  left_join(dst_flags, by = ".row") %>%
  mutate(dst_change = coalesce(dst_change, 0L)) %>%
  relocate(dst, dst_change, .after = end_time) %>%
  select(-.row, -.start_dt, -.end_dt, -.year)

actigraphy <- shift_and_format(actigraphy)
write_csv(actigraphy, file.path(out_dir, "snapshot-actigraphy-intervals-dataset.0.1.0.pre.csv"), na = "")

######################################## write: eDiary ########################################

ediary <- ediary %>%
  mutate(
    m_created_dst = is_dst(to_dt(m_created_at)),         # from REAL timestamps, before shifting
    e_created_dst = is_dst(to_dt(e_created_at))
  ) %>%
  relocate(m_created_dst, .after = m_created_at) %>%
  relocate(e_created_dst, .after = e_created_at)

ediary <- shift_and_format(ediary)
write_csv(ediary, file.path(out_dir, "snapshot-ediary-dataset.0.1.0.pre.csv"), na = "")

######################################## write: weekly questionnaire ########################################

weekly <- weekly %>%
  mutate(created_dst = is_dst(to_dt(created_at))) %>%    # from REAL timestamp, before shifting
  relocate(created_dst, .after = created_at)

weekly <- shift_and_format(weekly, id = id_col)
write_csv(weekly, file.path(out_dir, "snapshot-weekly-questionnaire-dataset.0.1.0.pre.csv"), na = "")





#########################
#############harmonized dataset
##########################
# age, race, gender, ethnicity
harmonized_data <- merged[, c("subject_code", "visit", "age", "race", "sex", "ethnicity", "height_ft", "height_in", "weight_lbs")] %>%
  dplyr::filter(visit == 1) %>%
  dplyr::mutate(nsrrid = subject_code,
                nsrr_age = age,
                nsrr_race = dplyr::case_when(
                  race == "White" ~ "white",
                  race == "African Amer" ~ "black or african american",
                  race == "Asian" ~ "asian",
                  race == "More than one" ~ "multiple",
                  race == "Other" ~ "other",
                  TRUE ~ "not reported"
                ),
                nsrr_sex = dplyr::case_when(
                  sex == 1 ~ "male",
                  sex == 0 ~ "female",
                  TRUE ~ "not reported"
                ),
                nsrr_ethnicity = dplyr::case_when(
                  ethnicity == "Hispanic/Latino" ~ "hispanic or latino",
                  ethnicity == "Not Hispanic/Latino" ~ "not hispanic or latino",
                  TRUE ~ "not reported"
                ),
                nsrr_bmi = (weight_lbs * 0.453592) / (((height_ft * 12) + height_in) * 0.0254)^2,
                visit = 1
  ) %>%
  dplyr::select(nsrrid, visit, nsrr_age, nsrr_race, nsrr_sex, nsrr_ethnicity, nsrr_bmi)

write_csv(harmonized_data, file.path(out_dir, "snapshot-harmonized-dataset-0.1.0.pre.csv"), na = "")

