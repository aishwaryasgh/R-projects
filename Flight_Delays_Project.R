# Flight Delays — Course-end Project 1
# Exploratory data analysis in R
#
# Required input:
#   flightdelays.xlsx (the uploaded dataset)
#   OR FlightDelays.csv (change the data-loading section accordingly)
#
# Install required packages once:
# install.packages(c("readxl", "dplyr", "ggplot2", "tidyr"))

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Read the dataset -------------------------------------------------------
# Keep the Excel file in the same folder as this R script.
df <- read_excel("flightdelays.xlsx")

# For a CSV file, use instead:
# df <- read.csv("FlightDelays.csv", stringsAsFactors = FALSE)

# Standardize column names to lowercase for convenient reference.
names(df) <- tolower(names(df))

# The dataset description may use flightnum while the file may use flightnumber.
if ("flightnumber" %in% names(df) && !("flightnum" %in% names(df))) {
  df <- df %>% rename(flightnum = flightnumber)
}

# 2. Understand the data ----------------------------------------------------
cat("Dataset dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
print(head(df))
str(df)
print(summary(df))

# 3. Missing values and duplicate rows --------------------------------------
missing_counts <- colSums(is.na(df))
print(missing_counts)
cat("Duplicate rows:", sum(duplicated(df)), "\n")

# 4. Clean and derive variables --------------------------------------------
# HHMM values are converted into hour-of-day values.
data <- df %>%
  mutate(
    delay = tolower(trimws(as.character(delay))),
    delay_status = case_when(
      delay == "ontime" ~ "On time",
      delay == "delayed" ~ "Delayed",
      TRUE ~ NA_character_
    ),
    sched_hour = as.integer(schedtime %/% 100),
    dep_hour = as.integer(deptime %/% 100),
    sched_minute_of_day = (schedtime %/% 100) * 60 + schedtime %% 100,
    dep_minute_of_day = (deptime %/% 100) * 60 + deptime %% 100,
    is_delayed = if_else(delay_status == "Delayed", 1L, 0L),
    weather_label = case_when(
      weather == 0 ~ "No weather flag",
      weather == 1 ~ "Weather flag",
      TRUE ~ "Unknown"
    ),
    departure_period = case_when(
      dep_hour >= 0 & dep_hour <= 5 ~ "Night (00–05)",
      dep_hour >= 6 & dep_hour <= 11 ~ "Morning (06–11)",
      dep_hour >= 12 & dep_hour <= 17 ~ "Afternoon (12–17)",
      dep_hour >= 18 & dep_hour <= 23 ~ "Evening (18–23)",
      TRUE ~ NA_character_
    )
  )

# 5. Descriptive statistics -------------------------------------------------
print(data %>% summarise(
  flights = n(),
  on_time = sum(delay_status == "On time", na.rm = TRUE),
  delayed = sum(delay_status == "Delayed", na.rm = TRUE),
  delay_rate_percent = mean(is_delayed, na.rm = TRUE) * 100,
  mean_distance = mean(distance, na.rm = TRUE),
  median_distance = median(distance, na.rm = TRUE)
))

# 6. Histograms / frequency charts -----------------------------------------
# Scheduled departure hour
ggplot(data, aes(x = sched_hour)) +
  geom_histogram(binwidth = 1, boundary = -0.5, color = "white", fill = "steelblue") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Flights by Scheduled Departure Hour",
       x = "Scheduled departure hour", y = "Number of flights") +
  theme_minimal()

# Carrier, destination, origin, weather, and dayweek are categorical;
# use bar charts (frequency distributions) rather than numeric histograms.
ggplot(data, aes(x = carrier)) +
  geom_bar(fill = "darkseagreen") +
  labs(title = "Flights by Carrier", x = "Carrier", y = "Number of flights") +
  theme_minimal()

ggplot(data, aes(x = dest)) +
  geom_bar(fill = "goldenrod") +
  labs(title = "Flights by Destination", x = "Destination", y = "Number of flights") +
  theme_minimal()

ggplot(data, aes(x = origin)) +
  geom_bar(fill = "mediumpurple") +
  labs(title = "Flights by Origin", x = "Origin", y = "Number of flights") +
  theme_minimal()

ggplot(data, aes(x = factor(weather))) +
  geom_bar(fill = "coral") +
  labs(title = "Weather Flag Counts", x = "Weather (0/1)", y = "Number of flights") +
  theme_minimal()

ggplot(data, aes(x = factor(dayweek))) +
  geom_bar(fill = "skyblue3") +
  labs(title = "Flights by Dayweek Code", x = "Dayweek code", y = "Number of flights") +
  theme_minimal()

# 7. Delay rate by weather, carrier, origin, destination -------------------
delay_rate_by_weather <- data %>%
  group_by(weather_label) %>%
  summarise(flights = n(),
            delayed = sum(delay_status == "Delayed", na.rm = TRUE),
            delay_rate_percent = mean(is_delayed, na.rm = TRUE) * 100,
            .groups = "drop")
print(delay_rate_by_weather)

ggplot(delay_rate_by_weather, aes(x = weather_label, y = delay_rate_percent)) +
  geom_col(fill = "tomato") +
  labs(title = "Delay Rate by Weather Flag", x = "Weather condition",
       y = "Delayed flights (%)") +
  theme_minimal()

delay_rate_by_carrier <- data %>%
  group_by(carrier) %>%
  summarise(flights = n(),
            delay_rate_percent = mean(is_delayed, na.rm = TRUE) * 100,
            .groups = "drop") %>%
  arrange(desc(delay_rate_percent))
print(delay_rate_by_carrier)

ggplot(delay_rate_by_carrier,
       aes(x = reorder(carrier, delay_rate_percent), y = delay_rate_percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Delay Rate by Carrier", x = "Carrier",
       y = "Delayed flights (%)") +
  theme_minimal()

delay_rate_by_origin <- data %>%
  group_by(origin) %>%
  summarise(flights = n(),
            delay_rate_percent = mean(is_delayed, na.rm = TRUE) * 100,
            .groups = "drop")
ggplot(delay_rate_by_origin,
       aes(x = reorder(origin, delay_rate_percent), y = delay_rate_percent)) +
  geom_col(fill = "darkseagreen") +
  coord_flip() +
  labs(title = "Delay Rate by Origin", x = "Origin",
       y = "Delayed flights (%)") +
  theme_minimal()

delay_rate_by_dest <- data %>%
  group_by(dest) %>%
  summarise(flights = n(),
            delay_rate_percent = mean(is_delayed, na.rm = TRUE) * 100,
            .groups = "drop")
ggplot(delay_rate_by_dest,
       aes(x = reorder(dest, delay_rate_percent), y = delay_rate_percent)) +
  geom_col(fill = "goldenrod") +
  coord_flip() +
  labs(title = "Delay Rate by Destination", x = "Destination",
       y = "Delayed flights (%)") +
  theme_minimal()

# 8. Scatter plots: delay status against scheduled hour and distance --------
# Jitter helps separate overlapping binary outcomes.
ggplot(data, aes(x = sched_hour, y = is_delayed,
                 color = delay_status)) +
  geom_jitter(height = 0.04, width = 0.12, alpha = 0.55) +
  scale_y_continuous(breaks = c(0, 1), labels = c("On time", "Delayed")) +
  labs(title = "Delay Status by Scheduled Departure Hour",
       x = "Scheduled departure hour", y = "Delay status") +
  theme_minimal()

ggplot(data, aes(x = distance, y = is_delayed,
                 color = delay_status)) +
  geom_jitter(height = 0.04, alpha = 0.55) +
  scale_y_continuous(breaks = c(0, 1), labels = c("On time", "Delayed")) +
  labs(title = "Delay Status by Flight Distance",
       x = "Distance", y = "Delay status") +
  theme_minimal()

# 9. Box plot: actual departure hour by day of month for delayed flights ----
delayed_flights <- data %>% filter(delay_status == "Delayed")
ggplot(delayed_flights, aes(x = factor(daymonth), y = dep_hour)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Actual Departure Hour for Delayed Flights by Day of Month",
       x = "Day of month", y = "Actual departure hour") +
  theme_minimal()

# 10. Categorical table for departure period --------------------------------
departure_table <- table(data$departure_period, data$delay_status, useNA = "ifany")
print(departure_table)
print(prop.table(departure_table, margin = 1))

# 11. Summary of major variables --------------------------------------------
print(summary(data %>% select(schedtime, deptime, distance, weather,
                              daymonth, is_delayed)))
print(data %>%
        group_by(delay_status) %>%
        summarise(mean_distance = mean(distance, na.rm = TRUE),
                  mean_sched_hour = mean(sched_hour, na.rm = TRUE),
                  mean_dep_hour = mean(dep_hour, na.rm = TRUE),
                  flights = n(),
                  .groups = "drop"))

# 12. Histograms of major variables -----------------------------------------
ggplot(data, aes(x = distance)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Flight Distance",
       x = "Distance", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = sched_hour)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "darkseagreen",
                 color = "white") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Distribution of Scheduled Departure Hour",
       x = "Scheduled hour", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = dep_hour)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "goldenrod",
                 color = "white") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Distribution of Actual Departure Hour",
       x = "Actual departure hour", y = "Frequency") +
  theme_minimal()

# 13. Pie chart: delayed versus on-time flights -----------------------------
delay_counts <- data %>%
  count(delay_status) %>%
  mutate(percent = n / sum(n) * 100,
         label = paste0(delay_status, "\n", n, " (", round(percent, 1), "%)"))

ggplot(delay_counts, aes(x = "", y = n, fill = delay_status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Share of On-time and Delayed Flights", x = NULL, y = NULL) +
  theme_void()

# 14. Final summary ---------------------------------------------------------
cat("\nOverall delay rate:",
    round(mean(data$is_delayed, na.rm = TRUE) * 100, 2), "%\n")
cat("Weather delay rates:\n")
print(delay_rate_by_weather)

# Interpretation reminder:
# The delay field is binary (ontime/delayed), not delay duration in minutes.
# Do not calculate delay duration by subtracting HHMM values directly.
# The provided dayweek description should be verified because the data may
# contain values other than the documented 0/1.

