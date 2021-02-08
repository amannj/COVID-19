######################################
#                                    #
#     COVID-19 Dashboard functions   #
#                                    #
######################################


## Download CSSE data  -----------
store_local <- function(filename) {
  link_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  date_today <- substr(Sys.time(), 1, 10)
  url <- file.path(link_url, filename)
  dest <- file.path("./data/CSSE", filename)
  download.file(url, dest)
}



## Tidy up data -----------
tidy_data <- function(df, series = "value") {
  df %>%
    select(-Province.State, -Lat, -Long) %>%
    pivot_longer(-Country.Region, names_to = "date", values_to = series) %>%
    mutate(
      date = substr(date, 2, nchar(date)),
      date = mdy(date)
    ) %>%
    group_by(Country.Region, date) %>%
    summarize(!!sym(series) := sum(!!sym(series), na.rm = TRUE)) %>%
    select(country = Country.Region, date, series)
}


# Extract top_X affected  --------------
extract_topX <- function(df, name = "value", X = 10) {
  df %>%
    mutate(series = name) %>%
    select(country, series, cases = name) %>%
    arrange(-cases) %>%
    slice(1:X) %>%
    mutate(
      share = round(cases / sum(cases) * 100, 0),
      cases = round(cases / 100000, 0)
    ) %>%
    rename(
      `cases (100k)` = cases,
      `share (%)` = share
    )
}


# Plot cummulative   --------------
plot_TS <- function(df, country_select = NULL,
                    start_date = NULL,
                    end_date = NULL,
                    transform_series = NULL,
                    scale_series = NULL,
                    date_today = date_today,
                    is.ts = FALSE) {

  ## Initialise figure title
  title_cases <- "total cases"
  title_transformation <- "linear scale."
  title <- paste0("COVID-19 ", title_cases, " over time, ", title_transformation)

  ## Per capita transformation
  if (scale_series() == "pc") {
    df %>%
      mutate_at(.vars = c("confirmed", "dead", "recovered", "remaining"), ~ . / pop * 1000000) -> df

    title_cases <- "per thousand inhabitants"
  }

  ## Reshape and order series
  df %>%
    filter(country %in% country_select()) %>%
    filter(date >= start_date() & date <= end_date()) %>%
    pivot_longer(cols = "confirmed":"remaining") %>%
    mutate(
      value = ifelse(value == 0, NA, value),
      value = round(value, 0)
    ) %>%
    na.omit() %>%
    rename(value = value) -> plot_data



  if (transform_series() == "log") {
    plot_data %>%
      mutate(value = log(value)) -> plot_data

    title_transformation <- "logarithmic scale."
  }

  ## Update plot title
  title <- paste0("COVID-19 ", title_cases, " over time, ", title_transformation)


  ## Build plot
  if (is.ts == FALSE) {
    ggplot(
      plot_data,
      aes(x = date, y = value, fill = country)
    ) +
      geom_area(col = "white", size = .1, alpha = .85) +
      facet_wrap(~name, scales = "free_y", ncol = 1) +
      labs(
        title = title,
        x = "", y = "", fill = "",
        caption = paste0("Date recorded: ", date_today, ".")
      ) +
      theme(legend.title = element_blank()) +
      theme_minimal() +
      xlim(as.Date(c(
        min(plot_data$date, na.rm = TRUE),
        (max(plot_data$date, na.rm = TRUE) + 0)
      ))) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_date(date_labels = "%e-%m") +
      scale_y_continuous(label = comma) +
      scale_fill_viridis_d() -> p
  }

  ## Build plot
  if (is.ts == TRUE) {
    ggplot(
      plot_data,
      aes(x = date, y = value, col = country)
    ) +
      geom_line() +
      facet_wrap(~name, scales = "free_y", ncol = 1) +
      labs(
        title = title,
        x = "", y = "", fill = "",
        caption = paste0("Date recorded: ", date_today, ".")
      ) +
      theme(legend.title = element_blank()) +
      theme_minimal() +
      xlim(as.Date(c(
        min(plot_data$date, na.rm = TRUE),
        (max(plot_data$date, na.rm = TRUE) + 0)
      ))) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_date(date_labels = "%e-%m") +
      scale_y_continuous(label = comma) +
      scale_color_viridis_d() -> p
  }

  return(p)
}
