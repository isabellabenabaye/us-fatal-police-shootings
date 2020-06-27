library(tidyverse)
library(lubridate)
library(reactable)

fatal_police_shootings <- read_csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")

# States table -----
# state counts pre-pivot - used for cell shading
state_counts <- fatal_police_shootings %>%
  count(state, race, sort = TRUE)

# state name data - get full state names
states <- bind_cols(datasets::state.name, datasets::state.abb) %>%
  rename(state_name = "...1",
         state = "...2")

# main table
state_table <- fatal_police_shootings %>%
  left_join(states) %>%
  count(state_name, race) %>%
  ## make a column for each race
  pivot_wider(id_cols = state_name, names_from = race, values_from = n, values_fill = 0) %>%
  rename(unknown = `NA`) %>%
  mutate(total = A + B + N + W + unknown + H + O) %>%
  select(state_name, total, W, B, H, A, N, O, unknown)

# palette
red_pal <- function(x) rgb(colorRamp(c("#ffeef1", "#f87274"), bias = 2)(x), maxColorValue = 255)

# race column
race_column <- function(col, class = NULL, ...){
  colDef(
    ...,
    width = 100,
    defaultSortOrder = "desc",
    style = function(value) {
      normalized <- (value - min(state_counts[-1:-2,"n"])) / (max(state_counts[-1:-2,"n"]) - min(state_counts[-1:-2,"n"]))
      if (value < 1) {
        list(color = "#aaa")
      } else if (normalized > 0.99) {
        list(background = "#f87274")
      } else {
        list(background = red_pal(normalized))
      }
    }
  )
}

# grouped columns
race_cols <- c("W", "B", "H", "A", "N", "O", "unknown")

# bar chart
library(htmltools)
bar_chart <- function(label, width = "100%", height = "18px", fill = "#118AB2", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# reactable table
reactable(
  data = state_table,
  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
  defaultSorted = "total",
  defaultPageSize = 25,
  highlight = TRUE,
  searchable = TRUE,
  compact = TRUE,
  columnGroups = list(
    colGroup(name = "Race", columns = race_cols)
  ),
  columns = list(
    state_name = colDef(name = "State",
                        minWidth = 120),
    total = colDef(
      name = "Number of killings",
      defaultSortOrder = "desc",
      minWidth = 200,
      cell = function(value) {
        width <- paste0(value * 100 / max(state_table$total), "%")
        value <- format(value, big.mark = ",")
        # Fix each label using the width of the widest number (incl. thousands separators)
        value <- format(value, width = 4, justify = "right")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      # And left-align the columns
      align = "left",
      style = list(fontFamily = "monospace", whiteSpace = "pre")
    ),
    W = race_column(name = "White", col = "W"),
    B = race_column(name = "Black", col = "B"),
    H = race_column(name = "Hispanic", col = "H"),
    A = race_column(name = "Asian", col = "A"),
    N = race_column(name = "Native American", col = "N"),
    O = race_column(name = "Other", col = "O"),
    unknown = race_column(name = "Unknown", col = "unknown")
  )
)


# Cities table -----
# city counts pre-pivot - used for cell shading
city_counts <- fatal_police_shootings %>%
  mutate(city = paste0(city,", ",state)) %>%
  count(city, race, sort = TRUE)

# main table
city_table <- fatal_police_shootings %>%
  mutate(city = paste0(city,", ",state)) %>%
  count(city, race) %>%
  ## make a column for each race
  pivot_wider(id_cols = city, names_from = race, values_from = n, values_fill = 0) %>%
  rename(unknown = `NA`) %>%
  mutate(total = A + B + N + W + unknown + H + O) %>%
  select(city, total, W, B, H, A, N, O, unknown) %>%
  filter(total >= 10)

# race column
race_column <- function(col, class = NULL, ...){
  colDef(
    ...,
    maxWidth = 80,
    defaultSortOrder = "desc",
    headerStyle = list(fontSize = "13px", fontWeight = 400),
    style = function(value) {
      normalized <- (value - min(city_counts[-1:-2,"n"])) / (max(city_counts[-1:-2,"n"]) - min(city_counts[-1:-2,"n"]))
      if (value < 1) {
        list(color = "#aaa")
      } else if (normalized > 0.99) {
        list(background = "#f87274")
      } else {
        list(background = red_pal(normalized))
      }
    }
  )
}

# reactable table
final_table_cities <- reactable(
  data = city_table,
  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", background = "#F7F7F7"),
  defaultSorted = "total",
  defaultPageSize = 15,
  defaultColDef = colDef(class = "cell", headerClass = "header"),
  highlight = TRUE,
  searchable = TRUE,
  compact = TRUE,
  columnGroups = list(
    colGroup(name = "Race", columns = race_cols)
  ),
  columns = list(
    city = colDef(name = "State",
                  minWidth = 120,
                  style = list(fontWeight = 500)),
    total = colDef(
      name = "Number of killings",
      defaultSortOrder = "desc",
      minWidth = 160,
      cell = function(value) {
        width <- paste0(value * 100 / max(city_table$total), "%")
        value <- format(value, big.mark = ",")
        # Fix each label using the width of the widest number (incl. thousands separators)
        value <- format(value, width = 4, justify = "right")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      # And left-align the columns
      align = "left",
      style = list(fontFamily = "DM Mono", whiteSpace = "pre")
    ),
    W = race_column(name = "White", col = "W"),
    B = race_column(name = "Black", col = "B"),
    H = race_column(name = "Hispanic", col = "H"),
    A = race_column(name = "Asian", col = "A"),
    N = race_column(name = "Native American", col = "N"),
    O = race_column(name = "Other", col = "O"),
    unknown = race_column(name = "Unknown", col = "unknown")
  )
)

final_table_cities
