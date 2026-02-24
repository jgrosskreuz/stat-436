library(tidyverse)
library(shiny)

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')


STATS <- c("hp", "attack", "defense", "speed", "special_attack", "special_defense")

STAT_LABELS <- c(hp = "HP",attack = "Attack", defense = "Defense", speed = "Speed", 
                 special_attack  = "Special Attack", special_defense = "Special Defense")

make_stat_plot <- function(pokemon_df, pokemon_name, stat) {
  
  poke_row <- pokemon_df %>%
    filter(pokemon == pokemon_name)
  
  poke_value <- poke_row[[stat]]
  poke_type  <- poke_row$type_1
  poke_color <- poke_row$color_1
  
  overall_median <- median(pokemon_df[[stat]], na.rm = TRUE)
  
  type_median <- pokemon_df %>%
    filter(type_1 == poke_type) %>%
    pull(stat) %>%
    median(na.rm = TRUE)
  
  group_levels <- c(
    pokemon_name,
    "All Pokemon",
    paste(poke_type, "type")
  )
  
  ggplot(
    data = data.frame(
      group = factor(group_levels, levels = group_levels),
      value = c(poke_value, overall_median, type_median),
      fill  = c(poke_color, "steelblue", poke_color),
      alpha = c(1, 0.5, 0.5)
    ),
    aes(x = group, y = value, fill = fill, alpha = alpha)
  ) +
    geom_col(show.legend = FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    labs(
      x = NULL,
      y = STAT_LABELS[[stat]],
      title = paste("Pokemon:", pokemon_name)
    )
}

ui <- fluidPage(
  titlePanel("Pokemon Stat Analyzer"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        "pokemon_name",
        "Which Pokemon would you like to analyze?"
      ),
      selectInput(
        "selected_stat",
        "Which battle stat would you like to see?",
        choices = setNames(STATS, STAT_LABELS)
      ),
      p("This visualization tool allows you to quickly analyze a Pokemon's battle 
        statistics relative to other Pokemon. Enter the name of any Pokemon from 
        Generation VII or before (not case sensitive), and select which statistic you'd like to compare, and
        a bar chart will be generated for comparison. The leftmost bar will represent the
        chosen Pokemon; the middle one will represent the median over all Pokemon in that
        particular statistic; the rightmost bar will represent the median over all Pokemon
        of the same primary type as the chosen Pokemon.")
    ),
    mainPanel(
      plotOutput("stat_plot")
    )
  )
)


server <- function(input, output) {
  pokemon_name <- reactive({
    name <- tolower(trimws(input$pokemon_name))
    req(name %in% pokemon_df$pokemon)
    name
  })
  
  output$stat_plot <- renderPlot({
    make_stat_plot(pokemon_df, pokemon_name(), input$selected_stat)
  })
}

shinyApp(ui, server)