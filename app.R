library(shiny)
library(leaflet)

#ui <- fluidPage(
#  h1("Metropoly game")
#  )


# minimum data needed for game --------------------------------------------

team_names = 1:3

# todo: add 2 groups per team
team_columns_names = paste0("team_", team_names)

stations = tibble::tibble(station = c("Apperly Bridge", "S2"),
                          points = c(200, 200))

initial_scores = rep(0, nrow(scoresheet))
# test scoring: group 2 has scored in station 1, 3 has scored in 1 and 2
team_initial_scores = replicate(length(team_names), initial_scores)
team_initial_scores[1, 2] = 1
team_initial_scores[1:2, 3] = 1
scoresheet = cbind(stations, team_initial_scores)
names(scoresheet)[3:ncol(scoresheet)] = team_columns_names

ui <- navbarPage(
  title = "Metropoly game",
  tabPanel(title = "About",
           h1("What is Metropoly game?"),
           p("ITS")),
  tabPanel(title = "Game rules",
           h1("How to play Metropoly game "),
           p("ITS")),
  tabPanel(
    h1("Teams: log your scores here!"),
    title = "Team scoring tab",
    selectInput(
      inputId = "teamname",
      label = "Select your team name to see your score",
      choices = team_names,
      selected = team_names[1]
    ),
    
    radioButtons(
      inputId = "Group",
      label = "Your group",
      choices = list("Group A" = 1,
                     "Group B" = 2),
      selected = 1
    ),
    shiny::tableOutput('scoresheet')
  ),
  tabPanel(title = "Leader board"),
  tabPanel(
    title = "Maps",
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "New points")
  )
  
)

server <- function(input, output) {
  # If we get the scores from Google Sheets:
  # scoresheet = function_to_read_from_google_sheets()
  output$scoresheet = renderTable(scoresheet[c(names(scoresheet)[1:2], paste0("team_", input$teamname))])
  
  # This is the map
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2, rnorm(40) + 6)
  }, ignoreNULL = FALSE)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui = ui, server = server)