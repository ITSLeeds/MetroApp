library(shiny)
library(leaflet)

#ui <- fluidPage(
#  h1("Metropoly game")
#  )

# minimum data needed for game --------------------------------------------

# todo: add 2 groups per team
team_names = 1:3
team_columns_names = paste0("team_", team_names)

# Idea: select team at the beginning
# stations = readr::read_csv("stations.csv") # todo: create stations .csv with name and number points
# robin to geocode them and put on map
stations = sf::read_sf("stations.geojson")

initial_scores = rep(0, nrow(stations))

# test scoring: group 2 has scored in station 1, 3 has scored in 1 and 2
team_initial_scores = replicate(length(team_names), initial_scores)
team_initial_scores[1, 2] = 1
team_initial_scores[1:2, 3] = 1
scoresheet = cbind(sf::st_drop_geometry(stations), team_initial_scores)
names(scoresheet)[4:ncol(scoresheet)] = team_columns_names

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
    shiny::tableOutput("shiny_scoresheet")
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
  output$shiny_scoresheet = renderTable(scoresheet[c(names(scoresheet)[1:2], paste0("team_", input$teamname))])
  
  # This is the map
  points <- eventReactive(input$recalc, {
    stations
  }, ignoreNULL = FALSE)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui = ui, server = server)
