library(shiny)
library(leaflet)

#ui <- fluidPage(
#  h1("Metropoly game")
#  )

# minimum data needed for game --------------------------------------------

#added two groups to each team
team_names = 1:3
groups = 1:2
rep_teams = replicate(length(groups),team_names)
rep_teams = sort(rep_teams)
team_columns_names = paste0("team_",rep_teams,"_group_",groups)

#Group list
group_list <- c("Group A","Group B")

#Station names list
StationName =read.csv("Metro_station_score.csv")
StationName$Points <- NULL

DestName = read.csv("Dest_Score.csv")
DestName$Score <-NULL 

BonusName = read.csv("Bonus.csv")
BonusName$Score <-NULL

# Idea: select team at the beginning
# stations = readr::read_csv("stations.csv") # todo: create stations .csv with name and number points
# robin to geocode them and put on map
stations = sf::read_sf("stations.geojson")
initial_scores = rep(0, nrow(stations))

# test scoring: group 2 has scored in station 1, 3 has scored in 1 and 2
team_initial_scores = replicate(length(team_columns_names), initial_scores)
team_initial_scores[1, 2] = 1
team_initial_scores[1:2, 3] = 1
scoresheet = cbind(sf::st_drop_geometry(stations), team_initial_scores)
names(scoresheet)[4:ncol(scoresheet)] = team_columns_names

ui <- fluidPage(
  headerPanel(tags$img(src = "Metropoly_Logo.png")),
  sidebarLayout(
    sidebarPanel( 
      h3("Select your team"),
    selectInput(
    inputId = "teamname",
    label = "Team name",
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
  h3("Log your score"),
  selectInput(
    inputId = "VisitStation",
    label = "Select the STATION you visited",
    choices = StationName,
    selected = StationName[1]
  ),
  selectInput(
    inputId ="VisitDest",
    label = "Select the DESTINATION you visited",
    choice = DestName,
    selected = DestName[1]
  ),
  selectInput(
    inputId = "ScoredBonous",
    label = "Select a Bonous type",
    choices = BonusName,
    selected = BonusName[1]
  ),

  submitButton(text = "Submit")),
  
  mainPanel(
  tabsetPanel(
  tabPanel(
    title = "Game rules",
    h1("How to play Metropoly game "),
    h2("The objective of the game"),
    p(
      "The object of the game is to get the highest points score possible. Teams will score points by either travelling on trains through stations, or by visiting named destinations across West Yorkshire"
    ),
    h2("Rules"),
    tags$li(
      "Teams may only use three travel modes: train, bus and walking. Cycling is not allowed in this game, nor is using a private car or taxi."
    ),
    tags$li(
      "Teams may split into a maximum of two groups when travelling around the area."
    ),
    tags$li(
      "All points scored will be awarded to the team as a whole and not to the individual group which scored them."
    ),
    tags$li(
      "To score points for a named destination from the destination list the group must visit it and take a photograph as evidence. Please see the destination list for the specifics of what needs to be captured in the photograph. Each photograph MUST also include in it at least one member of the group."
    ),
    tags$li(
      "To score points for a station the group must be on a train that departs from the station, arrives at the station, or stops at the station. A log of all trains used by the group must be submitted as evidence that they have visited the stations they have travelled through."
    ),
    tags$li(
      "Points may only be scored once for each destination and station that is visited. This means that if both groups within a team visit the same destination/station but at different times of the day, the points for the visit by the second group won’t count."
    ),
    tags$li(
      "Each group within a team must visit at least one destination that is accessible only by bus; these are highlighted in green on your scorecard and destination list sheets. If a group does not visit such a destination then they will receive a penalty of -1000 points"
    ),
    tags$li(
      "Grand Central operates an express service (each way) between Bradford Interchange and London. This service represents a quick way to get from one side of the Metro card area to the other and any team that uses it will receive +500 bonus points (but remember to get off at Pontefract Monkhill)"
    ),
    tags$li(
      "Teams may get a bonus of +1,000 points if they are lucky enough to spot the two roving representatives of ITS (Anna and Haruko) who will be travelling about the area whilst the game is operating. Photographic evidence of the team with the representatives is required."
    ),
    tags$li(
      "Teams may get a bonus of +1,000 points if they are lucky enough to spot and photograph the station cat, Felix, at Huddersfield Station. Felix is famous and has his own Facebook page https://www.facebook.com/Felix-the-Huddersfield-Station-Cat-107081156301541/"
    ),
    tags$li(
      "The starting point for the game will be the garden in front of the ITS building. Teams may not depart before the official starting time of 9:15."
    ),
    tags$li(
      "Teams should return to ITS by 16:00. The finishing point for the game will be the Entrance Doors to the ITS building. As each team returns it will be officially clocked in and their finishing time recorded. An additional 5 points per minute will be scored for teams that arrive back before 16:00, but 100 points per minute will be deducted for teams which arrive back after 16:00."
    ),
    tags$li(
      "Teams will only count as having returned when both groups making up the team are back at ITS.
      "
    ),
    h2("Twitter update"),
    p(
      "Annoy and intimidate your rival groups by tweeting pictures of each point scoring destination you arrive at to the hashtag #ITSmetropoly.
      We’ll be looking at the twitter feed during the day and highlighting/promoting any particularly funny/good comments and photos.
      "
    ),
    h1("Advices"),
    tags$li("Plan your route in advance. Although finding out about trains whilst on the move is relatively easy, it is much harder to determine which bus to take and even whether a bus service to a specific destination is available."),
    
    h1("Resources")
    ),
  tabPanel(
    h1("Your current score"),
    title = "Team scoring tab",
    shiny::textOutput("shiny_teamname"),
    shiny::textOutput("shiny_group"),
    shiny::tableOutput("shiny_scoresheet")
  ),
  tabPanel(title = "Leader board"),
  tabPanel(
    title = "Maps",
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "New points")
  )
  
    ))))

server <- function(input, output) {
  # If we get the scores from Google Sheets:
  # scoresheet = function_to_read_from_google_sheets()
  output$shiny_teamname = renderText(paste("Team Name:",input$teamname))
  output$shiny_group = renderText(group_list[as.numeric(input$Group)])
  output$shiny_scoresheet = renderTable(scoresheet[c(names(scoresheet)[1:2], paste0("team_",input$teamname,"_group_",groups))])
  
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
