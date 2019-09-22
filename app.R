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
      
      submitButton(text = "Submit")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          h1("Your current score"),
          title = "Team scoring tab",
          shiny::textOutput("shiny_teamname"),
          shiny::textOutput("shiny_group"),
          
          h3("Log your score")
          #selectInput(
           # inputId = "PointSource",
            #label = "Select the STATION you visited",
            #choices = StationName,
            #choices = list("Station" = StationName,
             #              "Destination" = DestName,
              #             "Bonus" = BonusName),
            #selected = StationName[1]
          #),
          
          #submitButton(text = "Submit"),
          #shiny::tableOutput("shiny_scoresheet")
        ),
        tabPanel(title = "Leader board"),
        tabPanel(
          title = "Maps",
          leafletOutput("mymap"),
          p(),
          actionButton("recalc", "New points")
        )
        
      ))))

server <- function(input, output, session) {
  # If we get the scores from Google Sheets:
  # scoresheet = function_to_read_from_google_sheets()
  output$shiny_teamname = renderText(paste("Team Name:",input$teamname))
  output$shiny_group = renderText(group_list[as.numeric(input$Group)])
  output$shiny_scoresheet = renderTable(scoresheet[c(names(scoresheet)[1:2], paste0("team_",input$teamname,"_group_",groups))])
  
  #Update the contents of slide bar based on the score type 
  observe({
    ScoreType <- input$Logtype
    print(input$Logtype)
    if (ScoreType == "Station") 
    {updateSelectInput(session,"PointSource",
                       label = "Select the STATION you visited",
                       choices = StationName,
                       selected = StationName[1]
    )}
    else if (ScoreType == "Destination")
    {updateSelectInput(session,"PointSource",
                       label = "Select the DESTINATION you visited",
                       choices = DestName,
                       selected = DestName[1])}
    
    else if (ScoreType == "Bonus Point")
    {updateSelectInput(session,"PointSource",
                       label = "Select the Bonus type",
                       choices = BonusName,
                       selected = BonusName[1])}
    
  })
  
  
  
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
