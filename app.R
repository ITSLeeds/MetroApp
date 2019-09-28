library(shiny)
library(leaflet)

#ui <- fluidPage(
#  h1("Metropoly game")
#  )

# minimum data needed for game --------------------------------------------

#added two groups to each team
team_names = c("Haruko","Andrew","Robin") #1:3 
#Group numeric 
groups = 1:3
#Group list
group_list <- c("A","B","Total")
rep_teams = replicate(length(groups),team_names)
rep_teams = sort(rep_teams)
team_columns_names = paste0("team_",rep_teams,"_",group_list)

#Read stitaion scoresheet
Stationlist =read.csv("Metro_station_score.csv")
colnames(Stationlist) <- c("Station","Points")
#Create staiton name list
StationName = Stationlist
StationName$Points <- NULL
#Creat station score list
#StationScore = Stationlist
#StationScore$Station <-NULL

#Read destination scoresheet
destinations = read.csv("Dest_Score.csv")
colnames(destinations)<-c("Destination","Score")
#Create desination names list 
DestName = destinations
DestName$Score <-NULL 
#Create "bus only" destination 
BusOnlyDest <- read.csv("BusOnlyDest.csv")
BusOnlyDest$Score <- NULL

#Create destination score list 
#DestScore = destinations 
#DestScore$Destination <-NULL

#Read Bonus scoresheet
bonus = read.csv("Bonus.csv")
colnames(bonus)<- c("Bonus","Score")
#Create Bonus name list 
BonusName = bonus [1:3,]
BonusName$Score <-NULL
#Create Bonus score list 
#BonusScore = bonus 
#BonusScore$Bonus <- NULL

# Idea: select team at the beginning
# stations = readr::read_csv("stations.csv") # todo: create stations .csv with name and number points
# robin to geocode them and put on map
stations = sf::read_sf("stations.geojson")
station_vector = rep(0, nrow(stations))
#create intial socre sheet for bonus point and destinations
dest_vector = rep (0, nrow(DestName))
bonus_vector = rep(0,nrow(bonus))

repl_station_vecotr = replicate(length(team_columns_names), station_vector)
repl_dest_vector = replicate(length(team_columns_names),dest_vector)
repl_bonus_vector = replicate(length(team_columns_names),bonus_vector)

# test scoring: group 2 has scored in station 1, 3 has scored in 1 and 2
#team_initial_scores[1, 2] = 1
#team_initial_scores[1:2, 3] = 1

#Create score sheet for station
scoresheet_station = cbind(sf::st_drop_geometry(stations), repl_station_vecotr)
names(scoresheet_station)[4:ncol(scoresheet_station)] = team_columns_names
#Create score sheet for destination 
scoresheet_dest = cbind(destinations,repl_dest_vector)
names(scoresheet_dest)[3:ncol(scoresheet_dest)] =team_columns_names
#Create socre sheet for bonus 
scoresheet_bonus = cbind(bonus,repl_bonus_vector) 
names(scoresheet_bonus)[3:ncol(scoresheet_bonus)] =team_columns_names
VBO_row = which (scoresheet_bonus == "Visit Bus only")
scoresheet_bonus[VBO_row,paste0("team_",team_names,"_Total")] <- -2000

#Create score summary 
scoresum_vector <- rep(0,length(team_names))
scoresum = replicate(5,scoresum_vector)
colnames(scoresum) = c("Team name","Station","Destination","Bonus","Total")
scoresum[,1] = team_names
scoresum[,"Bonus"] <- -2000

#Create a ranking summary
scorerank_vector <- rep(0,length(team_names))
scorerank = replicate(2,scorerank_vector)
colnames(scorerank) = c("Rank","Team name")
scorerank[,1]=1:length(team_names)
scorerank[,2]=team_names



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
        inputId = "PointScore",
        label = "Select the visited station,destination or bonus type",
        choices = list("Station" = StationName,
                       "Destination" = DestName,
                       "Bonus" = BonusName),
        selected = StationName[1]
      ),
      actionButton("log","Submit")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Your team score",
          h2("Your are"),
          h4(shiny::textOutput("shiny_teamname")),
          h4(shiny::textOutput("shiny_group")),
          
          h2("Your score summary"),
          #Summary talbe for each cateogry 
          shiny::textOutput("shiny_loggedScore"),
          shiny::tableOutput("shiny_scoresheet_st"),
          shiny::tableOutput("shiny_scoresheet_dest"),
          shiny::tableOutput("shiny_scoresheet_bonus")
        ),
        tabPanel(
          title = "Leader board",
          h3("Current ranking "),
          shiny::tableOutput("shiny_scorerank"),
          h3("Current score summary"),
          shiny::tableOutput("shiny_scoresum")
        ),
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
  output$shiny_teamname = renderText(paste("Team:",input$teamname))
  output$shiny_group = renderText(paste("Group",group_list[as.numeric(input$Group)]))
  output$shiny_scoresum = renderTable(scoresum)
  output$shiny_scorerank = renderTable(scorerank)
  
  #update the data set table
  observeEvent(input$log,{
    log_col <- paste0("team_",input$teamname,"_",group_list[as.numeric(input$Group)])
    LogScore <-input$PointScore
    print(LogScore)
    #Log STATION score
    if (any(scoresheet_station == LogScore) == TRUE)
    {#find the row contains LogScore
      log_row = which(scoresheet_station == LogScore)
      #Update score table 
      scoresheet_station[log_row,log_col]<<- 1
      output$shiny_scoresheet_st = renderTable(scoresheet_station[c(names(scoresheet_station)[1:2], paste0("team_",input$teamname,"_",group_list))])
      #Calcurate the score 
      scoresheet_station[log_row,paste0("team_",input$teamname,"_Total")]<<-scoresheet_station[log_row,"Points"]
      #Update the total score table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Station"]<<-sum(scoresheet_station[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum)
    }
    #Log Destination socre
    else if  (any(scoresheet_dest == LogScore) == TRUE)
    {#find the row contains LogSocre
      log_row = which(scoresheet_dest == LogScore)
      #Update scoresheet 
      scoresheet_dest[log_row,log_col]<<- 1
      output$shiny_scoresheet_dest = renderTable(scoresheet_dest[c(names(scoresheet_dest)[1:2], paste0("team_",input$teamname,"_",group_list))])
     
      #Calculate the score 
      scoresheet_dest[log_row,paste0("team_",input$teamname,"_Total")]<<-scoresheet_dest[log_row,"Score"]      
      #Update the total socre table
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Destination"]<<-sum(scoresheet_dest[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum)
      
      #Check whether team visited bus only stop 
      if (any(BusOnlyDest == LogScore))
      {scoresheet_bonus[VBO_row,log_col] <<-1
        #Calculate the bonus 
       scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_Total")] <<-
         scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_A")]*1000 +scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_B")]*1000-2000
      output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))])
      #Update the total socre table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Bonus"]<<-sum(scoresheet_bonus[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum)
      }
      }
    #Log Bonus Score
    else if (any(scoresheet_bonus == LogScore) == TRUE)
    {#find the row contains LogScore
      log_row = which(scoresheet_bonus == LogScore)
      #Update socresheet  
      scoresheet_bonus[log_row,log_col]<<- 1
      output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))])
      
      #Calcurate Bonus score 
      scoresheet_bonus[log_row,paste0("team_",input$teamname,"_Total")]<<-scoresheet_bonus[log_row,"Score"]      
      #Update the total socre table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Bonus"]<<-sum(scoresheet_bonus[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum)
    }
    #Update a total score for each group 
    nrow_sumscore<- which (scoresum  == input$teamname) 
    scoresum[nrow_sumscore,"Total"]<<-sum(as.numeric(scoresum[nrow_sumscore,2:4]))
    #update current ranking based on the total score
    output$shiny_scorerank = renderTable(scorerank)
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
