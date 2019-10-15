library(shiny)
library(leaflet)

#ui <- fluidPage(
#  h1("Metropoly game")
#  )

# minimum data needed for game --------------------------------------------

#added two groups to each team
team_names = c("Haruko","Andrew","Robin") #1:16 
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

#Read destination scoresheet
destinations = read.csv("Dest_Score.csv")
colnames(destinations)<-c("Destination","Score")
#Create desination names list 
DestName = destinations
DestName$Score <-NULL 
#Create "bus only" destination 
BusOnlyDest <- read.csv("BusOnlyDest.csv")
BusOnlyDest$Score <- NULL

#Read Bonus scoresheet
bonus = read.csv("Bonus.csv")
colnames(bonus)<- c("Bonus","Score")
#Create Bonus name list 
BonusName = bonus [1:4,]
BonusName$Score <-NULL


#Create/load station scoresheet
if(file.exists("scoresheet_station_updated.csv")){
  scoresheet_station <- readr::read_csv("scoresheet_station_updated.csv")
}else{
# Idea: select team at the beginning
# stations = readr::read_csv("stations.csv") # todo: create stations .csv with name and number points
# robin to geocode them and put on map
stations = sf::read_sf("stations.geojson")
#Create score sheet for station
station_vector = rep(0, nrow(stations))
repl_station_vecotr = replicate(length(team_columns_names), station_vector)
scoresheet_station = cbind(sf::st_drop_geometry(stations), repl_station_vecotr)
names(scoresheet_station)[4:ncol(scoresheet_station)] = team_columns_names
}

#Create/load station scoresheet
if(file.exists("scoresheet_dest_updated.csv")){
  scoresheet_dest <- readr::read_csv("scoresheet_dest_updated.csv")
}else{
#create intial socre sheet for  destinations
dest_vector = rep (0, nrow(DestName))
repl_dest_vector = replicate(length(team_columns_names),dest_vector)
#Create score sheet for destination 
scoresheet_dest = cbind(destinations,repl_dest_vector)
names(scoresheet_dest)[3:ncol(scoresheet_dest)] =team_columns_names
}

#Create/load Bonus scoresheet
if(file.exists("scoresheet_bonus_updated.csv")){
  scoresheet_bonus <- readr::read_csv("scoresheet_bonus_updated.csv")
  VBO_row = which (scoresheet_bonus == "Visit Bus only")
}else{
#Create score sheet for bonus point
bonus_vector = rep(0,nrow(bonus))
repl_bonus_vector = replicate(length(team_columns_names),bonus_vector)
#Create socre sheet for bonus 
scoresheet_bonus = cbind(bonus,repl_bonus_vector) 
names(scoresheet_bonus)[3:ncol(scoresheet_bonus)] =team_columns_names
VBO_row = which (scoresheet_bonus == "Visit Bus only")
scoresheet_bonus[VBO_row,paste0("team_",team_names,"_Total")] <- -2000
}


#Create/load score summary 
if(file.exists("scoresum_updated.csv")){
  scoresum <- readr::read_csv("scoresum_updated.csv")
}else{
  scoresum_vector <- rep(0,length(team_names))
  repl_scoresum_Vector= replicate(4,scoresum_vector)
  scoresum <- data.frame(team_names,repl_scoresum_Vector)
  colnames(scoresum) = c("Team name","Station","Destination","Bonus","Total")
  scoresum[,1] = team_names
  scoresum[,"Bonus"] <- -2000
  scoresum[,"Total"] <- -2000}

#Create/load Travle log 
if(file.exists("travel_log_updated.csv")) {
  # starts from existing log
  Travel_log <- readr::read_csv("travel_log_updated.csv") # write csv on interaction...
} else {
  Travel_log <- data.frame (matrix(vector(), 0, 5, dimnames=list(c(), c("Team","Group","Log","Action","Time"))))
}


ui <- fluidPage(
  titlePanel(title = "", windowTitle = "MetroApp"),
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
      actionButton("log","Submit"),
      actionButton("unlog","Delete")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Your team score",
          h3("Your are"),
          h4(shiny::textOutput("shiny_teamname")),
          h4(shiny::textOutput("shiny_group")),
          
          h3("Score summary"),
          #Summary talbe for each cateogry 
          h4(shiny::tableOutput("shiny_groupscore")),
          
          h3("Visited stations/destinations"),
          
          shiny::textOutput("shiny_loggedScore"),
          shiny::tableOutput("shiny_visited_st"),
          shiny::tableOutput("shiny_visited_dest"),
          shiny::tableOutput("shiny_scoresheet_bonus")
        ),
        tabPanel(
         title = "Download data",
         # Input: Choose dataset ----
         selectInput("dataset", "Choose a dataset:",
                     choices = c("Score summary", "Station", "Destination","Bonus","Travel log")),
         
         # Button
         downloadButton("downloadData", "Download")
         
          #h3("Current score summary"),
          #shiny::tableOutput("shiny_scoresum")
        )
        #tabPanel(
        #  title = "Maps",
        #  leafletOutput("mymap"),
        #  p(),
        #  actionButton("recalc", "New points")
        #)
        
      ))))

server <- function(input, output, session) {
  # If we get the scores from Google Sheets:
  # scoresheet = function_to_read_from_google_sheets()
  output$shiny_teamname = renderText(paste("Team:",input$teamname))
  output$shiny_group = renderText(paste("Group",group_list[as.numeric(input$Group)]))
  output$shiny_scoresum = renderTable(scoresum, digits = 0)
  output$shiny_scorerank = renderTable(scorerank, digits = 0)
  output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
 
  #Show the list of visited places
  output$shiny_visited_st = renderTable(scoresheet_station[scoresheet_station[,paste0("team_",input$teamname,"_Total")]>0,c(names(scoresheet_station)[1:2], paste0("team_",input$teamname,"_",group_list))])
  output$shiny_visited_dest = renderTable(scoresheet_dest[scoresheet_dest[,paste0("team_",input$teamname,"_Total")]>0,c(names(scoresheet_dest)[1:2], paste0("team_",input$teamname,"_",group_list))])
  
  #Bonus is always showen
  output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))], digits = 0)
 
  #Log a score when "submit" button is selected
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
      #Calcurate the score 
      scoresheet_station[log_row,paste0("team_",input$teamname,"_Total")]<<-scoresheet_station[log_row,"Points"]
      #write.csv(scoresheet_station, "scoresheet_station_updated.csv", row.names = FALSE)
      
      #Show the list of stations they visited 
      output$shiny_visited_st = renderTable(scoresheet_station[scoresheet_station[,paste0("team_",input$teamname,"_Total")]>0,c(names(scoresheet_station)[1:2], paste0("team_",input$teamname,"_",group_list))])
      
      #Update the total score table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Station"]<<-sum(scoresheet_station[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      #write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      #Track the output 
      Travel_log [dim(Travel_log)[1]+1,] <<- c(input$teamname, input$Group, input$PointScore, "Submit",format(Sys.time(), "%m/%d/%y %H:%M:%OS3"))
      #write.csv(Travel_log, "travel_log_updated.csv",row.names = FALSE)
    }
    #Log Destination socre
    else if  (any(scoresheet_dest == LogScore) == TRUE)
    {#find the row contains LogSocre
      log_row = which(scoresheet_dest == LogScore)
      #Update scoresheet 
      scoresheet_dest[log_row,log_col]<<- 1
      
      #Calculate the score 
      scoresheet_dest[log_row,paste0("team_",input$teamname,"_Total")]<<-scoresheet_dest[log_row,"Score"]      
      write.csv(scoresheet_dest, "scoresheet_dest_updated.csv",row.names = FALSE)
      
      #Show the list of destination they visited 
      output$shiny_visited_dest = renderTable(scoresheet_dest[scoresheet_dest[,paste0("team_",input$teamname,"_Total")]>0,c(names(scoresheet_dest)[1:2], paste0("team_",input$teamname,"_",group_list))])
      
      
      #Update the total socre table
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Destination"]<<-sum(scoresheet_dest[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      #Track the output 
      Travel_log [dim(Travel_log)[1]+1,] <<- c(input$teamname,input$Group, input$PointScore, "Submit",format(Sys.time(), "%m/%d/%y %H:%M:%OS3")) 
      write.csv(Travel_log, "travel_log_updated.csv",row.names = FALSE)
        
      #Check whether team visited bus only stop 
      if (any(BusOnlyDest == LogScore))
      {scoresheet_bonus[VBO_row,log_col] <<-1
      #Calculate the bonus 
      scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_Total")] <<-
        scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_A")]*1000 +scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_B")]*1000-2000
      output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))], digits = 0)
      write.csv(scoresheet_bonus, "scoresheet_bonus_updated.csv",row.names = FALSE)
      
      #Update the total socre table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Bonus"]<<-sum(scoresheet_bonus[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      }
      }
    #Log Bonus Score
    else if (any(scoresheet_bonus == LogScore) == TRUE)
    {#find the row contains LogScore
      log_row = which(scoresheet_bonus == LogScore)
      #Update socresheet  
      scoresheet_bonus[log_row,log_col]<<- 1
      output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))], digits = 0)
      #Track the output 
      Travel_log [dim(Travel_log)[1]+1,] <<- c(input$teamname, input$Group, input$PointScore, "Submit",format(Sys.time(), "%m/%d/%y %H:%M:%OS3")) 
      write.csv(Travel_log, "travel_log_updated.csv",row.names = FALSE)
      
      #Calcurate Bonus score 
      scoresheet_bonus[log_row,paste0("team_",input$teamname,"_Total")]<<-scoresheet_bonus[log_row,"Score"]      
      write.csv(scoresheet_bonus, "scoresheet_bonus_updated.csv",row.names = FALSE)
      #Update the total socre table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Bonus"]<<-sum(scoresheet_bonus[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
    }
    #Update a total score for each group 
    nrow_sumscore<- which (scoresum  == input$teamname) 
    scoresum[nrow_sumscore,"Total"]<<-sum(as.numeric(scoresum[nrow_sumscore,2:4]))
    write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
  })
  
  #Delete a score when "delete" button is hit
  observeEvent(input$unlog,{
    log_col <- paste0("team_",input$teamname,"_",group_list[as.numeric(input$Group)])
    LogScore <-input$PointScore
    print(LogScore)
    #Log STATION score
    if (any(scoresheet_station == LogScore) == TRUE)
    {#find the row contains LogScore
      log_row = which(scoresheet_station == LogScore)
      #Update score table 
      scoresheet_station[log_row,log_col]<<- 0
      write.csv(scoresheet_station, "scoresheet_station_updated.csv", row.names = FALSE)
      
      
      # Delete the score if other group has not scored 
      if (any(scoresheet_station[log_row, paste0("team_",input$teamname,"_",group_list[1:2])] == 1) == FALSE)
      {scoresheet_station[log_row,paste0("team_",input$teamname,"_Total")]<<- 0 }
      write.csv(scoresheet_station, "scoresheet_station_updated.csv", row.names = FALSE)
      
      #Show the list of stations they visited 
      output$shiny_visited_st = renderTable(scoresheet_station[scoresheet_station[,paste0("team_",input$teamname,"_Total")]>0,c(names(scoresheet_station)[1:2], paste0("team_",input$teamname,"_",group_list))])
      
      #Update the total score table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Station"]<<-sum(scoresheet_station[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      #Track the output 
      Travel_log [dim(Travel_log)[1]+1,] <<- c(input$teamname, input$Group, input$PointScore, "Delete",format(Sys.time(), "%m/%d/%y %H:%M:%OS3"))
      write.csv(Travel_log, "travel_log_updated.csv",row.names = FALSE)
        }
    #Log Destination socre
    else if  (any(scoresheet_dest == LogScore) == TRUE)
    {#find the row contains LogSocre
      log_row = which(scoresheet_dest == LogScore)
      #Update scoresheet 
      scoresheet_dest[log_row,log_col]<<- 0
      write.csv(scoresheet_dest, "scoresheet_dest_updated.csv",row.names = FALSE)
      
      # Delete the score if other group has not scored 
      if (any(scoresheet_dest[log_row, paste0("team_",input$teamname,"_",group_list[1:2])] == 1) == FALSE)
      {scoresheet_dest[log_row,paste0("team_",input$teamname,"_Total")]<<- 0 
      write.csv(scoresheet_dest, "scoresheet_dest_updated.csv",row.names = FALSE)
      }
     
      #Show the list of destination they visited 
      output$shiny_visited_dest = renderTable(scoresheet_dest[scoresheet_dest[,paste0("team_",input$teamname,"_Total")]>0,c(names(scoresheet_dest)[1:2], paste0("team_",input$teamname,"_",group_list))])
      
      #Update the total socre table
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Destination"]<<-sum(scoresheet_dest[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      
      #Track the output 
      Travel_log [dim(Travel_log)[1]+1,] <<- c(input$teamname, input$Group, input$PointScore, "Delete",format(Sys.time(), "%m/%d/%y %H:%M:%OS3"))
      write.csv(Travel_log, "travel_log_updated.csv",row.names = FALSE)
      
      #Check whether team visited bus only stop 
      if (any(BusOnlyDest == LogScore))
      {scoresheet_bonus[VBO_row,log_col] <<- 0
      write.csv(scoresheet_bonus, "scoresheet_bonus_updated.csv",row.names = FALSE)
      #Calculate the bonus 
      scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_Total")] <<-
        scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_A")]*1000 +scoresheet_bonus[VBO_row,paste0("team_",input$teamname,"_B")]*1000-2000
      write.csv(scoresheet_bonus, "scoresheet_bonus_updated.csv",row.names = FALSE)
      output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))], digits = 0)
      #Update the total socre table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Bonus"]<<-sum(scoresheet_bonus[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      }
    }
    #Log Bonus Score
    else if (any(scoresheet_bonus == LogScore) == TRUE)
    {#find the row contains LogScore
      log_row = which(scoresheet_bonus == LogScore)
      #Update socresheet  
      scoresheet_bonus[log_row,log_col]<<- 0
      write.csv(scoresheet_bonus, "scoresheet_bonus_updated.csv",row.names = FALSE)
       output$shiny_scoresheet_bonus = renderTable(scoresheet_bonus[c(names(scoresheet_bonus)[1:2], paste0("team_",input$teamname,"_",group_list))], digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      
      # Delete the score if other group has not scored 
      if (any(scoresheet_bonus[log_row, paste0("team_",input$teamname,"_",group_list[1:2])] == 1) == FALSE)
      {scoresheet_bonus[log_row,paste0("team_",input$teamname,"_Total")]<<- 0
      write.csv(scoresheet_bonus, "scoresheet_bonus_updated.csv",row.names = FALSE)}
      
     #Update the total socre table 
      sumlog_row = which (scoresum == input$teamname)
      scoresum[sumlog_row,"Bonus"]<<-sum(scoresheet_bonus[,paste0("team_",input$teamname,"_Total")])
      output$shiny_scoresum = renderTable(scoresum, digits = 0)
      output$shiny_groupscore = renderTable (scoresum[scoresum[,colnames(scoresum)[1]] == input$teamname,], digits = 0)
      write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
      
      
      #Track the change
      Travel_log [dim(Travel_log)[1]+1,] <<- c(input$teamname, input$Group, input$PointScore, "Delete",format(Sys.time(), "%m/%d/%y %H:%M:%OS3"))
      write.csv(Travel_log, "travel_log_updated.csv",row.names = FALSE)
      }
    #Update a total score for each group 
    nrow_sumscore<- which (scoresum  == input$teamname) 
    scoresum[nrow_sumscore,"Total"]<<-sum(as.numeric(scoresum[nrow_sumscore,2:4]))
    write.csv(scoresum, "scoresum_updated.csv", row.names = FALSE)
    
  })
  
  ### download data 
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Score summary" = scoresum,
           "Station" = scoresheet_station,
           "Destination" = scoresheet_dest,
           "Bonus" = scoresheet_bonus,
           "Travel log" = Travel_log
           )
  })
  
  # Table of selected dataset ----
  #output$table <- renderTable({
   # datasetInput()
  #})
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  #allow users to reconnect instead of start new session 
  session$allowReconnect(TRUE)
  # This is the map
 # points <- eventReactive(input$recalc, {
  #  stations
  #}, ignoreNULL = FALSE)
  #output$mymap <- renderLeaflet({
   # leaflet() %>%
    #  addProviderTiles(providers$Stamen.TonerLite,
    #                   options = providerTileOptions(noWrap = TRUE)) %>%
    #  addMarkers(data = points())
  #})
}

shinyApp(ui = ui, server = server)
