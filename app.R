library(shiny)
library(leaflet)

#ui <- fluidPage(
#  h1("Metropoly game")
#  )

ui <- navbarPage(
  title = "Metropoly game",
  tabPanel(title = "About",
           h1("What is Metropoly game?"),
           p("ITS")),
  tabPanel(title = "Game rules",
           h1("How to play Metropoly game "),
           p("ITS")),
  tabPanel(
    title = "Scoreboard",
    textInput(
      inputId = "teamname",
      label = "Your team name",
      value = "Your team name"
    ),
    
    radioButtons(
      inputId = "Group",
      label = "Your group",
      choices = list("Group A" = 1,
                     "Group B" = 2),
      selected = 1
    )
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