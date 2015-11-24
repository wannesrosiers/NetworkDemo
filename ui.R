# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage( 
    
    # Give the page a title
    titlePanel("The InfoFarm Graph"),
    
    # Create sidebar layout
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("timeSelection",
                       "Select your time period",
                       start = "2013-11-14",
                       end = "2015-11-19",
                       min = "2013-11-14",
                       max = "2015-11-19"),
        selectInput("type",
                    "Interested in:",
                    c("People","Links","Plot","Degree","Closeness","Betweenness","Edge betweenness","Cliques"),
                    selected = "People")
        ),
      mainPanel(uiOutput("chosenUI")),
                #tableOutput("temp"))
    )
  )
)