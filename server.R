shinyServer(
  function(input, output) {
    subGraph <- reactive({
      minDays <- as.numeric(difftime(as.Date("2015-11-19"),input$timeSelection[2]))
      maxDays <- as.numeric(difftime(as.Date("2015-11-19"),input$timeSelection[1]))
      
      subPeople <- subset(InfoFarmPeople, (numberOfDays >= minDays) & (numberOfDays <= maxDays))
      subIDs    <- subPeople$startOrder
      
      subLinks <- subset(InfoFarmLinks, (ID1 %in% subIDs) & (ID2 %in% subIDs))
      
      graph <- graph_from_data_frame(subLinks, directed = FALSE)
      
      return(list(graph,subLinks,subPeople))
    })
    
    output$people <- renderDataTable({
      subGraph()[[3]]
    })
    
    output$graphData <- renderDataTable({
      temp <- subGraph()[[2]]
      temp <- merge(temp,InfoFarmPeople[,1:2], by.x="ID1",by.y="startOrder")
      colnames(temp)[5] <- "Person1"
      temp <- merge(temp,InfoFarmPeople[,1:2], by.x="ID2",by.y="startOrder")
      colnames(temp)[6] <- "Person2"
      temp <- temp[,c("Person1","Person2","daysTogether")]
      
      return(temp)
    }) 
    
    output$graphPlot <- renderPlot({
      plot(subGraph()[[1]])
    })
    
    output$degree <- renderDataTable({
      temp <- data.frame(ID = names(degree(subGraph()[[1]])),
                         degree = degree(subGraph()[[1]]))
      temp <- merge(temp,InfoFarmPeople[,1:2],by.x="ID",by.y="startOrder")
      
      return(temp[,c("name","degree")])
    })
    
    output$closeness <- renderDataTable({
      temp <- data.frame(ID = names(closeness(subGraph()[[1]])),
                         closeness = closeness(subGraph()[[1]]),normalized = TRUE)
      temp <- merge(temp,InfoFarmPeople[,1:2],by.x="ID",by.y="startOrder")
      
      return(temp[,c("name","closeness")])
    })
    
    output$betweenness <- renderDataTable({
      temp <- data.frame(ID = names(betweenness(subGraph()[[1]])),
                         betweenness = betweenness(subGraph()[[1]]),normalized = TRUE)
      temp <- merge(temp,InfoFarmPeople[,1:2],by.x="ID",by.y="startOrder")
      
      return(temp[,c("name","betweenness")])
    })
    
    output$edgeBetweenness <- renderDataTable({
      temp <- data.frame(link = attr(E(subGraph()[[1]]),"vnames"),
                         edgeBetweenness = as.integer(edge.betweenness(subGraph()[[1]], directed = FALSE)))
      temp$id1 <- unlist(strsplit(as.character(temp$link),"\\|"))[c(T,F)]
      temp$id2 <- unlist(strsplit(as.character(temp$link),"\\|"))[c(F,T)]
      
      temp <- merge(temp,InfoFarmPeople[,1:2],by.x="id1",by.y="startOrder")
      colnames(temp)[5] <- "Person1"
      temp <- merge(temp,InfoFarmPeople[,1:2],by.x="id2",by.y="startOrder")
      colnames(temp)[6] <- "Person2"
      
      return(temp[,c("Person1","Person2","edgeBetweenness")])
    })
    
    output$cliques <- renderText({
      temp <- maximal.cliques(subGraph()[[1]])
      text <- ""
      for (i in 1:length(temp)){
        ids    <- temp[[i]]
        people <- subset(InfoFarmPeople, startOrder %in% as.numeric(attr(ids,"name")))$name
        text <- paste(text,
                      paste("The people in clique",
                            i,
                            "are:",
                            paste(people, collapse =", "),
                            sep=" "),
                      sep = "\n")
      }
      return(text)
    })
    
    output$chosenUI <- renderUI({
      if (input$type == "People"){
        dataTableOutput("people")
      } else if (input$type == "Links"){
        dataTableOutput("graphData")
      } else if (input$type == "Plot"){
        plotOutput("graphPlot")
      } else if (input$type == "Degree"){
        dataTableOutput("degree")
      } else if (input$type == "Closeness"){
        dataTableOutput("closeness")
      } else if (input$type == "Betweenness"){
        dataTableOutput("betweenness")
      } else if (input$type == "Edge betweenness"){
        dataTableOutput("edgeBetweenness")
      } else {
        verbatimTextOutput("cliques")
      }
    })
  }
)