library(shiny)
library(shinythemes)
library(ggplot2)
source('ergast_calls.R')
  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Formula 1 Shiny",
      tabPanel("Teams",
               sidebarPanel(
                 tags$h3("Menù"),
                 selectInput("seasonTabTeams", 
                             "Select season",
                             c(2000:2021),
                             selected = 2020,
                             ),
                 tableOutput("pointsSystemTeam"),
                 selectizeInput("teamName", choices = c(), label = "Select team"),
                 sliderInput(inputId = "races",
                             label="Number of races ",
                             min = 1,
                             max = 23,
                             dragRange=TRUE,
                             value=c(5,9)),
                 imageOutput("logoTeam"),
                 h2(textOutput("nameteam")),
                 h3("Drivers:"),
                 h3(textOutput("driver1")),
                 h3(textOutput("driver2"))
                 
                 

               ), # sidebarPanel
               mainPanel(
                            h1("Team stats"),
                            h4("In this section is possible to visualize the results of a whole Formula One team in a given season and is also possible to see the evolution of the results from one race to the other"),
                            textOutput("title"),
                            plotOutput("teamPointsChart"),
                            fluidRow(column(6,h2("Standings position:")),column(2,h2(textOutput("position"))),column(4,h3(textOutput("avgPoints")))),
                            h2("Season Review"),
                            fluidRow(column(4,h3("Wins")),column(4,h3(textOutput("wins"))),column(4,h3(textOutput("percWins")))),
                            fluidRow(column(4,h3("Podiums")),column(4,h3(textOutput("podiums")))),
                            fluidRow(column(4,h3("Pole Positions")),column(4,h3(textOutput("poles")))),
                            fluidRow(column(4,h3("Fastest Laps")),column(4,h3(textOutput("fastestLaps")))),
                            fluidRow(column(4, h3("Best race result")),column(4,h3(textOutput("best")))),
                            fluidRow(column(4,h3("Retirements")),column(4,h3(textOutput("dnf")))),
               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Drivers",
               sidebarPanel(
                 tags$h3("Menù"),
                 selectInput("seasonTabDrivers", 
                             "Select season",
                             c(2000:2021),
                             selected = 2020),
                 tableOutput("pointsSystemDriver"),
                 tags$h3("Driver 1"),
                 selectizeInput("teamName1", choices = c(), label = "Select team"),
                 selectizeInput("driverName1", choices = c(), label = "Select driver"),
                 tags$h3("Driver 2"),
                 selectizeInput("teamName2", choices = c(), label = "Select team"),
                 selectizeInput("driverName2", choices = c(), label = "Select driver")
                 ),
               mainPanel(
                 h1("One VS One - Drivers stats"),
                 h4("In this section is possible to select a Formula One driver from a given team in a given season and compare the results over time during the season"),
                 plotOutput("driverPointsChart"),
                 fluidRow(column(6,h3(textOutput("D1"))),column(6,align="right",h3(textOutput("D2")))),
                 fluidRow(column(6,h4(textOutput("teamD1"))),column(6,align="right",h4(textOutput("teamD2")))),
                 fluidRow(column(12,align="center",h3("Standings"))),
                 fluidRow(column(6,h3(textOutput("standingsD1"))),column(6,align="right",h3(textOutput("standingsD2")))),
                 fluidRow(column(12,align="center",h3("Wins"))),
                 fluidRow(column(6,h3(textOutput("winsD1"))),column(6,align="right",h3(textOutput("winsD2")))),
                 fluidRow(column(12,align="center",h3("Podiums"))),
                 fluidRow(column(6,h3(textOutput("podiumsD1"))),column(6,align="right",h3(textOutput("podiumsD2")))),
                 fluidRow(column(12,align="center",h3("Poles"))),
                 fluidRow(column(6,h3(textOutput("polesD1"))),column(6,align="right",h3(textOutput("polesD2")))),
                 fluidRow(column(12,align="center",h3("Fastest Laps"))),
                 fluidRow(column(6,h3(textOutput("fastestLapsD1"))),column(6,align="right",h3(textOutput("fastestLapsD2")))),
                 fluidRow(column(12,align="center",h3("Retirements"))),
                 fluidRow(column(6,h3(textOutput("dnfD1"))),column(6,align="right",h3(textOutput("dnfD2")))),
                 fluidRow(column(12,align="center",h3("Average points scored per race"))),
                 fluidRow(column(6,h3(textOutput("avgPointsD1"))),column(6,align="right",h3(textOutput("avgPointsD2")))),
                 fluidRow(column(12,align="center",h3("Average qualifying position"))),
                 fluidRow(column(6,h3(textOutput("avgQualiD1"))),column(6,align="right",h3(textOutput("avgQualiD2")))),
                fluidRow(column(12,align="center",h3("Average finish position"))),
                fluidRow(column(6,h3(textOutput("avgFinishedD1"))),column(6,align="right",h3(textOutput("avgFinishedD2")))),
               )
        )
      ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output){
    observeEvent(input$seasonTabTeams, {
        output$pointsSystemTeam <- renderTable(reqPointsSystem(input$seasonTabTeams))
        updateSelectInput(session=getDefaultReactiveDomain(),"teamName", choices = reqTeams(input$seasonTabTeams)$names, selected = "" )
        updateSliderInput(session=getDefaultReactiveDomain(),"races", min = 1, max = reqRaceRounds(input$seasonTabTeams), value = 5)
      })
    observeEvent(input$teamName,{
      summarySeason <<- reqTeamPoints(input$seasonTabTeams, input$teamName, input$races)
      if(!nzchar(summarySeason)){
        output$title <- renderText({"No team selected"})
      }
      else{
        df <- data.frame(races = seq(from=input$races[1], to=input$races[2], by=1), points = summarySeason$pointsProgress)
        output$teamPointsChart <- renderPlot({ggplot(df, aes(races, points)) + geom_line() + geom_label(aes(label=points), vjust=0.5, color="white", fill="steelblue",nudge_y=0.3,nudge_x=0, size=7)
        })
        output$logoTeam <- renderImage({
          filename <- normalizePath(file.path("logos", paste(summarySeason$teamId,".png",sep="")))
          list(src = filename,
               width = "100%",
               height = "auto")
        },deleteFile = FALSE)
        output$avgPoints <- renderText({
          n_races <- length(c(input$races[1]:input$races[2]))
          total_points <- tail(df$points[-1],1)
          paste("Average points scored", round(total_points/n_races, digits = 2))
        })
        output$driver1 <- renderText({summarySeason$drivers$driverNames[1]})
        output$driver2 <- renderText({summarySeason$drivers$driverNames[2]})
        output$position <- renderText({summarySeason$position
        })
        output$nameteam <- renderText({input$teamName
        })
        output$wins <- renderText({summarySeason$wins
        })
        output$percWins <- renderText({paste("Percentage of wins:",round(summarySeason$wins/input$races[2],digits =2)*100,"%", sep="")})
        
        output$podiums <- renderText({summarySeason$podiums
        })
        output$poles <- renderText({summarySeason$polePositions
        })
        output$fastestLaps <- renderText({summarySeason$fastestLaps
        })
        output$best <- renderText({paste(summarySeason$raceBestResult,"-",summarySeason$maximumPoints," points - Finished in", summarySeason$positionsBest)
        })
        output$dnf <- renderText({summarySeason$retirements
        })
      }
    })
      
    observeEvent(input$races,{
        summarySeason <<- reqTeamPoints(input$seasonTabTeams, input$teamName, input$races)
        if(!nzchar(summarySeason)){
          output$title <- renderText({"No team selected"})
        }
        else{
          df <- data.frame(races = seq(from=input$races[1], to=input$races[2]), points = summarySeason$pointsProgress)
          output$teamPointsChart <- renderPlot({
            ggplot(df, aes(races, points)) +
            geom_line() + 
            geom_label(aes(races, points,label=points), vjust=0.5, color="white", fill="steelblue",nudge_y=0.3,nudge_x=0, size=7)
          })
          output$position <- renderText({summarySeason$position
          })
          output$logoTeam <- renderImage({
            filename <- normalizePath(file.path("logos", paste(summarySeason$teamId,".png",sep="")))
            list(src = filename,
                 width = "100%",
                 height = "auto")},
            deleteFile = FALSE)
          output$wins <- renderText({summarySeason$wins})
          output$percWins <- renderText({paste("Percentage of wins:",round(summarySeason$wins/input$races[2],digits =2)*100,"%", sep="")})
          output$podiums <- renderText({summarySeason$podiums})
          output$avgPoints <- renderText({
            n_races <- length(c(input$races[1]:input$races[2]))
            total_points <- tail(df$points[-1],1)
            paste("Average points scored", round(total_points/n_races, digits = 2))
          })
          output$poles <- renderText({summarySeason$polePositions})
          output$driver1 <- renderText({summarySeason$drivers$driverNames[1]})
          output$driver2 <- renderText({summarySeason$drivers$driverNames[2]})
          output$fastestLaps <- renderText({summarySeason$fastestLaps})
          output$best <- renderText({
            paste(summarySeason$raceBestResult,"-",summarySeason$maximumPoints," points - Finished in", summarySeason$positionsBest)
          })
          output$dnf <- renderText({summarySeason$retirements})
        }
    })
    
    observeEvent(input$seasonTabDrivers,{
      updateSelectInput(session=getDefaultReactiveDomain(),"teamName1", choices = reqTeams(input$seasonTabDrivers)$names, selected = "" )
      updateSelectInput(session=getDefaultReactiveDomain(),"teamName2", choices = reqTeams(input$seasonTabDrivers)$names, selected = "" )
      driversTeam1 <- reqDrivers(input$seasonTabDrivers, input$teamName1)
      driversTeam2 <- reqDrivers(input$seasonTabDrivers, input$teamName2)
      output$pointsSystemDriver<- renderTable(reqPointsSystem(input$seasonTabDrivers))
      updateSelectInput(session=getDefaultReactiveDomain(),"driverName1", choices = driversTeam1$driverNames, selected = "" )
      updateSelectInput(session=getDefaultReactiveDomain(),"driverName2", choices = driversTeam2$driverNames, selected = "" )
    })
    observeEvent(input$teamName1,{
      driversTeam1 <- reqDrivers(input$seasonTabDrivers, input$teamName1)
      updateSelectInput(session=getDefaultReactiveDomain(),"driverName1", choices = driversTeam1$driverNames, selected = "" )
    })
    observeEvent(input$teamName2,{
      driversTeam2 <- reqDrivers(input$seasonTabDrivers, input$teamName2)
      updateSelectInput(session=getDefaultReactiveDomain(),"driverName2", choices = driversTeam2$driverNames, selected = "" )
    })
    observeEvent(input$driverName1,{
      seasonDriver1 <- reqSeasonResult(input$seasonTabDrivers, input$driverName1)
      seasonDriver2 <- reqSeasonResult(input$seasonTabDrivers, input$driverName2)
      print(seasonDriver1)
      if(length(seasonDriver1)!=0 && length(seasonDriver2)==0){
        races_axis <- c(1:seasonDriver1$races)
        output$driverPointsChart <- renderPlot({
          ggplot()+
            geom_line(data = as.data.frame(seasonDriver1), aes(x=races_axis, y=points,  ), color="darkred")+
            geom_label(aes(races_axis, seasonDriver1$points, label=seasonDriver1$points), vjust=0.5, color="white", fill="darkred",nudge_y=0.3,nudge_x=0, size=7)
          
        })
      }
      if(length(seasonDriver1)==0 && length(seasonDriver2)!=0){
        races_axis <- c(1:seasonDriver2$races)
        output$driverPointsChart <- renderPlot({
          ggplot()+
            geom_line(data = as.data.frame(seasonDriver2), aes(x=races_axis, y=points), color="darkred")+
            geom_label(aes(races_axis, seasonDriver2$points, label=seasonDriver2$points), vjust=0.5, color="white", fill="darkred",nudge_y=0.3,nudge_x=0, size=7)
          
        })
      }
      if(length(seasonDriver1)!=0 && length(seasonDriver2)!=0){
        races_axis <- c(1:seasonDriver1$races)
        output$driverPointsChart <- renderPlot({
          ggplot()+
            geom_line(data=as.data.frame(seasonDriver1), aes(x=races_axis, y=points, color="darkred"))+
            geom_label(aes(races_axis, seasonDriver1$points, label=seasonDriver1$points), vjust=0.5, color="white", fill="darkred",nudge_y=0.3,nudge_x=0, size=7)+
            geom_line(data=as.data.frame(seasonDriver2), aes(x=races_axis, y=points, color="steelblue"))+
            geom_label(aes(races_axis, seasonDriver2$points, label=seasonDriver2$points), vjust=0.5, color="white", fill="steelblue",nudge_y=0.3,nudge_x=0, size=7)+
            scale_color_identity(name = "Drivers points",
                                 breaks = c("darkred", "steelblue"),
                                 labels = c(input$driverName1, input$driverName2),
                                 guide = "legend")+
            theme(legend.title = element_text(size = 19),
                  legend.text = element_text(size = 12),
                  axis.title = element_text(size = 19))

        })
      }
      output$D1 <- renderText(input$driverName1)
      output$winsD1 <- renderText({seasonDriver1$wins})
      output$podiumsD1 <- renderText({seasonDriver1$podiums})
      output$polesD1 <- renderText({seasonDriver1$poles})
      output$fastestLapsD1 <- renderText({seasonDriver1$fastestLaps})
      output$dnfD1 <- renderText({seasonDriver1$retirements})
      output$standingsD1 <- renderText({paste(seasonDriver1$standings,"°", sep="")})
      output$teamD1 <- renderText({seasonDriver1$team})  
      output$avgQualiD1 <- renderText({
        round(mean(seasonDriver1$qualified,na.rm=TRUE), digits = 1)
      })
      output$avgFinishedD1 <- renderText({
        round(mean(seasonDriver1$finished, na.rm=TRUE),digits = 1)
      })
      output$avgPointsD1 <- renderText({
        round(tail(seasonDriver1$points,1)/sum(seasonDriver1$finished>0, na.rm = TRUE), digits = 1)
      })
     })
    observeEvent(input$driverName2,{
      seasonDriver1 <- reqSeasonResult(input$seasonTabDrivers, input$driverName1)
      seasonDriver2 <- reqSeasonResult(input$seasonTabDrivers, input$driverName2)
      print(seasonDriver2)
      if(length(seasonDriver1)!=0 && length(seasonDriver2)==0){
        races_axis <- c(1:seasonDriver1$races)
        output$driverPointsChart <- renderPlot({
          ggplot()+
            geom_line(data = as.data.frame(seasonDriver1), aes(x=races_axis, y=points), color="darkred")+
            geom_label(aes(races_axis, seasonDriver1$points, label=seasonDriver1$points), vjust=0.5, color="white", fill="darkred",nudge_y=0.3,nudge_x=0, size=7)
        })
      }
      if(length(seasonDriver1)==0 && length(seasonDriver2)!=0){
        races_axis <- c(1:seasonDriver2$races)
        output$driverPointsChart <- renderPlot({
          ggplot()+
            geom_line(data = as.data.frame(seasonDriver2), aes(x=races_axis, y=points), color="darkred")+
            geom_label(aes(races_axis, seasonDriver2$points, label=seasonDriver2$points), vjust=0.5, color="white", fill="darkred",nudge_y=0.3,nudge_x=0, size=7)
          })
      }
      if(length(seasonDriver1)!=0 && length(seasonDriver2)!=0){
        races_axis <- c(1:seasonDriver1$races)
        output$driverPointsChart <- renderPlot({
          ggplot()+
            geom_line(data=as.data.frame(seasonDriver1), aes(x=races_axis, y=points, color="darkred"))+
            geom_label(aes(races_axis, seasonDriver1$points, label=seasonDriver1$points), vjust=0.5, color="white", fill="darkred",nudge_y=0.3,nudge_x=0, size=7)+
            geom_line(data=as.data.frame(seasonDriver2), aes(x=races_axis, y=points, color="steelblue"))+
            geom_label(aes(races_axis, seasonDriver2$points, label=seasonDriver2$points), vjust=0.5, color="white", fill="steelblue",nudge_y=0.3,nudge_x=0, size=7)+
            scale_color_identity(name = "Drivers points",
                                 breaks = c("darkred", "steelblue"),
                                 labels = c(input$driverName1, input$driverName2),
                                 guide = "legend")+
            theme(legend.title = element_text(size = 19),
                  legend.text = element_text(size = 12),
                  axis.title = element_text(size = 19))
          
        })
      }
      output$D2 <- renderText(input$driverName2)
      output$winsD2 <- renderText({seasonDriver2$wins})
      output$podiumsD2 <- renderText({seasonDriver2$podiums})
      output$polesD2 <- renderText({seasonDriver2$poles})
      output$fastestLapsD2 <- renderText({seasonDriver2$fastestLaps})
      output$dnfD2 <- renderText({seasonDriver2$retirements})
      output$standingsD2 <- renderText({paste(seasonDriver2$standings,"°", sep="")})
      output$teamD2 <- renderText({seasonDriver2$team})  
      
      output$avgQualiD2 <- renderText({
        round(mean(seasonDriver2$qualified,na.rm=TRUE), digits = 1)
      })
      output$avgFinishedD2 <- renderText({
        round(mean(seasonDriver2$finished, na.rm=TRUE),digits = 1)
      })
      output$avgPointsD2 <- renderText({
        round(tail(seasonDriver2$points,1)/sum(seasonDriver2$finished>0, na.rm = TRUE), digits = 1)
      })
      
    })
    

  } # server
  


  # Create Shiny object
  shinyApp(ui = ui, server = server)
