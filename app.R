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
                 selectizeInput("teamName", choices = c(), label = "Select team"),
                 sliderInput(inputId = "races",
                             label="Number of races ",
                             min = 1,
                             max = 23,
                             value=5),

               ), # sidebarPanel
               mainPanel(
                            h1("Formula One Shiny - Team stats"),
                            h4("In this section is possible to visualize the results of a whole Formula One team in a given season and is also possible to see the evolution of the results from one race to the other"),
                            textOutput("title"),
                            plotOutput("teamPointsChart"),
                            fluidRow(column(6,h2("Standings position:")),column(4,h2(textOutput("position")))),
                            h2("Season Review"),
                            fluidRow(column(4,h3("Wins")),column(4,h3(textOutput("wins")))),
                            fluidRow(column(4,h3("Podiums")),column(4,h3(textOutput("podiums")))),
                            fluidRow(column(4,h3("Pole Positions")),column(4,h3(textOutput("poles")))),
                            fluidRow(column(4,h3("Fastest Laps")),column(4,h3(textOutput("fastestLaps")))),
                            fluidRow(column(4, h3("Best race result")),column(4,h3(textOutput("best")))),
                            fluidRow(column(4,h3("Retirements")),column(4,h3(textOutput("dnf")))),
               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Drivers", "This panel is intentionally left blank")
      ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output){
    observeEvent(input$seasonTabTeams, {
        updateSelectInput(session=getDefaultReactiveDomain(),"teamName", choices = reqTeams(input$seasonTabTeams)$names, selected = "" )
        updateSliderInput(session=getDefaultReactiveDomain(),"races", min = 1, max = reqRaceRounds(input$seasonTabTeams), value = 5)
      })
    observeEvent(input$teamName,{
      summarySeason <<- reqTeamPoints(input$seasonTabTeams, input$teamName, input$races)
      if(!nzchar(summarySeason)){
        #output$title <- renderText({"No team selected"})
      }
      else{
        df <- data.frame(races = c(1:input$races), points = summarySeason$pointsProgress)
        output$teamPointsChart <- renderPlot({ggplot(df, aes(races, points)) + geom_line() + geom_text(aes(label=points), vjust=0.5, color="blue",nudge_y=0.3,nudge_x=0, size=7)
        })
        summarySeason <- head(summarySeason,1)
        output$position <- renderText({summarySeason$position
        })
        output$wins <- renderText({summarySeason$wins
        })
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
          #output$title <- renderText({"No team selected"})
        }
        else{
          df <- data.frame(races = c(1:input$races), points = summarySeason$pointsProgress)
          print(df$points)
          output$teamPointsChart <- renderPlot({ggplot(df, aes(races, points)) + geom_line() + geom_text(aes(label=points), vjust=0.5, color="blue",nudge_y=0.3,nudge_x=0, size=7)
          })
          summarySeason <- head(summarySeason,1)
          output$position <- renderText({summarySeason$position
          })
          output$wins <- renderText({summarySeason$wins
          })
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
  } # server
  


  # Create Shiny object
  shinyApp(ui = ui, server = server)
