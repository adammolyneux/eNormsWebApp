
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  navbarPage("E-Norms Web App",
             
             tabPanel("Data Location",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            textInput("path", label = "Enter directory of XLS files", 
                                      value = "/Users/adam/Dropbox/Research/Matthew Pitt/enorms"),
                            textInput("emgstr", label = "Enter text specific for EMG files:", 
                                      value = "EMG"),
                            textInput("sensstr", label = "Enter text specific for sensory files:", 
                                      value = "SNAP"),
                            textInput("cmapstr", label = "Enter text specific for motor files:", 
                                      value = "CMAP")
                          ),
                          mainPanel(
                            h6(tableOutput("dataSum"))
                          )
                        )
                      )),
             
             tabPanel("EMG Data",
                      fluidPage(
                        
                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Select subgroup of data to anaylse."),
                            sliderInput ("Age",
                                         "Age Range:",
                                         min = 0,
                                         max = 18,
                                         value = c(1,5)),
                            selectInput("Muscle", 
                                        label = "Choose a muscle to display",
                                        choices = c("Tibialis anterior", "Glossus",
                                                    "Biceps"),
                                        selected = "Biceps"),
                            
                            selectInput("select", label = "Choose measure:", 
                                        choices = list("Simple Amp" = 7, "Simple Dur" = 8,
                                                       "Polyphasic Amp" = 9, "Polyphasic Dur" = 10,
                                                       "Mean Amp" = 11, "Mean Dur" = 12,
                                                       "Poly Percent" = 13
                                        ), selected = 7),
                            sliderInput("Xrange",
                                        "Y axis upper limit for top plot:",
                                        min=0.9,
                                        max=1,
                                        value=0.99),
                            sliderInput("XrangeL",
                                        "Y axis lower limit for top plot:",
                                        min=0,
                                        max=0.2,
                                        value=0.01),
                            sliderInput("Yrange",
                                        "Y axis upper limit for bottom plot:",
                                        min=0.9,
                                        max=1,
                                        value=0.99),
                            checkboxInput("logE", "Plot as log", FALSE),
                            uiOutput("emgControls")
                            
                          ), 
                          # Show a plot of the generated distribution
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Summary",
                                       tableOutput("sumTable")),
                              tabPanel("Ordered plots",
                                       plotOutput("cumPlot"),
                                       plotOutput("difPlot")),
                              tabPanel("Distribution",
                                       plotOutput("normPlot"),
                                       plotOutput("normPlot2"))
                            )
                          )
                        ))),
             
             
             tabPanel("Sensory SNAPs",
                      fluidPage(
                        sidebarLayout(
                          
                          sidebarPanel(
                            helpText("Select subgroup of data to anaylse."),
                            sliderInput ("AgeS",
                                         "Age Range:",
                                         min = 0,
                                         max = 18,
                                         value = c(1,5)),
                            selectInput("NerveS", 
                                        label = "Choose a nerve to display",
                                        choices = c("Dig plant med Sensory","Peroneus superfic Sensory","Suralis Sensory"),
                                        selected = "Suralis Sensory"),
                            selectInput("selectS", label = "Choose measure:", 
                                        choices = list("Onset" = 8, "Amp" = 9,"CV" = 10), 
                                        selected = 9),
                            sliderInput("XrangeS",
                                        "Y axis upper limit for top plot:",
                                        min=0.9,
                                        max=1,
                                        value=0.99),
                            sliderInput("XrangeSL",
                                        "Y axis lower limit for top plot:",
                                        min=0,
                                        max=0.2,
                                        value=0.01),
                            sliderInput("YrangeS",
                                        "Y axis upper limit for bottom plot:",
                                        min=0.9,
                                        max=1,
                                        value=0.99),
                            checkboxInput("logS", "Plot as log", FALSE),
                            uiOutput("sensoryContr")
                            
                          ),
                          
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Summary",
                                       tableOutput("sumTableS")),
                              tabPanel("Ordered plots",
                                       plotOutput("cumPlotS"),
                                       plotOutput("diffPlotS")),
                              tabPanel("Distribution",
                                       plotOutput("normPlotS"),
                                       plotOutput("normPlot2S"))
                            )
                          )
                        ))),
             
             tabPanel("Motor CMAPs",
                      fluidPage(
                        sidebarLayout(
                          
                          sidebarPanel(
                            helpText("Select subgroup of data to anaylse."),
                            sliderInput ("AgeM",
                                         "Age Range:",
                                         min = 0,
                                         max = 18,
                                         value = c(1,5)),
                            selectInput("NerveM", 
                                        label = "Choose a nerve to display",
                                        choices = c("Medianus Motor", "Peroneus Motor" ,"Tibialis Motor", "Ulnaris Motor" ),
                                        selected = "Medianus Motor"),
                            selectInput("selectM", label = "Choose measure:", 
                                        choices = list("Onset" = 8, "Amp" = 9,"CV" = 10, "NegDur"=11,"Area"=12,"Latency"=13), 
                                        selected = 9),
                            sliderInput("XrangeM",
                                        "Y axis upper limit for top plot:",
                                        min=0.9,
                                        max=1,
                                        value=0.99),
                            sliderInput("XrangeML",
                                        "Y axis lower limit for top plot:",
                                        min=0,
                                        max=0.2,
                                        value=0.01),
                            sliderInput("YrangeM",
                                        "Y axis upper limit for bottom plot:",
                                        min=0.9,
                                        max=1,
                                        value=0.99),
                            checkboxInput("logM", "Plot as log", FALSE),
                            uiOutput("motorContr")
                            
                          ),
                          
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Summary",
                                       tableOutput("sumTableM")),
                              tabPanel("Ordered plots", 
                                       plotOutput("cumPlotM"),
                                       plotOutput("diffPlotM")),
                              tabPanel("Distribution",
                                       plotOutput("normPlotM"),
                                       plotOutput("normPlot2M"))
                            )
                          )
                        )))
  ))
