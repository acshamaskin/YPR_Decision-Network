library(shiny)
library(shinythemes)
library(markdown)


source('global.R',local=T)
shinyUI(fluidPage(
  navbarPage("Length Limit Explorer",theme = shinytheme("superhero"),
             tabPanel("Build Simulation",
#titlePanel("Enter Simulation Parameters Below"),
  sidebarLayout(position = "left",
    sidebarPanel(h3(strong("Welcome to Length Limit Explorer"),style = "font-family: 'times'; font-si6pt"),width=5,
                 h5(strong("Equipped with an extension of the Beverton-Holt equilibrium yield model, this application
                           can simulate minimum length limits for any combination of population dynamics
                           that you can think of.  To get started, select which
                           species you would like to simulate."),style= "font-family: 'times'; font-si6pt"),
      fluidRow(
        column(5,
               selectInput("Sppsel",
                         label =h4(strong("Select Species"),style = "font-family: 'times'; font-si6pt"),selectize = T,choices = c("White Crappie","Black Crappie",
                                                                               "Largemouth Bass","Smallmouth Bass",
                                                                               "Spotted Bass",
                                                                               "Striped Bass","White Bass",
                                                                               "Walleye","Sauger","Northern Pike"),
                         selected = "White Crappie",multiple = F)))),
    mainPanel(width=7,
      tabsetPanel(type = "tabs",
                  tabPanel("First Population Group",
                           fluidRow(
                             column(5,
                                    textInput("nlakes",
                                              label =h5(strong("Number of Populations"),style = "font-family: 'times'; font-si6pt"), value = "")),
                             column(2,
                                    tags$head(tags$style("#initpops {margin-left: -22px;
                                                         margin-top: 35px;
                                                         background-color:orange;
                                                         vertical-align: middle;}")),
                                    actionButton("initpops",label = h5(strong("Initialize"),style = "font-family: 'times'; font-si6pt;vertical-align: middle;"))
                                    )),
                           h4(strong("First Parameter Estimates"),style = "font-family: 'times'; font-si6pt"),
                           fluidRow(   #WL and A Parameters
                             column(3,
                                    tags$style("input[type=checkbox] {
                                               transform: scale(1.0);
                                               }"),
               tags$head(tags$style("#b {font-size:33px;height:40px;margin-top: -22px;}"),
                         tags$style("#A {font-size:33px;height:40px;margin-top: -22px;}"),
                         tags$style("#Linf {font-size:33px;height:40px;margin-top: -22px;}"),
                         tags$style("#uprop {font-size:33px;height:40px;margin-top: -22px;}")),
               
               checkboxGroupInput("b",strong("Weight-Length:",style = "font-family: 'times'; font-si6pt"), 
                                  c("High"=4,"Medium"=3,"Low"=2,"Fixed"=1), selected=1)),
               column(3,
                      div(style="height:25px;",
                          #textInput("bprophigh",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"), value = 33),
                          sliderInput("bprophigh",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F,round=T),
                          #tags$head(tags$style(type="text/css", "#bprophigh {height: 10px; font-size: 12px;}")),
                          #textInput("bpropmid",label= NULL, value = 33),
                          sliderInput("bpropmid",label= NULL,min = 0,max = 100,value = 33,ticks = F,round=T),
                          #tags$head(tags$style(type="text/css", "#bpropmid {height: 20px; font-size: 12px;}")),
                          #textInput("bproplow",label= NULL, value = 33),
                          sliderInput("bproplow",label= NULL,min = 0,max = 100,value = 33,ticks = F,round=T),
                          textInput("bfixed",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                          tags$head(tags$style(type="text/css", "#bfixed {height: 20px; font-size: 12px;}"))),
                      br()
               ),
               column(3,
                      checkboxGroupInput("A", strong("Annual Mortality:",style = "font-family: 'times'; font-si6pt"),
                                         c("High"=4,"Medium"=3,"Low"=2,"Fixed"=1), selected=1)),
               column(3,
                      div(style="height:25px;",
                          sliderInput("Aprophigh",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F),
                          sliderInput("Apropmid",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                          sliderInput("Aproplow",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                          textInput("Afixed",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                          tags$head(tags$style(type="text/css", "#Afixed {height: 20px; font-size: 12px;}"))),
                      br()
               )),
               br(),
               br(),
               br(),br(),br(),br(),br(),br(),br(),br(),
               fluidRow(   #VBGF and uprop Parameters
                 column(3,
                        checkboxGroupInput("Linf",strong("Length-at-Age:",style = "font-family: 'times'; font-si6pt"),
                                           c("Fast"=4,"Medium"=3,"Slow"=2,"Fixed"=1), selected=1)),
                 column(3,
                        div(style="height:25px;",
                            sliderInput("Linfprophigh",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("Linfpropmid",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("Linfproplow",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            textInput("Linffixed",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                            tags$head(tags$style(type="text/css", "#Linffixed {height: 20px; font-size: 12px;}"))),
                        br()
                 ),
                 column(3,
                        checkboxGroupInput("uprop", strong("Exploitation:",style = "font-family: 'times'; font-si6pt"),
                                           c("High"=4,"Medium"=3,"Low"=2,"Fixed"=1), selected=1)),
                 column(3,
                        div(style="height:25px;",
                            sliderInput("uprophigh",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("upropmid",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("uproplow",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            textInput("upropfixed",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                            tags$head(tags$style(type="text/css", "#upropfixed {height: 20px; font-size: 12px;}"))),
                        br()
                 )),
               fluidRow(
                 br(),br(),br()
               ),
               fluidRow(
                 br(),br(),br(),br(),br(),br(),br()
               ),
               fluidRow(
                 br(),br()
               )),
                  tabPanel("Second Population Group",
                           fluidRow(
                             column(5,
                                    textInput("nlakes2",
                                              label =h5(strong("Number of Populations"),style = "font-family: 'times'; font-si6pt"), value = "")),
                             column(2,
                                    tags$head(tags$style("#initpops2 {margin-left: -22px;
                                                         margin-top: 35px;
                                                         background-color:orange;
                                                         vertical-align: middle;}")),
                                    actionButton("initpops2",label = h5(strong("Initialize"),style = "font-family: 'times'; font-si6pt;vertical-align: middle;"))
                                    )),
                           h4(strong("Second Parameter Estimates"),style = "font-family: 'times'; font-si6pt"),
                           fluidRow(   #WL and A Parameters
                             column(3,
                                    tags$style("input[type=checkbox] {
                                               transform: scale(1.0);
                                               }"),
               tags$head(tags$style("#b2 {font-size:33px;height:40px;margin-top: -22px;}"),
                         tags$style("#A2 {font-size:33px;height:40px;margin-top: -22px;}"),
                         tags$style("#Linf2 {font-size:33px;height:40px;margin-top: -22px;}"),
                         tags$style("#uprop2 {font-size:33px;height:40px;margin-top: -22px;}")),
               
               checkboxGroupInput("b2",strong("Weight-Length:",style = "font-family: 'times'; font-si6pt"), 
                                  c("High"=4,"Medium"=3,"Low"=2,"Fixed"=1), selected=1)),
               column(3,
                      div(style="height:25px;",
                          #textInput("bprophigh",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"), value = 33),
                          sliderInput("bprophigh2",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F,round=T),
                          #tags$head(tags$style(type="text/css", "#bprophigh {height: 10px; font-size: 12px;}")),
                          #textInput("bpropmid",label= NULL, value = 33),
                          sliderInput("bpropmid2",label= NULL,min = 0,max = 100,value = 33,ticks = F,round=T),
                          #tags$head(tags$style(type="text/css", "#bpropmid {height: 20px; font-size: 12px;}")),
                          #textInput("bproplow",label= NULL, value = 33),
                          sliderInput("bproplow2",label= NULL,min = 0,max = 100,value = 33,ticks = F,round=T),
                          textInput("bfixed2",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                          tags$head(tags$style(type="text/css", "#bfixed2 {height: 20px; font-size: 12px;}"))),
                      br()
               ),
               column(3,
                      checkboxGroupInput("A2", strong("Annual Mortality:",style = "font-family: 'times'; font-si6pt"),
                                         c("High"=4,"Medium"=3,"Low"=2,"Fixed"=1), selected=1)),
               column(3,
                      div(style="height:25px;",
                          sliderInput("Aprophigh2",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F),
                          sliderInput("Apropmid2",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                          sliderInput("Aproplow2",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                          textInput("Afixed2",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                          tags$head(tags$style(type="text/css", "#Afixed2 {height: 20px; font-size: 12px;}"))),
                      br()
               )),
               br(),
               br(),
               br(),br(),br(),br(),br(),br(),br(),br(),
               fluidRow(   #VBGF and uprop Parameters
                 column(3,
                        checkboxGroupInput("Linf2",strong("Length-at-Age:",style = "font-family: 'times'; font-si6pt"),
                                           c("Fast"=4,"Medium"=3,"Slow"=2,"Fixed"=1), selected=1)),
                 column(3,
                        div(style="height:25px;",
                            sliderInput("Linfprophigh2",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("Linfpropmid2",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("Linfproplow2",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            textInput("Linffixed2",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                            tags$head(tags$style(type="text/css", "#Linffixed2 {height: 20px; font-size: 12px;}"))),
                        br()
                 ),
                 column(3,
                        checkboxGroupInput("uprop2", strong("Exploitation:",style = "font-family: 'times'; font-si6pt"),
                                           c("High"=4,"Medium"=3,"Low"=2,"Fixed"=1), selected=1)),
                 column(3,
                        div(style="height:25px;",
                            sliderInput("uprophigh2",label= strong("Proportion (%)",style = "font-family: 'times'; font-si6pt"),min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("upropmid2",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            sliderInput("uproplow2",label= NULL,min = 0,max = 100,value = 33,ticks = F),
                            textInput("upropfixed2",label = strong("Fixed Value:",style = "font-family: 'times'; font-si6pt"),value = ""),
                            tags$head(tags$style(type="text/css", "#upropfixed2 {height: 20px; font-size: 12px;}"))),
                        br()
                 )),
               fluidRow(
                 br(),br(),br()
               ),
               fluidRow(
                 br(),br(),br(),br(),br(),br(),br()
               ),
               fluidRow(
                 br(),br()
               )), #END TAB PANEL
                  tabPanel("Length Limits & Scores", 
                           fluidRow(
                             column(4,
                                    textInput("mll",
                                              label =strong("Length Limit (In.)"), value = "10,11,12"),
                                    textInput("quality",
                                              label =strong("Big Fish Length (In.)"), value="13"),
                                    br(),
                                    h4(strong("Utilities (All Total 100)",style="font-family: 'times'; font-si6pt")),
                                    checkboxGroupInput("Testattribchoices","Choices:",
                                                       c("Yield"=1,"Average Weight"=2,"Harvest Rate"=3,"Big Fish Harvest"=4,"Harvest Opp."=5,"Quality Harvest Opp."=6,"Growth Overfishing"=7)),
                                    br(),
                                    actionButton("llsimulate",strong("Run Simulation"))),
                             column(4,
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==1",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),#Shows panel if checkbox has checks
                                      sliderInput("testattrib1",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 100, step = 1)),
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==2",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),
                                      sliderInput("testattrib2",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 50, step = 1),
                                      sliderInput("testattrib3",
                                                  label=h6("attribchoice[2]"), min = 0, max = 100, value = 50, step = 1)),
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==3",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),
                                      sliderInput("testattrib4",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 33, step = 1),
                                      sliderInput("testattrib5",
                                                  label=h6("attribchoice[2]"), min = 0, max = 100, value = 33, step = 1),
                                      sliderInput("testattrib6",
                                                  label=h6("attribchoice[3]"), min = 0, max = 100, value = 33, step = 1)),
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==4",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),
                                      sliderInput("testattrib7",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 25, step = 1),
                                      sliderInput("testattrib8",
                                                  label=h6("attribchoice[2]"), min = 0, max = 100, value = 25, step = 1),
                                      sliderInput("testattrib9",
                                                  label=h6("attribchoice[3]"), min = 0, max = 100, value = 25, step = 1),
                                      sliderInput("testattrib10",
                                                  label=h6("attribchoice[4]"), min = 0, max = 100, value = 25, step = 1)),
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==5",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),
                                      sliderInput("testattrib11",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 20, step = 1),
                                      sliderInput("testattrib12",
                                                  label=h6("attribchoice[2]"), min = 0, max = 100, value = 20, step = 1),
                                      sliderInput("testattrib13",
                                                  label=h6("attribchoice[3]"), min = 0, max = 100, value = 20, step = 1),
                                      sliderInput("testattrib14",
                                                  label=h6("attribchoice[4]"), min = 0, max = 100, value = 20, step = 1),
                                      sliderInput("testattrib15",
                                                  label=h6("attribchoice[5]"), min = 0, max = 100, value = 20, step = 1)),
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==6",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),
                                      sliderInput("testattrib16",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 16, step = 1),
                                      sliderInput("testattrib17",
                                                  label=h6("attribchoice[2]"), min = 0, max = 100, value = 16, step = 1),
                                      sliderInput("testattrib18",
                                                  label=h6("attribchoice[3]"), min = 0, max = 100, value = 16, step = 1),
                                      sliderInput("testattrib19",
                                                  label=h6("attribchoice[4]"), min = 0, max = 100, value = 16, step = 1),
                                      sliderInput("testattrib20",
                                                  label=h6("attribchoice[5]"), min = 0, max = 100, value = 16, step = 1),
                                      sliderInput("testattrib21",
                                                  label=h6("attribchoice[6]"), min = 0, max = 100, value = 16, step = 1)),
                                    conditionalPanel(
                                      condition= "input.Testattribchoices.length==7",
                                      h4(strong("Utility Weights",style="font-family: 'times'; font-si6pt")),
                                      sliderInput("testattrib22",
                                                  label=h6("attribchoice[1]"), min = 0, max = 100, value = 14, step = 1),
                                      sliderInput("testattrib23",
                                                  label=h6("attribchoice[2]"), min = 0, max = 100, value = 14, step = 1),
                                      sliderInput("testattrib24",
                                                  label=h6("attribchoice[3]"), min = 0, max = 100, value = 14, step = 1),
                                      sliderInput("testattrib25",
                                                  label=h6("attribchoice[4]"), min = 0, max = 100, value = 14, step = 1),
                                      sliderInput("testattrib26",
                                                  label=h6("attribchoice[5]"), min = 0, max = 100, value = 14, step = 1),
                                      sliderInput("testattrib27",
                                                  label=h6("attribchoice[6]"), min = 0, max = 100, value = 14, step = 1),
                                      sliderInput("testattrib28",
                                                  label=h6("attribchoice[7]"), min = 0, max = 100, value = 14, step = 1))
                             ),
                             column(4,
                           br(),
                           #tableOutput("ScoreLL"),
                           br(),
                           tableOutput("AWtest")))
                           )#END TAB PANEL
                  )#END TABSET PANEL
      )# END MAIN PANEL
      )#END SIDEBAR LAYOUT
), #END TAB PANEL
tabPanel("Simulation Results",
         mainPanel("Plot",
                   br(),
                   tableOutput("ScoreLL"),
                   br(),
         fluidRow(
           column(4,
                  radioButtons("datt","Data Type:",
                               c("Yield" = "Yield",
                                 "Average Wt." = "AverageWt",
                                 "Harvest Rate" = "HarvestRate",
                                 "Quality Harvest" = "QualityHarvest"))),
           # column(4,
           #        textOutput("llinput"))
           column(4,
                  radioButtons("llselect","Length Limit (inches):",
                               choices = c("llinput()[[1]]",
                                           "llinput()[[2]]","llinput()[[3]]",
                                           "showall"= "Show All")))
         ),
         br(),  
         textOutput("text1"),
         plotOutput("plot", height = 350),
         br()
)#END MAIN PANEL
),#END TAB PANEL
tabPanel("Raw Output",
         mainPanel("Table",
                   br(),
                   tableOutput("Sim_2"),
                   downloadButton('download_table2','Download')
                   )#END MAIN PANEL
         )#END TAB PANEL
  )#navbarpage
)#fluidpage
)#ShinyUI
