library(shiny)
library(ggplot2)
library(plotly)
library(ggpubr)
library(plotly)
library(tidyverse)
library(dplyr)
load(url("https://github.com/parkerfranzen/ol_line_combine_project/blob/main/OLCombine.RData?raw=true"))
all_players <- subset(all_players, select = -c(id))
all_players <- all_players[, c(1,9,2,3,4,5,6,7,8,10)]
all_players <- all_players %>% dplyr::rename(Position = 3, Vertical_Jump = 5, Bench_Press = 6, Three_Cone_Drill = 8)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel(
        h1("Offensive Linemen Longevity and the NFL Scouting Combine", align = "center"),
    ),
    HTML('<hr style="color: black;">'),
    p("Parker Franzen | May 2021", align = "right"),
    h4("This shiny application takes every Offensive Linemen's NFL Scouting Combine results from 2000 - 2017 and determines if there is a correlation between drill results and years played in the NFL."),
    h5("- The average NFL career is 3.3 years according to the NFL Players' Association, so this shiny application looks at players drafted from 2000 to 4 years ago from when this application was made."),
    h5("- 964 players in the data."),
    h5("- Data was web-scraped from http://www.pro-football-reference.com/."),
    p(em("- The NFL Scouting Combine is an annual event lasting a week in February where college football players perform physical tests in front of National Football League coaches, general managers, and scouts.")),
    
    sidebarLayout(
        
        sidebarPanel(
            p("- Select a drill for the Y-Axis to view the correlation of the Offensive Linemen's drill results and their years played in the NFL."),
            selectInput(inputId = "y",
                        label = "Drill:",
                        choices = c("40_Yard_Dash", "Vertical_Jump", "Bench_Press", "Broad_Jump", "Three_Cone_Drill", "Shuttle"),
                        selected = "40_Yard_Dash"),
            p("*Hover over individual points on the graph to view the player's drill score, years played, and name."),
            p("*Graphs only players who participated in the selected drill."),
            p(strong("Drill descriptions:"), align = "center"),
            p(strong("40 Yard Dash"), " - Players sprint 40 yards. Measured in seconds."),
            p(strong("Vertical Jump"), " - From a standing position, players jump as high as they can. Measured in inches."),
            p(strong("Bench Press"), " - Players bench press 225 pounds as many times as possible. Measured in number of repetitions."),
            p(strong("Broad Jump"), " - From a standing position, players jump as far as they can. Measured in inches."),
            p(strong("Three Cone Drill"), " - Also known as the L-Drill. Three cones are placed five yards apart from each other forming a right angle. 
                                            Players start with one hand down on the ground and run to the middle cone and touch it. 
                                            Players then reverse direction back to the starting cone and touch it. 
                                            Players reverse direction again but this time run around the outside of the middle cone on the way to the far cone running around it in figure eight fashion on their way back around the outside of the middle cornering cone. 
                                            Measured in seconds."),
            p(strong("Shuttle"), " - 20-yard suttle. Players start with one hand down on the ground and then run five yards to their right, touch the ground, reverse and run back 10 yards, touch the ground, before heading back five yards to the finish line. Measured in seconds."), 
            
            
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Correlation", 
                         plotlyOutput(outputId = "plot"), 
                         HTML('<hr style="color: black;">'),
                         p(strong("KEY"), align = "center"),
                         p("-", em(strong("R")), ": ", "The correlation coefficient measures how strong a relationship is between the selected drill and years played."),
                         p("Ranges from -1 to 1."),
                         img(src = "correlation.png", align = "center", height = 150, width = 725),
                         br(),
                         br(),
                         p("-", em(strong("p")), ":", "The p-value represents the probability that the correlation between the selected drill and years played occurred by chance. For example a p-value of 0.05 means there is a 5% chance the data occurred by chance."),
                         p("Ranges from 0 to 1."),
                         HTML('<hr style="color: black;">'),
                         p(strong("Results"), align = "center"),
                         p("There is a visual correlation in all six graphs, however mathematically all but one graph has negligible correlation with the 40 yard dash only having low correlation (-0.32). Every graphs p-value is very low making all correlations significant."),
                         p(strong("The data is broken down by the 75th percentile in each drill by NFL Scouting Combine year in the next panel.")),
                ),
                
                tabPanel("75th Pecentile in Drills",
                         h3(strong("This panel looks at how career lengths of players who scored in the 75th percentile in a certain drill during their respective year at the NFL Scouting Combine, compare to those who did not.")),
                         HTML('<hr style="color: black;">'),
                         h4("Calculation:"),
                         p("- For every NFL Scouting Combine year, the 75th percentile value in each drill is calculated."),
                         img(src = "75values.png", height = 450, width = 800, align = "center"),
                         br(),
                         br(),
                         p("- That value is compared to every player's result in that specific drill."),
                         p("- A player is then grouped into one of two groups for that drill:", em(strong("At or Above 75th Percentile")), " or ", em(strong("Below 75th Percentile."))),
                         HTML('<hr style="color: black;">'),
                         h3(strong("Results"), align = "center"),
                         img(src = "75years.png", height = 200, width = 800, align = "center"),
                         br(),
                         br(),
                         p(strong("When the 75th percentile value for each drill is used some remarkable data is produced.")),
                         p("- Offensive linemen who score in the top 75th percentile in every drill besides Bench Press play nearly", strong("2 years longer"), "than those who do not on average."),
                         p("- The difference in the Bench Press isn't even 1 year."),
                         h4("Remarks:"),
                         p("As a former collegiate offensive linemen I understand explosive hips are crucial for an offensive linemen's success and this data backs it up. Every drill besides Bench Press requires explosive hips in order to score well."),
                ),
                
                tabPanel("Data", DT::dataTableOutput(outputId="datasheet"), 
                         p(em("*OT - Offensive Tackle")),
                         p(em("*OG - Offensive Guard")),
                         p(em("*C - Center")),
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    #Graph in Correlation panel
    output$plot <- renderPlotly({
        
        ggplot(all_players, aes(x = Years_Played, y = .data[[input$y]], group = 1, text = paste("Player: ", Player))) + geom_point(size = 1) + geom_smooth(method=lm) + stat_cor(method = "pearson", label.x.npc = 0.84, label.y.npc = 1, r.accuracy = 0.01, p.accuracy = 0.001, output.type = "text")
        
    })
    
    #Raw data in Data panel
    output$datasheet<-DT::renderDataTable({
        DT::datatable(data=all_players[,1:10],
                      options=list(pageLength= 20),
                      rownames=FALSE)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)