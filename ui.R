source("extRa/libraries.R")
source("extRa/datImport.R")

shinyUI(navbarPage(HTML("<img src='harvard-logo.png'/>"),
                   
##########
# Plot
##########

tabPanel("Visualize",
fluidPage(
    headerPanel(
        HTML("<h1><b><P ALIGN=Center>Welcome to the scHemer webapp</b></h1>")),
    tags$br(), tags$br(),tags$br(),HTML("<h4><b><P ALIGN=Center>Buenrostro Lab</b></h4>"),
    tags$br(),tags$br(),
    sidebarPanel(
        tags$h3(tags$b('Configure Visualization')), 
        tags$br(),
        selectInput("colorVisPoints", "Specify Color Type", selected = "Cluster", selectize = TRUE,
            choices = list("Cell Annotation" = c("Cell", "Cluster"),
                            "Transcription Factor Annotation" = tfshort)),
        conditionalPanel('input.colorVisPoints != "Cell" && input.colorVisPoints != "Cluster"', 
            uiOutput("minMaxColor"), 
            selectInput("contColorTheme", "Specify Color Theme", selected = "Spectral", 
                choices = list("Sequential" = c("Blues", "YlOrRd", "YlGnBu"),
                "Diverging" = c("Spectral", "PuOr", "RdGy", "PiYG"))))
        # verbatimTextOutput("brush")
    ),
    mainPanel(
        plotlyOutput("plotgraph1", height = 800),
        tags$br(), tags$br()
    )
)),                   
                                      
##########
# GUIDE
##########

tabPanel("Guide",
    includeMarkdown("www/guide.Rmd")
),

##########
# FOOTER
##########

theme = shinytheme("flatly"),
footer = HTML(paste0('<P ALIGN=Center>scHemer &copy; <A HREF="mailto:caleblareau@g.harvard.edu">Caleb Lareau</A>')),
collapsible = TRUE, 
fluid = TRUE,
windowTitle = "scHemer"))