source("extRa/libraries.R")
source("extRa/datImport.R")

shinyUI(navbarPage(HTML("<img src='harvard-logo.png'/>"),
                   
##########
# Plot
##########

tabPanel("Visualize",
fluidPage(
    fluidRow(
    headerPanel(
        HTML("<h1><b><P ALIGN=Center>Welcome to the scHemer webapp</b></h1>")),
    shiny::tags$br(),shiny::tags$br(), shiny::tags$br(), HTML("<h4><b><P ALIGN=Center>Buenrostro Lab</b></h4>"),
    shiny::tags$br(),shiny::tags$br(),
    bsCollapse(id = "collapseAdvancedPlotOptions", open = c("Panel1"), multiple = TRUE,
    bsCollapsePanel(title = HTML("<h4><b>Visualization Options</b></h4>"), value = "Panel1",
    fluidRow(
        column(1, shiny::tags$br()),
        column(6,
        selectInput("colorVisPoints", "Specify Color Logic", selected = "Cell", selectize = TRUE,
            choices = list("Annotation" = c("Cell", "Cluster", "Transcription Factor"))),
        shiny::tags$br(),
        conditionalPanel('input.colorVisPoints != "Cell" && input.colorVisPoints != "Cluster"', 
            uiOutput("tfname"),
            textOutput("tfvarianceval"),
            checkboxInput("sortVar", "Sort TFs by variance?", value = TRUE, width = NULL))
        ),
        column(5,
        conditionalPanel('input.colorVisPoints != "Cell" && input.colorVisPoints != "Cluster"', 
            selectInput("contColorTheme", "Specify Color Theme", selected = "Spectral", 
                choices = list("Sequential" = c("Blues", "YlOrRd", "YlGnBu"),
                "Diverging" = c("Spectral", "PuOr", "RdGy", "PiYG"))), shiny::tags$br(),
            uiOutput("minMaxColor")
        ))
    ))
    ), 
    mainPanel(
        plotlyOutput("plotgraph1", height = "1000", width = "150%")
    )), shiny::tags$br(),shiny::tags$br(),
    conditionalPanel('input.colorVisPoints != "Cell" && input.colorVisPoints != "Cluster"', 
        bsCollapse(id = "tfMotif", open = c("Panel2"), multiple = TRUE,
        bsCollapsePanel(title = HTML("<h4><b>Transcription Factor Motif</b></h4>"), value = "Panel2",
        fluidRow(plotOutput("TFplot")))
    ))
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