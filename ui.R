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
        HTML("<h1><b><P ALIGN=Center>Single cell ATAC heme Interactive Visualization</b></h1>")),
    shiny::tags$br(),shiny::tags$br(), shiny::tags$br(), HTML("<h4><b><P ALIGN=Center>Buenrostro Lab</b></h4>"),
    shiny::tags$br(),shiny::tags$br(),
    bsCollapse(id = "collapseAdvancedPlotOptions", open = c("Panel1"), multiple = TRUE,
    bsCollapsePanel(title = HTML("<h4><b>Visualization Options</b></h4>"), value = "Panel1",
    fluidRow(
        column(4, 
        radioButtons("dimPlot", "Dimension:", choices = list("2D" = "2D", "3D" = "3D"), selected = "3D"),
        selectInput("xaxisVal", "X-Axis", selected = "PC1",  choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")), 
        selectInput("yaxisVal", "Y-Axis", selected = "PC2",  choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")), 
        conditionalPanel('input.dimPlot == "3D"', 
            selectInput("zaxisVal", "Z-Axis", selected = "PC3",  choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"))
        )),
        column(4,
        selectInput("colorVisPoints", "Specify Annotation", selected = "Cell", selectize = TRUE,
            choices = list("Annotation" = c("Cell", "Cluster",
                            "Transcription Factor Score", "Transcription Factor Cluster"))),
        shiny::tags$br(),
        conditionalPanel('input.colorVisPoints != "Cell" && input.colorVisPoints != "Cluster"', 
            conditionalPanel('input.colorVisPoints == "Transcription Factor Score"', 
            uiOutput("tfname"),
            checkboxInput("sortVar", "Sort TFs by variance?", value = TRUE, width = NULL)),
            conditionalPanel('input.colorVisPoints == "Transcription Factor Cluster"', 
                sliderInput("groupCor", "Minimum TF Correlation", 
                    min = 0.2, max = 0.99, value = 0.8, step = 0.01),
                uiOutput("tfpossiblegroups"),
                uiOutput("groupTFopts")
                ),
            textOutput("tfvarianceval")
        )
        ),
        column(4,
        conditionalPanel('input.colorVisPoints != "Cell" && input.colorVisPoints != "Cluster"', 
            selectInput("contColorTheme", "Specify Color Theme", selected = "Spectral", 
                choices = list("Sequential" = c("Blues", "YlOrRd", "YlGnBu"),
                "Diverging" = c("Spectral", "PuOr", "RdGy", "PiYG"))), shiny::tags$br(),
            uiOutput("minMaxColor")
        ))
    ))
    ), 
    bsCollapse(id = "plotOut", open = c("PlotGraph"), multiple = TRUE,
        bsCollapsePanel(title = HTML("<h4><b>Interactive Plot</b></h4>"), value = "PlotGraph",
        fluidRow(plotlyOutput("plotgraph1", height = "800", width = "100%"))))
    ), shiny::tags$br(),shiny::tags$br(),
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
footer = HTML(paste0('<P ALIGN=Center>scHemer &copy; Buenrostro Lab. <A HREF="mailto:caleblareau@g.harvard.edu">Contact</A>')),
collapsible = TRUE, 
fluid = TRUE,
windowTitle = "scHemer"))