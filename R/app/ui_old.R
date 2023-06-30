##Pedigree App##
# This is the UI logic of the Pedigree Shiny web application. You can run the
# application by clicking 'Run App' above.

## Libraries needed ------------------------------
library(shiny)
library(bootstrap)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  ## Configuration -------------------------------
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}
                    .navigationBar{background-color:#0001;height:350px}
                    #console{max-height: 300px;overflow-y:auto;}
                    #legendToPlot{max-height:150px};")),
    tags$script(
      'Shiny.addCustomMessageHandler("scrollCallback",
        function(color) {
          var objDiv = document.getElementById("console");
          objDiv.scrollTop = objDiv.scrollHeight;
        }
      );
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      '
    )
  ),
  ## Application title --------------------------
  fluidRow(
    column(12,align = "center",
           titlePanel("Pedigree creation")
           )
  ),

  ## Navigation bar -----------------------------
  fluidRow(title = "Navigation",class = "navigationBar",
    ## ___Data and Family selection -------------
    column(2,
      fileInput("pedData",h5(strong("Choose a file")),accept = c(".csv",".xls",".xlsx")),
      uiOutput("sheetSelection"),
      uiOutput("familySelection")
    ),
    ## ___Health selection ----------------------
    column(3,
      h5(strong("Health options")),
      uiOutput("healthVariable"),
      uiOutput("healthOptionsFullScale"),
      uiOutput("healthOptionsNumToFact"),
      uiOutput("affectedSelection")
    ),
    ## ___Informative individuals selection -----------
    column(2,
           selectInput("infSelected", label = h5(strong("Select informative individuals")),
                        choices = list("All individuals" = "All","Available or Affected" = "Av/Af", "Available only" = "Av",
                                       "Affected only" = "Af","Available and Affected" = "AvAf","Custom" = "Cust"),
                       selected = "All"),
           uiOutput("customVariable"),
           uiOutput("customSelection")
           ),
    ## ___Filter to use ------------------------
    column(2,
      numericInput("gen_max",label = h5(strong("Number of generation max")),value = 3, min=0),
      numericInput("kinDeg",label = h5(strong("Max kinship")),value = 4, min=1)
    ),
    ## ___Trim and generate --------------------
    column(3,
      h5(strong("Other options :")),
      checkboxInput("trimPed",label = "Trim non informative parents",value = T),
      checkboxInput("keepInfos",label = "When trimmed, keep individuals with infos",value = T),
      checkboxInput("pedInteractive",label = "Make the pedigree interactive",value = F),
      actionButton("generate",label = h5(strong("Generate Pedigree")))
    )
  ),
  tags$br(),
  
  tags$hr(),
  ## Errors found ----------------------------------------------
  fluidRow(align = "center",
           column(4,align = "center",uiOutput("DownloadAllDataButton")),
           column(4,align = "center",uiOutput("ErrorsText")),
           column(4,align = "center",uiOutput("DownloadErrorsButton"))
  ),
  tags$hr(),
  
  ## Family informations --------------------------------------
  fluidRow(
    column(5,align = "center",
           uiOutput("familiesVariable"),
           tableOutput("familiesTable")),
    column(7,align = "center",
           textOutput("textFamilySelectedInfos"),
           DT::dataTableOutput("tableFamilySelectedInfos"))
  ),
  
  ## Sub-family informations ---------------------------------
  conditionalPanel(
    condition = "output.showSubFamily==true",
    tags$hr(),
    
    fluidRow(
      column(5,align = "center",
             uiOutput("SubFamilySelection"),
             uiOutput("DownloadDataButton")),
      column(7,align = "center",
             tableOutput("SubFamilyPresent"))
    )
  ),
  tags$hr(),
  
  ## Graph and legend panel ----------------------------------
  conditionalPanel(
    condition = "output.showPlot==true",
    fluidRow(
      column(12,align = "center",
             textOutput("titlePlot")
             )
    ),
    # Show a plot of the generated pedigree
    fluidRow(
      shinycssloaders::withSpinner(uiOutput("pedPlot")),
    ),
    fluidRow(
      div(
        column(6,align = "center",
              plotOutput("legendToPlot")),
        column(3,align = "center",
              sliderInput("cexPlot", label = h4("Texte size"), min = 0.2, 
                          max = 2, value = 1, step = 0.1),
              textOutput("paramPlot")),
        column(3,align = "center",
               uiOutput("DownloadPlotButton")),
        style = "align: center; height: 200"
      )
    )
  ),
  tags$hr(),
  #verbatimTextOutput("dimension_display"),
  
  ## Console ------------------------------------------------
  fluidRow(
    pre(id = "console")
  )
))
