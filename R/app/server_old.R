##Pedigree App##
# This is the server logic of the Pedigree Shiny web application. You can run the
# application by clicking 'Run App' above.

## Setup ----------------------------------
# Library needed
usethis::use_package("shiny")
usethis::use_package("shinyjs")
usethis::use_package("DT")

# Directory and files
work_dir <- getwd()
#try(dev.off(), silent = T)

# Function and sketch
sketch <- htmltools::withTags({
  table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2,"Variable used"),
        th(class = 'dt-center',colspan = 3, "Availability")
      ),
      tr(
        th("Ind not Avail"),
        th("Ind Avail")
      )
    )
  )
})


# Define server logic required to draw a histogram
## Shiny server ----------------------------
shinyServer(function(input, output, session) {

  ## Data selection ------------------------
  selectedInfInds <- reactive({
    infCustValues <- input$InfCustValues
    if (input$infSelected != "Cust") {
      infInds = input$infSelected
      
    }else if (!identical(infCustValues,"")) {
      print("Custom selection")
      infCustVariable <- input$InfCustVariable
      infFamilyDependent = infCustVariable %in% c("dadid","momid","id")
      
      df = readData()[[1]]
      if (infFamilyDependent) {
        df = selectFamily(readData()[[1]],as.numeric(input$familySelected))
      }
      
      infCustValues <- unlist(strsplit(infCustValues,","))
      index = match(infCustValues, df[,infCustVariable])
      if (!all(!is.na(index))) {
        if (infFamilyDependent) {
          familyText = paste("inside family",input$familySelected)
        }else{
          familyText = ""
        }
        message(paste("Values",infCustValues[is.na(index)],"not present in",infCustVariable,familyText))
      }
      if (all(is.na(index))) {
        showNotification("No individuals value given present in variable")
        infInds = NA
      }else{
        infInds = df$id[index[!is.na(index)]]
      }
      
    }else{
      showNotification("Custom option selected but no individual id given")
      infInds = NA
    }
    return(infInds)
  })
  getAffectedInds <- reactive({
    return(generate_aff_inds(readData()[[1]], input$HealthCustVariable,
                                input$HealthAffectedMod, input$HealthThreshold,
                                input$AffSupToThreshold, input$KeepFullScale,
                                breaks = 4))
  })
  selectIndsToPlot <- reactive({
    infCustVariable <- input$InfCustVariable
    if (input$infSelected != "Cust") {
      infFamilyDependent = TRUE
    }else{
      infFamilyDependent = infCustVariable %in% c("dadid","momid","id")
    }
    
    LogF = getAffectedInds()[[1]]
    if (infFamilyDependent) {
      LogF = selectFamily(LogF,as.numeric(input$familySelected))
    }
    
    infInds <- selectedInfInds()
    if (!is.null(LogF) & !any(is.na(infInds))) {
      print(paste0("Informative individuals selected:",paste(infInds)))
      print("")
      print("Select data from informative individuals")
      LogFL <- selectAllFromInfInds(LogF,infInds = infInds,
                                    gen_max = input$gen_max,KinDeg = input$kinDeg)
      if (input$trimPed) {
        print("")
        print("Trim data")
        return(trimPed(LogFL,input$keepInfos))
      }else{
        return(LogFL)
      }
    }else{
      return(NULL)
    }
  })
  getSubFamily <- function() {
    isolate({
      LogFL <- selectIndsToPlot()
      if (is.null(LogFL)) {
        return(NULL)
      }else{
        SubFam <- as.data.frame(table(LogFL$family))
        colnames(SubFam) <- c("Sub-family","Nb Ind" )
        SubFam$`Sub-family` <- as.character(SubFam$`Sub-family`)
        if (0 %in% SubFam$`Sub-family`) {
          SubFam$Infos[SubFam$`Sub-family` == "0"] <- "Connected to none"
        }
        return(SubFam)
      }
    })
  }
  
  ## Navigation panel ---------------------
  
  #Informative individuals custom selection
  output$customVariable <- renderUI({
    if (input$infSelected == "Cust") {
      col_allowed = c("IndID","Name","Gender","Availability",
                      "BirthDate","FatherID","MotherID",
                      "id","dadid","momid")
      col_present = colnames(readData()[[1]])
      col_selection = intersect(col_present,col_allowed)
      selectInput("InfCustVariable", label = "Select Variable to use to select informative individuals",
                  choices = as.list(setNames(col_selection,col_selection)))
    }else{
      return()
    }
  })
  output$customSelection <- renderUI({
    if (input$infSelected == "Cust") {
      textAreaInput("InfCustValues", label = h5("Custom selection"),
                    placeholder = "Please enter individuals values separate by a comma")
    }else{
      return()
    }
  })
  
  ## Data representation ------------------
  output$tableFamilySelectedInfos <- DT::renderDataTable({
    if (is.null(input$HealthCustVariable) | is.null(input$familySelected)) {
      return(NULL)
    }
    df = readData()[[1]]
    familyInfos <- getFamilyInfos(df[df$family == input$familySelected,],
                                  input$HealthCustVariable, input$HealthThreshold)
    if (is.null(familyInfos)) {return(NULL)}
    tabInfos <- xtabs(familyInfos[["Freq"]]~familyInfos[[input$HealthCustVariable]]+
                        familyInfos[["Availability"]], addNA = T)
    tabInfos <- as.data.frame.matrix(tabInfos,optional = T)
    rownames(tabInfos) <- replace(rownames(tabInfos),rownames(tabInfos) == "","NA")
    datatable(tabInfos,container = sketch,
              options = list(
                columnDefs = list(
                  list(targets = "_all", className = "dt-center"))
                ,dom = "t")
    )
  })
  output$textFamilySelectedInfos <- renderText({
    if (length(input$familySelected) != 0) {
      familiesTable <- getFamiliesTable()
      varSelected = familiesTable$'Major mod'[familiesTable$FamilyNum == input$familySelected]
      paste("Data representation for family",varSelected,input$familySelected)
    }else{
      NULL
    }
  })
  
  ## General Infos -----------------------
  # Errors in data
  output$DownloadErrorsButton <- renderUI({
    if (is.null(readData()[[2]])) {
      output$ErrorsText <- renderText({NULL})
      return()
    } else {
      nb_errors = nrow(readData()[[2]])
      output$ErrorsText <- renderUI({
        HTML(paste(nb_errors," errors has been found in the data <br/>
        (Data can only be export to Download folder)",sep = ' '))})
      
      output$ErrorsDownload <- downloadHandler(filename = function() {
        paste("Errors_", Sys.Date(), ".csv", sep = "")
      },
        content = function(file) {
        write.csv2(readData()[[2]], file)
      })
      downloadButton("ErrorsDownload","Download errors as .csv")
    }
  })
  # All families infos
  output$DownloadAllDataButton <- renderUI({
    if (is.null(readData()[[1]])) {
      return()
    }else{
      fileName = str_remove(input$pedData$name,"\\.\\S*")
      title = paste("AllData_",fileName,"_",Sys.Date(), ".csv", sep = "")
      output$AllDataDownload <- downloadHandler(filename = title,
                                                content = function(file) {write.csv2(readData()[[1]], file)})
      downloadButton("AllDataDownload","Download data processed as .csv")
    }
  })
  
  ## Isolated function ------------------
  getPedData <- reactive({
    LogFL <- v$LogFL[v$LogFL$family == input$subFamilySelected,]
    if ((!is.null(LogFL))) {
      if (nrow(LogFL) > 2 & nrow(LogFL) < 400) {
        return(getPed(LogFL))
      }else{
        return()
      }
    }else{
      return()
    }
  })
  getPlotPedigree <- reactive({
    LogFL <- v$LogFL[v$LogFL$family == input$subFamilySelected,]
    if ((!is.null(LogFL))) {
      if (nrow(LogFL) > 2 & nrow(LogFL) < 400) {
        plot <- print_console(plotPed(LogFL, input$cexPlot, mar = c(1.5,1,3,1),
                                       psize = c(input$dimension[1]/100,length(getPedData()))),
                               session)
        return(plot)
      }else{
        return()
      }
    }else{
      return()
    }
  })
  plotlyPedigree <- function() {
    isolate({
      LogFL <- v$LogFL[v$LogFL$family == input$subFamilySelected,]
      if ((!is.null(LogFL))) {
        if (nrow(LogFL) > 2 & nrow(LogFL) < 75) {
          pedPlotly <- print_console(plotPed(LogFL, input$cexPlot, mar = c(1.5,1,3,1),
                                              psize = c(input$dimension[1]/100,length(getPedData())),
                                              ggplotGen=T),
                                      session)
          return(pedPlotly)
        }else{
          return()
        }
      }else{
        return()
      }
    })
  }
  addTitle <- function() {
    isolate({
      LogFL <- selectIndsToPlot()
      LogFL <- LogFL[LogFL$family == input$subFamilySelected,]
      interPlot <- input$pedInteractive
      if (input$subFamilySelected == "0") {
        title = "This sub-family contains the individuals not linked to any"
      }else if ((!is.null(LogFL))) {
        if (nrow(LogFL) > 2) {
          if ((!interPlot & nrow(LogFL) < 400) | (interPlot & nrow(LogFL) < 75)) {
            familiesTable <- getFamiliesTable()
            if (input$infSelected == "Cust") {
              infSelected = paste(input$InfCustVariable," (id:",paste(selectedInfInds(),collapse = ','),")")
            }else{
              infSelected = selectedInfInds()
            }
            if (input$trimPed) {
              trimText = "trimmed"
            }else{
              trimText = ""
            }
            title = paste0(c("Pedigree ",trimText,"of ",familiesTable$'Major mod'[familiesTable$FamilyNum == input$familySelected],
                             " family N\u00B0",input$familySelected,
                             " sub-family N\u00B0",input$subFamilySelected,
                             " ( N=",nrow(LogFL),") from ",infSelected," individuals"))
          }else{
            title = paste("Too much individuals to plot, please select fewer informative individuals or trim the pedigree \n
            limit 400 for normal plot and 75 for interactive plot (",nrow(LogFL)," selected)")
          }
        }else{
          title = "Not enough individuals in this family"
        }
      }else{
        title = "No data selected"
      }
      return(title)
    })
  }
  getFileName <- function(){
    isolate({
      familiesTable <- getFamiliesTable()
      if (input$infSelected == "Cust") {
        infSelected = paste(input$InfCustVariable,"(id ",paste(selectedInfInds(),collapse = ','),")")
      }else{infSelected = selectedInfInds()}
      
      if (input$trimPed) {
        trimText = "10"
        if (input$keepInfos) {
          trimText = "11"
        }
      }else{trimText = "00"}
      
      title = paste0(c("Ped",familiesTable$'Major mod'[familiesTable$FamilyNum == input$familySelected],
                       "_G",input$gen_max,"-K",input$kinDeg,"-T",trimText,
                       "-I", infSelected,
                       "_SF",input$subFamilySelected), collapse = "")
      title = str_replace(title,"/","-")
      title = str_replace(title," ","-")
      return(title)
    })
  }

  ## Sub Family infos ---------------------
  output$DownloadDataButton <- renderUI({
    if (v$doPrepData == FALSE) {
      return()
    }
    
    if (is.null(input$subFamilySelected)) {
      LogFL <- v$LogFL
    }else{
      LogFL <- v$LogFL[v$LogFL$family == input$subFamilySelected,]
    }

    if (is.null(LogFL)) {
      print("No data selected")
      return()
    }
    if (nrow(LogFL) == 0) {
      print(paste("No individual in sub-family", input$subFamilySelected))
      return()
    }

    title = paste0(c(getFileName(), ".csv"), collapse = "")
    
    output$DataDownload <- downloadHandler(filename = title,
                                           content = function(file) {write.csv2(LogFL, file)})
    downloadButton("DataDownload","Download sub-family data as .csv")
  })
  output$SubFamilyPresent <- renderTable({
    if (v$doPrepData == FALSE) {return()}
    isolate({
      SubFam <- getSubFamily()
      return(SubFam)
    })
  })
  output$SubFamilySelection <- renderUI({
    if (v$doPrepData == FALSE) {return()}
    isolate({
      SubFam <- getSubFamily()
      if (!is.null(SubFam)) {
        subFamNum = as.numeric(SubFam$`Sub-family`)
        if (max(subFamNum) > 0) {
          return(numericInput("subFamilySelected", label = "Select sub-family to plot",
                              value = 1, min = min(subFamNum), max = max(subFamNum), width = "50%"))
        }
      }
      return(h5(strong("No sub family to plot")))
    })
  })

  ## Conditional rendering ----------------
  v <- reactiveValues(doPlot = FALSE,doPrepData = FALSE,
                      showPlot = FALSE,showSubFamily = FALSE)
  
  observeEvent({
    input$pedData
    input$familySelected
    input$infSelected
    input$gen_max
    input$kinMax
    input$trimPed
    input$keepInfos
    input$pedInteractive},{
    v$showSubFamily <- FALSE
    v$showPlot <- FALSE
    v$doPlot <- FALSE
    v$doPrepData <- FALSE
  })
  observeEvent(input$generate,{
    v$doPrepData <- input$generate
    v$showSubFamily <- TRUE
    v$showPlot <- TRUE
    v$LogFL <- print_console(selectIndsToPlot(),session)
  })
  observeEvent({
    input$subFamilySelected
    input$generate},{
    print(paste("Sub-family Selected",input$subFamilySelected))
    v$doPlot <- FALSE
    v$doPlot <- input$subFamilySelected
  })
  observeEvent({input$cexPlot},
               {v$doPlot <- FALSE
               v$doPlot <- input$subFamilySelected})
  
  #Output for conditional rendering
  output$showSubFamily <- reactive({
    print(v$showSubFamily)
    return(v$showSubFamily)
  })
  output$showPlot <- reactive({
    print(v$showPlot)
    return(v$showPlot)
  })
  outputOptions(output, 'showSubFamily', suspendWhenHidden = FALSE)
  outputOptions(output, 'showPlot', suspendWhenHidden = FALSE)

  ## Graph and legend -------------------
  output$pedPlot <- renderUI({
    if (is.null(v$doPlot)) {
      print("Do plot is null")
      return()
    }else if (v$doPlot == FALSE) {
      print("Do plot is false")
      return()
    }else if (input$subFamilySelected == "0") {
      print("No link present")
      return()
    }
    isolate({
      if (input$pedInteractive) {
        output$Plotly_1 <- renderPlotly({plotlyPedigree()})
        return(plotlyOutput("Plotly_1"))
      }else{
        message(getPedData())
        output$Plot_1 <- renderPlot({getPlotPedigree()},height = function(){length(getPedData())*100})
        return(plotOutput("Plot_1",height = length(getPedData())*100))
      }
    })
  })
  output$titlePlot <- renderText({
    if (is.null(v$doPlot)) {return()}
    else if (v$doPlot == FALSE) {return()}
    isolate({
      addTitle()
    })
  })
  output$legendToPlot <- renderPlot({
    if (is.null(v$doPlot)) {return()}
    else if (v$doPlot == FALSE) {return()}
    isolate({
      legend = createLegend(getAffectedInds()[[2]], size = 0.8)
      grid.arrange(legend$A, legend$B,ncol = 2)
      }
    )}, height = 150)
  output$paramPlot <- renderText({
    plot = getPlotPedigree()
    if (length(plot) != 0) {
      widthPlot = round(max(plot$x) + 1,2)
      heightPlot = max(plot$y)
      return(paste("Width:", widthPlot, "/ Height:", heightPlot))
    }
  })
  output$DownloadPlotButton <- renderUI({
    if (v$doPrepData == FALSE) {return()}
    LogFL <- v$LogFL[v$LogFL$family == input$subFamilySelected,]
    if (is.null(LogFL)) {
      print("No data selected")
      return()
    }
    if (nrow(LogFL) == 0) {
      print(paste("No individual in sub-family", input$subFamilySelected))
      return()
    }
    
    output$PlotDownload <- downloadHandler(filename = paste0(c(getFileName(), ".pdf"), collapse = ""),
                                           content = function(file) {
                                             tryCatch({
                                               ped <- with(LogFL,pedigree(id,dadid,momid,sex))
                                               n = align.pedigree(ped)$n
                                               fig_hw = c(max(n), length(n) + 2)
                                               pdf(file, width = max(fig_hw[1],8), height = fig_hw[2],
                                                   onefile = F)
                                               layout(matrix(c(rep(1,length(n)),2), ncol = 1))
                                               vp.plot <- viewport(height = fig_hw[2] - 2.5, width = fig_hw[1], 
                                                                   just = c("center"),default.units = "in", 
                                                                   y = fig_hw[2]/2, x = fig_hw[1]/2)
                                               pushViewport(vp.plot)
                                               plotPed(LogFL, input$cexPlot, mar = c(2,0.3,3,0.3))
                                               mtext(getFileName(), line = 2, cex = 1.5)
                                               vp.legend <- viewport(height = 1, width = 6, 
                                                                     just = c("center", "center"),default.units = "in", 
                                                                     y = -0.72, x = fig_hw[1]/2)

                                               legend = createLegend(getAffectedInds()[[2]], nbColMany = 4, size = 0.65)
                                               grid.draw(arrangeGrob(legend$A, legend$B, ncol = 2, widths = c(6,1), vp = vp.legend))
                                             },
                                             finally = {
                                               while (TRUE) {
                                                 if (names(dev.off()) == "null device") {
                                                   break
                                                 }
                                               }
                                             })
                                           })
    downloadButton("PlotDownload","Download plot as .pdf")
  })
  
  ## Dimension --------------------------
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  })
  ## End --------------------------------
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
})


