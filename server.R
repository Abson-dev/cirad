#@utor: Aboubacar HEMA,aboubacarhema94@gmail.com
#@utor: Aboubacar HEMA,aboubacarhema94@gmail.com
#library needed
library(shiny)
library(sf) # classes and functions for vector data
library(raster)# classes and functions for raster data
library(ggplot2)
library(biomod2)
library(grid)
library(rhandsontable)
library(haven)
library(DT)
library(shinyBS)
library(data.table)
#data
sn0<-st_read("C:\\Users\\Hp\\Downloads\\SEN_adm\\SEN_adm0.shp")
sn1<-st_read("C:\\Users\\Hp\\Downloads\\SEN_adm\\SEN_adm1.shp")
sn2<-st_read("C:\\Users\\Hp\\Downloads\\SEN_adm\\SEN_adm2.shp")
sn3<-st_read("C:\\Users\\Hp\\Downloads\\SEN_adm\\SEN_adm3.shp")
sn4<-st_read("C:\\Users\\Hp\\Downloads\\SEN_adm\\SEN_adm4.shp")

# species occurrences
DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
                                    package="biomod2"), row.names = 1)


# Environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
myExpl = stack( system.file( "external/bioclim/current/bio3.grd", 
                             package="biomod2"),
                system.file( "external/bioclim/current/bio4.grd", 
                             package="biomod2"), 
                system.file( "external/bioclim/current/bio7.grd", 
                             package="biomod2"),  
                system.file( "external/bioclim/current/bio11.grd", 
                             package="biomod2"), 
                system.file( "external/bioclim/current/bio12.grd", 
                             package="biomod2"))


#functions
genObserver_menus <-
  function(pat="btn_results_", n=1, updateVal) {
    res <- paste0('observeEvent(input$',pat,n,', {
                  curid <- "',pat,n,'"
                  nn <- names(input)
                  nn <- nn[grep("',pat,'",nn)]
                  nn <- setdiff(nn, curid)
                  for (btnid in nn) {
                  updateButton(session, btnid, style="default")
                  }
                  obj$',updateVal,' <- "',pat,n,'"
                  updateButton(session, curid, style="primary")
  });
                  ')
    res
    }

cirad_hema <-function()
{
  #ui
  ui<-navbarPage(id="cirad","SDMs GUI",theme = "readtable",
                 tabPanel("Help/About",
                          uiOutput("ui_about")),
                 tabPanel("Data Upload",uiOutput("ui_import_data")),
                 tabPanel("ggplot2",uiOutput("ui_show_microdata")),        
                 tabPanel("Interactive map"),
                 tabPanel("biomod2",uiOutput("ui_sdmObj_create1")),
                 tabPanel("R-Code")
                 
                 
  )
  
  
  
  
  
  
  
  
  
  
  
  #server
  server<-function(input, output, session){
    #data
    inputdata<-reactive({
      read.csv(system.file("external/species/mammals_table.csv",
                           package="biomod2"), row.names = 1)
    }
    )
    # UI-output to display and reset currently available data
    output$ui_show_microdata <- renderUI({
      my_data_dt = reactive({
        datatable(inputdata(),
                  rownames = FALSE,
                  selection="none",
                  options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=20)
        )
      })
      #, options = list(scrollX=TRUE, lengthMenu=list(c(10, 25, 100, -1), c('10', '20', '100', 'All')), pageLength=25), filter="top", rownames=FALSE
      output$tab_inputdata <- DT::renderDataTable({
        my_data_dt()
      })
      out <- fluidRow(
        column(width = 12, offset = 0, h3("Loaded microdata")), class="wb-header")
      out <- list(out, fluidRow(
        column(12, dataTableOutput("tab_inputdata"))))
      return(out)
    })
    ###############################################################
    
    ######################## About/Help content ##########################################
    #
    
    output$ui_about <- renderUI({
      out <- fluidRow(
        column(width = 8, offset = 2, h2(("sdmApp"))),
        column(width = 8, offset = 2, p("This graphical user interface of",code("sdmgui")," offers the possibility to run 10 state-of-the-art modeling techniques to describe and model the relationships between a given species and its environment. It is an attempt to define the ecological niche of a particular species using environmental variables (temperature, precipitation, ...) with the potential use of making, for instance, future projections under climate and land use change scenarios. Although it has been mostly developed for ecologists that aim to predict species distribution, biomod2 can also be used to model any binomial data (for instance, gene, markers, ecosystem...) in function of any explanatory variables. 
                                        Even if you are not an expert in the",code("R"),"programming language. Detailed information on how to use this graphical user-interface (GUI) can be found in a tutorial (a so-called vignette) that is included in the",code("ensae"),"package.
                                        The vignette is available on",tags$a("GitHub pages", href="https://github.com/Abson-dev", target="_blank") 
                                        
                                        
        )))
      
      out <- list(out, fluidRow(
        column(width = 8, offset = 2, h4(("Contact and Feedback"))),
        column(width = 8, offset = 2, p("In case you have any suggestions or bug reports, please file an issue at the",
                                        tags$a("issue tracker", href="https://github.com/Abson-dev", target="_blank"),"in our",
                                        tags$a("GitHub repo", href="https://github.com/Abson-dev", target="_blank"),".")),
        column(width = 8, offset = 2, p("Before reporting any bugs, please make sure that you are working with an up-to-date",tags$b("R"),"installation and
                                        that all packages have been updated. You can do so by entering",code("update.packages(ask=FALSE)"),"into your",code("R"),"prompt."))
        ))
      out
    })
    #Import data
    # specific (gui)-options for csv-import
    output$ui_import_csv <- renderUI({
      rb1 <- radioButtons("import_csv_header", label=p("Does the first row contain the variable names?"), choices=c(TRUE,FALSE), inline=TRUE)
      rb2 <- radioButtons("import_csv_sep", label=p("Select the field separator"), choices=c(Comma=",", Semicolon=";", Tab="\t"), inline=TRUE)
      return(fluidRow(
        column(6, rb1, align="center"),
        column(6, rb2, align="center")))
    })
    
    # specific (gui)-options for r-dataframe import
    output$ui_import_rdf <- renderUI({
      selDF <- selectInput("sel_choose_df", label=NULL, choices=available_dfs,
                           selected=input$sel_choose_df, width="50%")
      # btn<-bsButton("choose_df", label = "Load data",
      #          block = TRUE,type="button" value = TRUE)
      btn <- myActionButton("btn_chooose_df",label=("Load data"), "primary")
      return(fluidRow(
        column(12, p("Select a test dataset or any object in your current workspace", align="center")),
        column(12, div(selDF, align="center")),
        column(12, p(btn, align="center"))))
    })
    
    # specific (gui)-options for sas import
    output$ui_import_sas <- renderUI({
      return(NULL)
    })
    
    # specific (gui)-options for spss import
    output$ui_import_spss <- renderUI({
      return(NULL)
    })
    
    # specific (gui)-options for stata import
    output$ui_import_stata <- renderUI({
      return(NULL)
    })
    output$ui_import_data_main <- renderUI({
      # cur_error <- lastError()
      # btn <- myActionButton("btn_reset_inputerror",label=("Try again!"), "primary")
      # if (!is.null(lastError())) {
      #   return(fluidRow(
      #     column(12, h4("Importing data resulted in an error!"), align="center"),
      #     column(12, verbatimTextOutput("ui_lasterror")),
      #     column(12, btn, align="center")))
      # }
      # Create the object with no values
      obj <- reactiveValues()
      obj$cur_selection_import <- "btn_import_data_1" # navigation for import
      val <- obj$cur_selection_import
      if (val=="btn_import_data_1") {
        val <- "rdf"
      }
      if (val=="btn_import_data_2") {
        val <- "rdata"
      }
      if (val=="btn_import_data_3") {
        val <- "spss"
      }
      if (val=="btn_import_data_4") {
        val <- "sas"
      }
      if (val=="btn_import_data_5") {
        val <- "csv"
      }
      if (val=="btn_import_data_6") {
        val <- "stata"
      }
      out <- fluidRow(
        column(width = 12, offset = 0, h3("Uploading microdata"), class="wb-header"),
        column(width = 12, offset = 0, p("Load the dataset."), class="wb-header-hint")
      )
      
      if (val %in% c("R","csv","spss","sas","rdata","stata")) {
        # convert characters automatically to factors
        rb1 <- radioButtons("rb_convert_c_to_f", label=p("Convert string variables (character vectors) to factor variables?"), choices=c(TRUE, FALSE), inline=TRUE)
        rb2 <- radioButtons("rb_drop_all_missings", label=p("Drop variables with only missing values (NA)?"), choices=c(TRUE, FALSE), inline=TRUE)
        
        out <- list(out, fluidRow(column(12, h4("Set additional options for the data import"))))
        
        out <- list(out, fluidRow(
          column(6, rb1, align="center"),
          column(6, rb2, align="center")))
        
        if (val == "csv") {
          allowed <- c(".txt",".csv")
          out <- list(out, uiOutput("ui_import_csv"))
        }
        if (val == "spss") {
          allowed <- c(".sav")
          out <- list(out, uiOutput("ui_import_spss"))
        }
        if (val == "sas") {
          allowed <- c(".sas7bdat")
          out <- list(out, uiOutput("ui_import_sas"))
        }
        if (val == "rdata") {
          allowed <- c(".rdata")
        }
        if (val == "stata") {
          allowed <- c(".dta")
          out <- list(out, uiOutput("ui_import_stata"))
        }
        
        out <- list(out, fluidRow(
          column(12, p("Note: the selected file is loaded immediately upon selecting. Set the above options before selecting the file."), align="center")
        ))
        
        fI <- fileInput("file1", p(paste0("Select file (allowed types are '",paste0(allowed, collapse="', '"),"')")),
                        width="75%", accept=allowed)
        out <- list(out, fluidRow(column(12, fI, align="center")))
      } else {
        out <- list(out, uiOutput("ui_import_rdf"))
      }
      out
    })
    ###################################################################
    output$ui_import_data_sidebar_left <- renderUI({
      output$ui_sel_resbtns_import <- renderUI({
        cc <- c("Testdata/internal data", "R-dataset (.rdata)", "SPSS-file (.sav)", "SAS-file (.sasb7dat)",
                "CSV-file (.csv, .txt)", "STATA-file (.dta)","Paste Data","Dowload Data")
        out <- fluidRow(column(12, h4("Select data source")))
        for (i in 1:length(cc)) {
          id <- paste0("btn_import_data_", i)
          if (obj$cur_selection_import==id) {
            style <- "primary"
          } else {
            style <- "default"
          }
          out <- list(out, fluidRow(
            # column(12, bsButton(id, label=cc[i], block=TRUE, size="extra-small", style=style), tags$br())
            column(12, bsButton(id, label=cc[i], block=TRUE, size="extra-small", style=style))
          ))
        }
        out
      })
      
      # required observers that update the color of the active button!
      eval(parse(text=genObserver_menus(pat="btn_import_data_", n=1:8, updateVal="cur_selection_import")))
      return(uiOutput("ui_sel_resbtns_import"))
    })
    output$ui_import_data <- renderUI({
      fluidRow(
        column(2, uiOutput("ui_import_data_sidebar_left"), class="wb_sidebar"),
        column(10, uiOutput("ui_import_data_main"), class="wb-maincolumn"))
    }
    )
    ##########################################################################
    #Biomod2 contents
    ##########################################################################
    ############### function ######################
    # all numeric variables in inputdata
    numVars <- reactive({
      inp <- inputdata()
      if (is.null(inp)) {
        return(NULL)
      }
      names(inp)[sapply(inp, class)%in% c("numeric","integer")]
    })
    ###############  end function ######################
    ############### function ######################
    # all factor variables in inputdata
    facVars <- reactive({
      inp <- inputdata()
      if (is.null(inp)) {
        return(NULL)
      }
      names(inp)[sapply(inp, class) == c("factor")]
    })
    ###############  end function ######################
    ############### function ######################
    # all character variables in inputata
    charVars <- reactive({
      inp <- inputdata()
      if (is.null(inp)) {
        return(NULL)
      }
      names(inp)[sapply(inp, class) == c("character")]
    })
    ###############  end function ######################
    ###############  function ######################
    # all variables available in the input data set
    allVars <- reactive({
      inp <- inputdata()
      if (is.null(inp)) {
        return(NULL)
      }
      cn <- colnames(inp)
      cl <- sapply(1:ncol(inp), function(x) {
        class(inp[[x]])
      })
      names(cn) <- paste0(cn," (",cl,")")
      cn
    })
    ###############  end function ######################
    ###############  function ######################
    dataTypes <- reactive({
      inputdata <- inputdata()
      if (is.null(inputdata)) {
        return(NULL)
      }
      cn <- colnames(inputdata)
      cl <- sapply(1:ncol(inputdata), function(x) {
        class(inputdata[[x]])
      })
      cl
    })
    
    ###############  end function ######################
    ############# function ########
    # dynamically generate inputs (currently used in setup(biomod2)
    shinyInput <- function(FUN, len, id, ...) {
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
      }
      inputs
    }
    ###############  end function ######################
    ############# function ########
    sdmData <- reactive({
      inputdata <- inputdata()
      if (is.null(inputdata)) {
        return(NULL)
      }
      vars <- allVars()
      
      vv <- obj$setupval_inc
      df <- data.frame(
        "Variable Name"=vars,
        Type=dataTypes(),
        Key=shinyInput(radioButtons, length(vars), paste0("setup_key_",vv,"_"), choices=c("No", "Cat.", "Cont."), inline=TRUE),
        Weight=shinyInput(checkboxInput, length(vars), paste0("setup_weight_",vv,"_"), value=FALSE, width="20px"),
        "Cluster ID"=shinyInput(checkboxInput, length(vars), paste0("setup_cluster_",vv,"_"), value=FALSE, width="20px"),
        Pram=shinyInput(checkboxInput, length(vars), paste0("setup_pram_",vv,"_"), value=FALSE, width="20px"),
        Remove=shinyInput(checkboxInput, length(vars), paste0("setup_remove_",vv,"_"), value=FALSE, width="20px")
      )
      df$nrCodes <- sapply(inputdata, function(x) { length(unique(x))} )
      df$nrNA <- sapply(inputdata, function(x) { sum(is.na(x))} )
      colnames(df) <- c("Variable name", "Type", "studied species", "latitude", "longitude","Environmental<br>variables",  "Delete", "Number of levels", "Number of missing")
      rownames(df) <- NULL
      df
    })
    ###############  end function ######################
    
    output$setupTable <- DT::renderDataTable({
      sdmData()
    }, server=FALSE, escape=FALSE, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
    options = list(
      scrollX=TRUE, scrollY=380, searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE, autoWidth=FALSE,
      # columnDefs=list(list(width='400px', targets = c(2))),
      columnDefs=list(list()),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); var event = new Event("AnonymiseDrawnEvent"); document.dispatchEvent(event);} ')
    ))
    
    
    
    output$ui_sdmObj_create1 <- renderUI({
      #input$btn_reset_sdc # dependency so that variable-types will get updated!
      #out <- NULL
      #if (!is.null(obj$last_error)) {
      #  out <- list(out, fluidRow(column(12, verbatimTextOutput("ui_lasterror")), class = "wb-error-toast"))
      #}
      
      txt_setup <- "Select the following variables for setting up the SDC problem instance: categorical key variables, continuous key variables (optional), variables selected for PRAM (optional), sample weight (optional), hierarchical identifier (optional), variables to be deleted (optional). Also, specify the parameter alpha and set a seed at the bottom of this page."
      txt_setup <- paste(txt_setup, tags$br(), tags$br(), "Tip - Before you start, make sure that variable types are appropriate. If not, go to the Microdata tab and convert variables to numeric or factor.")
      out <- NULL
      out <- list(out,
                  fluidRow(column(12, h4("Select variables", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center")),
                  fluidRow(column(12, DT::dataTableOutput("setupTable", height="100%"))))
      out
    })
    ######################################## end biomod2 contents ##########################################
    ########################################################################
  }
  shinyApp(ui, server)
  }
cirad_hema()
