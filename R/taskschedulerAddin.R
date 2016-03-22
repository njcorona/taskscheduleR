# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




taskschedulerAddin <- function() {
  # load pkg
  require("shiny")
  require("miniUI")
  require("taskscheduleR")
  
  ui <- miniPage(
    # Shiny fileinput resethandler
    tags$script('
                Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
                var id = "#" + x + "_progress";
                var idBar = id + " .bar";
                $(id).css("visibility", "hidden");
                $(idBar).css("width", "0%");
                });
                '),
    
    gadgetTitleBar("Schedule your Rscript fast and easy"),
    
    ## Your UI items go here.
    miniTabstripPanel(
      miniTabPanel(title = 'Upload and create', icon = icon("cloud-upload"),
                   miniContentPanel(
                     fillCol(flex = c(1,2),
                             fillRow(
                               uiOutput('fileSelect'),
                               miniContentPanel(padding = 0,
                                                strong("Task checking:"),
                                                textOutput("text")
                               )),
                             fillRow(
                               radioButtons('task', "Schedule:",
                                            choices = c( 'ONCE', 'MONTHLY', 'WEEKLY', 'DAILY', 'HOURLY', 'MINUTE', 'ONLOGON', 'ONIDLE'),
                                            selected = NULL),
                               dateInput('date',label = "Start date:",
                                         startview = "month", language = "en",
                                         weekstart = 0,
                                         min = Sys.Date())
                             )
                     )
                   ),
                   miniButtonBlock(border = "bottom",
                                   actionButton('create', "Create task", icon = icon("play-circle"))
                   )
      ),
      miniTabPanel(title = 'Stop or Delete', icon = icon("table"),
                   miniContentPanel(
                     uiOutput("getFiles")
                   ),
                   miniButtonBlock(border = "bottom",
                                   actionButton('Stop', "Stop task", icon = icon("stop")),
                                   actionButton('Delete', "Delete task", icon = icon("remove"))
                   )
      )
    )
    )
  
  # Server code for the gadget.
  server <- function(input, output, session) {
    
    # Path for central repository in package dir
    RscriptRepository <- system.file("extdata", package="taskscheduleR")
    
    # Ui element for fileinput
    output$fileSelect <- renderUI({
      fileInput(inputId = 'file', 'Choose your Rscript',
                accept = c("R-bestand"),
                multiple = FALSE)
    })
    
    # When file has been uploaded check if it already exists
    observeEvent(input$file, {
      output$text <- renderText({
        # TRUE if script already has a task
        # FALSE if script has no tasks
        if(input$file$name %in% list.files(RscriptRepository)){
          check <<- TRUE
          sprintf("Rscript %s already exists. \n Continuing will overwrite and make a new task schedule",
                  input$file$name)
        } else {
          check <<- FALSE
          sprintf("Schedulingtask for Rscript %s does not excist",
                  input$file$name)
        }
      })
    })
    
    
    
    ###########################
    # CREATE / OVERWRITE
    ###########################
    
    observeEvent(input$create, {
      req(input$task)
      req(input$file)
      
      # Get temp path from shiny inputFile + filename
      inputPath <- sprintf(input$file$datapath, input$file$name)
      # Make output path:
      # This is the subdir extdata from package taskscheduleR
      # This is the main folder to store the scheduled rscripts.
      outputPath <- paste0(RscriptRepository,"/", input$file$name)
      
      # Make input variables for taskscheduler
      inputStarttime <- format(input$date, "%d/%m/%Y")
      
      if(input$task == "MONTHLY" ){
        days = format(input$date, "%d")
      }
      else if(input$task == "WEEKLY"){
        weekdays = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
        days = weekdays[as.integer(format(input$date, "%w"))]
      }
      else {
        # get default value by setting days to null.
        days = NULL
      }
      
      # Make schedule task
      #
      if(check){
        # Copy the temp file to the main folder. With overwrite.
        file.copy(inputPath,  outputPath, overwrite = TRUE)
        taskscheduler_delete(taskname = input$file$name)
        taskscheduler_create(taskname = input$file$name, rscript = outputPath , schedule = input$task, startdate = inputStarttime, days = days)
      } else {
        # Copy the temp file to the main folder. With overwrite.
        file.copy(inputPath,  outputPath, overwrite = TRUE)
        taskscheduler_create(taskname = input$file$name, rscript = outputPath , schedule = input$task, startdate = inputStarttime, days = days)
      }
      
      # Reset ui inputs
      updateDateInput(session, inputId = 'date', value = Sys.Date())
      updateRadioButtons(session, inputId = 'task', selected = "ONCE")
      output$fileSelect <- renderUI({
        fileInput(inputId = 'file', 'Choose your Rscript',
                  accept = c("R-bestand"),
                  multiple = FALSE)
      })
      updateSelectInput(session, inputId="getFiles", choices = list.files(RscriptRepository))
      
      output$text <- renderText({
        ""})
      
    })
    
    ###########################
    # Schedule list
    ###########################
    
    output$getFiles <- renderUI({
      selectInput(inputId = 'getFiles', "Select Task",choices = list.files(RscriptRepository))
    })
    
    
    ###########################
    # STOP
    ###########################
    observeEvent(input$Stop, {
      updateSelectInput(session, inputId="getFiles", choices = list.files(RscriptRepository))
      taskcheduler_stop(taskname = input$getFiles)
    })
    
    ###########################
    # DELETE
    ###########################
    
    observeEvent(input$Delete,{
      taskscheduler_delete(taskname = input$getFiles)
      try(file.remove(paste0(RscriptRepository,"/", input$getFiles)))
      updateSelectInput(session, inputId="getFiles", choices = list.files(RscriptRepository))
      
    })
    
    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    observeEvent(input$done, {
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      stopApp()
    })
  }
  
  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Task ScheduleR", width = 700, height = 500)
  runGadget(ui, server, viewer = viewer)
  
  }




