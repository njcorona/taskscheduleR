#' @title Launch an RStudio addin which allows to schedule an Rscript interactively.
#' @description Launch an RStudio addin which allows to schedule an Rscript interactively.
#' 
#' @param RscriptRepository path to the folder where R scripts will be copied to and launched. Defaults to the extdata folder in the taskscheduleR R library
#' @param debug passed on to \code{\link{taskscheduler_create}}
#' @return the return of \code{\link[shiny]{runGadget}}
#' @export
#' @examples 
#' \dontrun{
#' taskschedulerAddin()
#' }
#' 
#' 
#' 

library(shiny)
library(shinyjs)
library(shinyTime)
library(shinythemes)
library(miniUI) # TODO: Delete - shouldn't be necessary.
library(taskscheduleR) # TODO: Delete before testing.

getDateFormat <- function(dateformat) {
  if (dateformat == "%m/%Y/%d") { return("mm-yyyy-dd") } else
    if (dateformat == "%d/%Y/%m") { return("dd-yyyy-mm") } else
      if (dateformat == "%Y/%d/%m") { return("yyyy-dd-mm") } else 
        if (dateformat == "%Y/%m/%d") { return("yyyy-mm-dd") } else
          if (dateformat == "%m/%d/%Y") { return("mm-dd-yyyy") } else
            if (dateformat == "%d/%m/%Y") { return("dd-mm-yyyy") }
}


taskschedulerAddinx <- function(RscriptRepository, debug = TRUE) {
  requireNamespace("shiny")
  requireNamespace("miniUI") # TODO: Delete.
  requireNamespace("shinyjs")
  requireNamespace("shinyTime")
  requireNamespace("shinythemes")
  
  current_repo <- file.path(system.file("extdata", package="taskscheduleR"), "RscriptRepository.rds")
  
  if (missing(RscriptRepository)) {
    if (file.exists(current_repo)) {
      RscriptRepository <- readRDS(file = current_repo)
    } else {
      RscriptRepository <- system.file("extdata", package="taskscheduleR")
      saveRDS(RscriptRepository, file = current_repo)
    }
  }
  
  local_dateformat <- file.path(system.file("extdata", package="taskscheduleR"), "dateformat.rds")
  
  if (file.exists(local_dateformat)) {
    datefmt <- readRDS(file = local_dateformat)
  } else {
    datefmt <- "%d/%m/%Y"
    saveRDS(datefmt, file = local_dateformat)
  }
  
  check <- NULL
  
  ui <- shiny::fluidPage(
    useShinyjs(),
    # Shiny fileinput resethandler
    shiny::tags$script('
                           Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
                           var id = "#" + x + "_progress";
                           var idBar = id + " .bar";
                           $(id).css("visibility", "hidden");
                           $(idBar).css("width", "0%");
                           });
                           '),
    theme = shinytheme("lumen"),
    titlePanel("Schedule your R scripts quickly and easily!"),
    # Input
    sidebarLayout(
      #sidebarPanel(
        shiny::fillCol(flex = c(5, 5, 25, 20, 20, 5, 5, 5, 5), height = "1000px",
                       shiny::fillRow(
                         shiny::uiOutput('fileSelect'),
                         shiny::strong("Task Check"),
                         shiny::textOutput("text")
                       ),
                       shiny::fillRow(
                         shiny::textInput('rscript_repository', label = "Specify a repository to store task logs.", value = RscriptRepository), 
                         shiny::textInput('taskname', label = "Choose a name for your task.", placeholder = "task_name")
                       ),
                       shiny::fillRow(
                         shiny::radioButtons('frequency', label = "Schedule:", choices = c('Once', 'Every minute', 'Hourly', 'Daily', 'Weekly', 'Monthly', 'On log-on', 'On idle')),
                         shiny::fillCol(
                           shiny::dateInput('startdate', label = "Start date:", startview = "month", weekstart = 1, min = Sys.Date(), format = getDateFormat(datefmt)),
                           #shiny::textInput('hour', label = "Start time (24-hr):", placeholder = "23:59"),
                           timeInput('hour', label = "Start time (24-hr):", value = Sys.time() + 3600, seconds = FALSE),
                           shiny::textInput('rscript_args', label = "Additional arguments to your R script:", value = ""),
                           shiny::selectInput('date_fmt', label = "Date format of your device:", choices = c("%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%Y/%d/%m", "%d/%Y/%m", "%m/%Y/%d"), multiple = FALSE, selected = datefmt)                                                           
                         )
                       ),
                       shiny::fillRow(
                         shiny::uiOutput('days_weekly'),
                         shiny::numericInput(inputId = 'idletime', label = "The amount of idle time, in minutes, to wait before running a scheduled ONIDLE task:", min = 1, max = 999, value = 1, step = 1)
                       ),
                       shiny::fillRow(
                         shiny::uiOutput('days_monthly'),
                         shiny::uiOutput('months')
                       ),
                       shiny::actionButton('create', "Create task", icon = shiny::icon("play-circle")),
                       shiny::uiOutput("getFiles"),
                       shiny::actionButton('Stop', "Stop task", icon = shiny::icon("stop")),
                       shiny::actionButton('Delete', "Delete task", icon = shiny::icon("remove"))
        ),
        # Output:
        mainPanel(
          shiny::uiOutput('fileSelect')
        )
      #)
    ),
    
  )
  
  # Server code for the gadget.
  server <- function(input, output, session) {
    
    ###############
    # UI ELEMENTS #
    ###############
    
    # UI ELEMENT FOR FILE INPUT
    output$fileSelect <- shiny::renderUI({
      shiny::fileInput(inputId = 'file', 'Choose your R script',
                       accept = c(".R", "R", ".Rmd", "Rmd"),
                       multiple = FALSE, placeholder = "your_R_script.R")
    })
    
    # UI ELEMENT FOR DAYS OF THE WEEK (MON-SUN)
    output$days_weekly <- shiny::renderUI({
      shiny::checkboxGroupInput(inputId = 'daysoftheweek', label = 'Choose days of the week to run task.', 
                                choices = c("All", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    })
    
    # UI ELEMENT FOR DAYS OF THE MONTH (1-31)
    output$days_monthly <- shiny::renderUI({
      shiny::checkboxGroupInput(inputId = 'daysofthemonth', label = 'Choose days of the month to run task.', choices = c("All", "1", " 2", " 3", " 4", " 5", " 6", " 7"," 8", " 9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"), inline = TRUE, width = "175px")
    })
    
    # UI ELEMENTS FOR MONTHS OF THE YEAR (JAN-DEC)
    output$months <- shiny::renderUI({
      shiny::checkboxGroupInput(inputId = 'monthsoftheyear', label = 'Choose months of the year to run task.', choices = c("All", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), inline = TRUE)
    })
    
    ##########################
    # BUTTON EVENT LISTENERS #
    ##########################
    
    # EVENT LISTENER:  CHECK IF THERE ARE PRE-EXISTING TASKS FOR UPLOADED FILE
    shiny::observeEvent(input$file, {
      output$text <- shiny::renderText({
        # TRUE if script already has a task
        # FALSE if script has no tasks
        if(input$file$name %in% list.files(RscriptRepository, pattern = ".R$|.r$")){
          check <<- TRUE
          sprintf("A task for %s already exists. \n Continuing will overwrite that task with this new task.",
                  input$file$name)
        } else {
          check <<- FALSE
          sprintf("No tasks exist for %s.  A new task will be created for %s.",
                  input$file$name)
        }
      })
    })
    
    # EVENT LISTENER:  CONTROL "ALL" FOR DAYS OF THE MONTH
    shiny::observeEvent(input$daysofthemonth, {
      if ("All" %in% input$daysofthemonth) {
        shiny::updateCheckboxGroupInput(session, 'daysofthemonth', selected = c("All", "1", " 2", " 3", " 4", " 5", " 6", " 7"," 8", " 9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"))
      } else if (!("All" %in% input$daysofthemonth) & length(input$daysofthemonth) == 31) {
        shiny::updateCheckboxGroupInput(session, 'daysofthemonth', selected = character(0))
      } 
    })
    
    # EVENT LISTENER:  CONTROL "ALL" FOR DAYS OF THE WEEK
    shiny::observeEvent(input$daysoftheweek, {
      if ("All" %in% input$daysoftheweek) {
        shiny::updateCheckboxGroupInput(session, 'daysoftheweek', selected = c("All", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
      } else if (!("All" %in% input$daysoftheweek) & length(input$daysoftheweek) == 7) {
        shiny::updateCheckboxGroupInput(session, 'daysoftheweek', selected = character(0))
      } 
    })
    
    # EVENT LISTENER:  CONTROL "ALL" FOR MONTHS OF THE YEAR
    shiny::observeEvent(input$monthsoftheyear, {
      if ("All" %in% input$monthsoftheyear) {
        shiny::updateCheckboxGroupInput(session, 'monthsoftheyear', selected = c("All", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
      } else if (!("All" %in% input$monthsoftheyear) & length(input$monthsoftheyear) == 12) {
        shiny::updateCheckboxGroupInput(session, 'monthsoftheyear', selected = character(0))
      } 
    })
    
    # EVENT LISTENER:  UPDATE BUTTONS GIVEN TASK FREQUENCY
    shiny::observeEvent(input$frequency, {
      updateCheckboxGroupInput(session, inputId = 'daysoftheweek', selected=character(0))
      updateCheckboxGroupInput(session, inputId = 'daysofthemonth', selected=character(0))
      updateCheckboxGroupInput(session, inputId = 'monthsoftheyear', selected=character(0))
      disable('days_weekly')
      disable('days_monthly')
      disable('months')
      disable('startdate')
      disable('hour')
      disable('idletime')
      updateTextInput(session, inputId = 'idletime', label = NULL, value = "", placeholder = NULL)
      
      if (input$frequency == "Once") {
        enable('startdate') # SHOULD THIS BE ENABLED?
        enable('hour')
      }
      if (input$frequency == "Every minute") {
        enable('hour')
        enable('startdate')
      }
      if (input$frequency == "Hourly") {
        enable('hour')
        enable('startdate')
      }
      if (input$frequency == "Daily") {
        enable('hour')
        enable('startdate')
      }
      if (input$frequency == "Weekly") {
        enable('hour')
        enable('days_weekly')
        enable('startdate')
      }
      if (input$frequency == "Monthly") {
        # MAKE SURE THAT HAVING A BLANK DATE IS HANDLED
        enable('hour')
        enable('days_monthly')
        enable('months')
        enable('startdate')
      }
      if (input$frequency == 'On log-on') {
        updateTimeInput(session, inputId = 'hour', label = NULL, value = paste(Sys.Date(), " 00:00:00 EDT", sep = ""))
        updateDateInput(session, inputId = 'startdate', label = NULL, value = Sys.Date(), min = NULL, max = NULL)
        
      }
      if (input$frequency == "On idle") {
        updateTimeInput(session, inputId = 'hour', label = NULL, value = paste(Sys.Date(), " 00:00:00 EDT", sep = ""))
        updateDateInput(session, inputId = 'startdate', label = NULL, value = Sys.Date(), min = NULL, max = NULL)
        updateTextInput(session, inputId = 'idletime', label = NULL, value = 1, placeholder = NULL)
        enable('idletime')
      }
    })
    
    # EVENT LISTENER:  CONFIRM THAT SCRIPT REPOSITORY IS VALID.  SAVE IF YES; REPROMPT IF NO.
    shiny::observeEvent(input$rscript_repository, {
      RscriptRepository <<- normalizePath(input$rscript_repository, winslash = "/")
      output$text <- shiny::renderText({
        if(file.exists(RscriptRepository) && file.info(RscriptRepository)$isdir == TRUE){
          saveRDS(RscriptRepository, file = current_repo)
        } else {
          sprintf("The R script repository at \"%s\" does not exist.  Please enter a valid filepath without spaces.", RscriptRepository)
        }
      })
    })
    
    # EVENT LISTENER:  CHECK LIST OF TASKS CREATED BY TASKSCHEDULER FOR REPEATED TASKNAMES
    shiny::observeEvent(input$taskname, {
      if (!file.exists(paste(system.file("extdata", package = "taskscheduleR"), "/tasknames.txt"))) {
        # This means no tasks have been created by the taskscheduleR.
      } else {
        processFile = function(filepath) {
          con = file(filepath, "r")
          while ( TRUE ) {
            line = readLines(con, n = 1)
            if (line == input$taskname) {
              sprintf("A taskscheduleR task called %s already exists.
                      Creating this task will overwrite your old task.
                      Please rename this task if you do not want to overwrite your old task.")
              break
            }
            if ( length(line) == 0 ) {
              break
            }
          }
          close(con)
          }
        processFile(paste(system.file("extdata", package="taskscheduleR"), "/tasknames.txt"))
      }
  })
    
    # EVENT LISTENER:  UPDATE DATE FORMAT
    shiny::observeEvent(input$date_fmt, {
      datefmt <<- input$date_fmt
      saveRDS(datefmt, file = local_dateformat)
      print(input$startdate)
    })
    
    ############################
    # CREATE OR OVERWRITE TASK #
    ############################
    
    shiny::observeEvent(input$create, {
      shiny::req(input$frequency)
      shiny::req(input$file)
      shiny::req(input$taskname)
      
      # Set fields appropriate for schedule/frequency.
      starttime <- input$hour
      rscript_args <- input$rscript_args
      days <- NULL # Only not null if schedule is weekly or monthly.
      months <- NULL # Only not null if schedule is monthly.
      startdate <- NULL # Only not null if schedule is monthly, weekly, daily, hourly, or by minute.
      # ^^^ DOES STARTDATE APPLY FOR THE "ONCE" SCHEDULE OPTION?
      idletime <- NULL # Only not null if on idle.
      
      if (length(input$startdate) == 0) {
        input$startdate <- Sys.Date()
      }
      
      if (input$frequency == "Once") {
        
      }
      if (input$frequency == "Every minute") {
      }
      if (input$frequency == "Hourly") {
      }
      if (input$frequency == "Daily") {
      }
      if (input$frequency == "Weekly") {
        weekdays <- input$daysoftheweek[which(input$daysoftheweek != "All")]
        weekdays <- toupper(weekdays)
      }
      if (input$frequency == "Monthly") {
        days <- as.numeric(input$daysofthemonths[which(input$daysofthemonth != "All")])
        months <- input$monthsoftheyear[which(input$monthsoftheyear != "All")]
      }
      if (input$frequency == 'On log-on') {
      }
      if (input$frequency == "On idle") {
      }
      
      # Convert "On log-on" to "ONLOGON" and "On idle" to "ONIDLE" 
      frequency <- gsub(" ", "", input$frequency, fixed = TRUE)
      frequency <- gsub("-", "", frequency, fixed = TRUE)
      frequency <- toupper(frequency)
      
      # Copy the uploaded file from the webapp to the main folder to store the scheduled rscripts.
      if(length(grep(" ", RscriptRepository)) > 0){
        stop(sprintf("Make sure the RscriptRepository does not contain spaces, change argument %s to another location on your drive which contains no spaces", RscriptRepository))
      }
      myscript <- paste0(RscriptRepository, "/", input$file$name)
      done <-  file.copy(input$file$datapath, myscript, overwrite = TRUE)
      if(!done){
        stop(sprintf('Copying file %s to %s failed. Do you have access rights to %s?', file.path(input$file$datapath, input$file$name), myscript, dirname(myscript)))
      }
      
      # Make schedule task - UPDATE TO USE TASK NAME INSTEAD OF FILE NAME LATER
      if(check) {
        taskscheduler_delete(taskname = input$file$name)
      }
      taskscheduler_create(taskname = input$file$name, rscript = myscript, schedule = frequency, startdate = format(input$date, input$date_fmt), starttime = starttime, days = days, months = months, rscript_args = rscript_args, debug = debug, idletime = input$idletime)
      
      print("GOTHERE")
      # Reset ui inputs
      shiny::updateDateInput(session, inputId = 'startdate', value = Sys.Date())
      shiny::updateRadioButtons(session, inputId = 'frequency', selected = "Once")
      shiny::updateSelectInput(session, inputId = "days", selected = "*")
      shiny::updateTextInput(session, inputId = "hour", value = format(Sys.time() + 122, "%H:%M"))
      shiny::updateTextInput(session, inputId = "rscript_args", value = "")
      output$fileSelect <- shiny::renderUI({
        shiny::fileInput(inputId = 'file', 'Choose your Rscript',
                         accept = c("R-bestand"),
                         multiple = FALSE)
      })
      shiny::updateSelectInput(session, inputId="getFiles", choices = list.files(RscriptRepository, pattern = ".R$|.r$"))
      
      output$text <- shiny::renderText({""})
      
    })
    
    ###########################
    # Schedule list
    ###########################
    output$getFiles <- shiny::renderUI({
      shiny::selectInput(inputId = 'getFiles', "Select Task", choices = list.files(RscriptRepository, pattern = ".R$|.r$"))
    })
    
    ###########################
    # STOP
    ###########################
    shiny::observeEvent(input$Stop, {
      shiny::updateSelectInput(session, inputId="getFiles", choices = list.files(RscriptRepository, pattern = ".R$|.r$"))
      taskcheduler_stop(taskname = input$getFiles)
    })
    
    ###########################
    # DELETE
    ###########################
    shiny::observeEvent(input$Delete, {
      taskscheduler_delete(taskname = input$getFiles)
      try(file.remove(file.path(RscriptRepository, input$getFiles)))
      shiny::updateSelectInput(session, inputId="getFiles", choices = list.files(RscriptRepository, pattern = ".R$|.r$"))
      
    })
    
    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    shiny::observeEvent(input$done, {
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      shiny::stopApp()
    })
    }
  
  # Use a modal dialog as a viewr.
  viewer <- shiny::dialogViewer("Task ScheduleR", width = 700, height = 600)
  # shiny::runGadget(ui, server, viewer = viewer)
  shinyApp(ui = ui, server = server)
}
