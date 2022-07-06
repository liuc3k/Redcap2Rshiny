####========================server==========##
##Required Packages:
library(shiny)
library(REDCapR)##Connect REDCap data system
#library(redcapAPI)
library(dplyr)  ##Data Processing
library(DT)     ##Render a table widget
library(shinyjs) ## Use JS code in shiny
library(shinyWidgets) ## Special Shiny Widgets
library(data.table)
# library(shinyDataFilter)##Dynamic data filter
Sys.setenv(REDCAP_BYPASS_SANITIZE_TOKEN=1)
server <- function(input, output, session) {
  
  ########===Data Dict===#########
  project_title<-reactive({
    ##validate
    validate(need(input$token!='' , 'Please fill Token!'))
    formData <- list("token"=input$token,
                     content='project',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=input$api, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Fields_num<-reactive({
    ##validate
    validate(need(length(input$token)!='' , 'Please fill Token!'))
    formData <- list("token"=input$token,
                     content='exportFieldNames',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=input$api, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Instruments_num<-reactive({
    ##validate
    validate(need(length(input$token)!='' , 'Please fill Token!'))
    formData <- list("token"=input$token,
                     content='instrument',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=input$api, body = formData, encode = "form")
    result <- httr::content(response)
  })
  ID<-reactive({
    ##validate
    validate(need(length(input$token)!='' , 'Please fill Token!'))
    formData <- list("token"=input$token,
                     content='record',
                     action='export',
                     format='csv',
                     type='flat',
                     csvDelimiter='',
                     'fields[0]'=Fields_num()$original_field_name[1],
                     'events[0]'=Event()$unique_event_name[1],
                     rawOrLabel='raw',
                     rawOrLabelHeaders='raw',
                     exportCheckboxLabel='false',
                     exportSurveyFields='false',
                     exportDataAccessGroups='false',
                     returnFormat='json'
    )
    response <- httr::POST(url=input$api, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Myproject<-reactive({
    ##validate
    validate(need(length(input$token)!='' , 'Please fill Token!'))
    `My Project`<-data.frame(
      `Project Title`=project_title()$project_title,
      Records=nrow(ID()),
      Events=nrow(Event()),
      Fields=nrow(Fields_num()),
      Instruments=paste(nrow(  Instruments_num())," forms"))
  })
  ### Execute Project Information
  output$execute_info<-renderUI({
    actionButton("execute_button_info", "Execute", icon=icon('glyphicon glyphicon-play'),class='glyphicon glyphicon-play')
  })
  info <- reactiveValues(doTable_info = FALSE)
  
  observeEvent(input$execute_button_info, {
    
    info$doTable_info <- input$execute_button_info
  })
  output$projectinfo<-renderDT({
    ##Execute
    if (info$doTable_info == FALSE) return()
    isolate({
      
      ##validate
      validate(need(length(input$token)!='' , 'Please fill Token!'))
      datatable(Myproject(),
                rownames= FALSE,
                #caption ='My Projects' ,
                options = list(dom = 't')) # only display the table, and nothing else
      
    })  
  })
  
  Event<-reactive({
    formData <- list("token"=input$token,
                     content='event',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=input$api, body = formData, encode = "form")
    result <- httr::content(response)
  })
  
  
  
  ### Data
  
  output$Report_name<-renderUI({
    selectInput("Report_name","Report name:",c("All Data"="all",
                                               "Selected Instruments"="selected"))
  })
  
  spec<-reactive({
    
    # REDCapR::redcap_metadata_read(redcap_uri = input$api,
    #                               token      = input$token)$data
    formData <- list("token"=input$token,
                     content='metadata',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=input$api, body = formData, encode = "form")
    result <- httr::content(response)
    
  })
  
  output$Instrument<-renderUI({
    selectInput("Instrument","Instrument:",selected ='',c(unique(spec()$form_name)))
    
  })
  select_fields<-reactive({
    select_fields<-c(spec()%>%
                       filter(form_name==input$Instrument)%>%
                       select(field_name))
  })
  
  output$Columns_all<-renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "Columns_All",
      label = "Fields:",
      # choices=c(unique(spec()$field_name)),
      choices = c(unique(Fields_num()$original_field_name)),
      # selected = c(unique(spec()$field_name)),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
      )
    )
    
    
  })
  output$Event_all<-renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "event_all",
      label = "Events:",
      # choices=c(unique(Event()$redcap_event_name)),
      # selected =c(unique(Event()$redcap_event_name)) ,
      choices=c(unique(Event()$unique_event_name)),
      selected =c(unique(Event()$unique_event_name)) ,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
      )
    )
    
    
  })
  output$Event_ins<-renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "event_ins",
      label = "Events:",
      # choices=c(unique(Event()$redcap_event_name)),
      # selected =c(unique(Event()$redcap_event_name)) ,
      choices=c(unique(Event()$unique_event_name)),
      selected =c(unique(Event()$unique_event_name)) ,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
      )
    )
    
    
  })
  ##Execute button for entire dataset
  v <- reactiveValues(doTable_all = FALSE)
  
  observeEvent(input$execute_button_all, {
    
    v$doTable_all <- input$execute_button_all
  })
  
  
  DataListing_all<-reactive({
    if (v$doTable_all == FALSE) return()
    
    isolate({
      data <- REDCapR::redcap_read(redcap_uri = input$api,
                                   token      = input$token,
                                   
                                   raw_or_label=input$raw_label,
                                   events = input$event_all,
                                   raw_or_label_headers=input$raw_label_headers,
                                   export_data_access_groups=input$export_data_access_groups,
                                   filter_logic=input$filter_all,
                                   # fields =c(field()[1],input$Columns_All)
                                   fields =c(Fields_num()$original_field_name[1],input$Columns_All)
      )$data
    })
  })
  output$DM_TBL_all <- renderDT({
    
    datatable(DataListing_all(),rownames= FALSE,options = list(autoWidth = TRUE,scrollX=TRUE))
  })
  
  
  ##Execute button for selected instrument
  q <- reactiveValues(doTable_selected = FALSE)
  
  observeEvent(input$execute_button_selected, {
    
    q$doTable_selected <- input$execute_button_selected
  })
  DataListing_selected<-reactive({
    if (q$doTable_selected == FALSE) return()
    
    isolate({
      data <- REDCapR::redcap_read(redcap_uri = input$api,
                                   token      = input$token,
                                   raw_or_label=input$raw_label_ins,
                                   raw_or_label_headers=input$raw_label_headers_ins,
                                   forms=input$Instrument,
                                   events = input$event_ins,
                                   filter_logic=input$filter_ins,
                                   export_data_access_groups=input$export_data_access_groups_ins,
                                   # fields =c(field()[1])
                                   fields =c(Fields_num()$original_field_name[1])
      )$data
    })
  })
  output$DM_TBL_selected <- renderDT({
    
    datatable(DataListing_selected(),rownames= FALSE,options = list(autoWidth = TRUE,scrollX=TRUE))
  })
  ## Reset button for entire data 
  output$reset_all<-renderUI({actionButton("reset_all", "Reset",
                                           # icon = icon("refresh"),
                                           icon=icon('glyphicon glyphicon-refresh'),
                                           class='glyphicon glyphicon-refresh',
                                           style = 'margin-top:25px')})
  
  
  observeEvent(
    input$reset_all,{shinyjs::reset("Table_all_div")}
  )
  ## Reset button for selected data 
  output$reset_selected<-renderUI({actionButton("reset_selected", "Reset",
                                                # icon = icon("refresh"),
                                                icon=icon('glyphicon glyphicon-refresh'),
                                                class='glyphicon glyphicon-refresh',
                                                style = 'margin-top:25px')})
  
  
  observeEvent(
    input$reset_selected,{shinyjs::reset("Table_selected_div")}
  )
  ## Download button for entire data 
  data_listing_dl_all<-reactive({
    data <- sapply(DataListing_all(), as.character)
    data[is.na(data)] <- ""   # Replace NA with blank
    data                      # Print updated data frame
  })
  output$Data_Listing_dl_BUTTON_all<-renderUI({downloadButton("Data_Listing_dl_all","Download")})
  
  output$Data_Listing_dl_all <- downloadHandler(
    filename = function() {
      paste0("Data Listing", ".csv")
    },
    content = function(file) {
      
      write.csv( data_listing_dl_all(), file, row.names=FALSE)
    }
  )
  ## Download button for selected data 
  data_listing_dl_selected<-reactive({
    data <- sapply(DataListing_selected(), as.character)
    data[is.na(data)] <- ""   # Replace NA with blank
    data                      # Print updated data frame
  })
  output$Data_Listing_dl_BUTTON_selected<-renderUI({downloadButton("Data_Listing_dl_selected","Download")})
  
  output$Data_Listing_dl_selected <- downloadHandler(
    filename = function() {
      paste0("Data Listing", ".csv")
    },
    content = function(file) {
      
      write.csv( data_listing_dl_selected(), file, row.names=FALSE)
    }
  )
  ### Execute
  output$execute_all<-renderUI({
    actionButton("execute_button_all", "Execute", icon=icon('glyphicon glyphicon-play'),class='glyphicon glyphicon-play')
  })
  output$execute_selected<-renderUI({
    actionButton("execute_button_selected", "Execute", icon=icon('glyphicon glyphicon-play'),class='glyphicon glyphicon-play')
  })
  ## Reports
  ##Execute button for Report
  output$execute_report<-renderUI({
    actionButton("execute_button_report", "Execute", icon=icon('glyphicon glyphicon-play'),class='glyphicon glyphicon-play')
  })
  report <- reactiveValues(doTable_report = FALSE)
  
  observeEvent(input$execute_button_report, {
    
    report$doTable_report <- input$execute_button_report
  })
  output$Reports_DT<-renderDT({
    if (report$doTable_report == FALSE) return()
    
    isolate({
      ##validate
      validate(need(input$Reports_Name!='' , 'Please fill Report ID!'))
      datatable(
        REDCapR::redcap_report(
          redcap_uri = input$api,
          token      = input$token,
          raw_or_label=input$raw_label_report,
          raw_or_label_headers=input$raw_label_headers_report,
          report_id  = as.numeric(input$Reports_Name))$data,
        
        rownames= FALSE,
        options = list(autoWidth = TRUE,
                       scrollX=TRUE,
                       scrollY = "460px",
                       fixedColumns = FALSE,
                       rowGroup = list(dataSrc = 2)))
    })
  })
  Reports_DT_DL<-reactive({
    if (report$doTable_report == FALSE) return()
    
    isolate({  
      ##validate
      validate(need(input$Reports_Name!='' , 'Please fill Report ID!'))
      formData <- list("token"=input$token,
                       content='report',
                       format='csv',
                       report_id=as.numeric(input$Reports_Name),
                       csvDelimiter='',
                       rawOrLabel=input$raw_label_report,
                       rawOrLabelHeaders=input$raw_label_headers_report,
                       exportCheckboxLabel='false',
                       returnFormat='json'
      )
      response <- httr::POST(input$api, body = formData, encode = "form")
      result <- httr::content(response)
      data <- sapply(result, as.character)
      data[is.na(data)] <- ""   # Replace NA with blank
      data                      # Print updated data frame
    })
  })
  output$Reports_dl <- downloadHandler(
    filename = function() {
      paste0("Reports", ".csv")
    },
    content = function(file) {
      write.csv(Reports_DT_DL(), file, row.names=FALSE)
    }
  )
  
  ### Data Dict
  ##Execute Button
  output$execute_dict<-renderUI({
    actionButton("execute_button_dict", "Execute", icon=icon('glyphicon glyphicon-play'),class='glyphicon glyphicon-play')
  })
  dict <- reactiveValues(doTable_dict = FALSE)
  
  observeEvent(input$execute_button_dict, {
    
    dict$doTable_dict <- input$execute_button_dict
  })   
  data_dict<-reactive({
    if (dict$doTable_dict == FALSE) return()
    
    isolate({  
      ##validate
      validate(need(!(is.na(input$form_dict)) , 'Please select instrument forms!')) 
      redcap_metadata_read(
        redcap_uri=input$api,
        token=input$token
        
      )$data
    })
  })
  output$Instrument_dict<-renderUI({
    #selectInput("form_dict","Instrument:",selected ='',c(unique(spec()$form_name)))
    shinyWidgets::pickerInput(
      "form_dict",
      "Instrument:",
      selected =c(unique(spec()$form_name)) ,
      choices=c(unique(spec()$form_name)),
      
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
      )
    )
  })  
  data_dict_dl<-reactive({
    data <- sapply(data_dict()%>%filter(form_name%in%c(input$form_dict)), as.character)
    data[is.na(data)] <- ""   # Replace NA with blank
    data                      # Print updated data frame
  })
  output$DD_TBL<-renderDT({
    if (dict$doTable_dict == FALSE) return()
    
    isolate({  
      ##validate
      validate(need(!(is.na(input$form_dict)) , 'Please select instrument forms!'))  
      datatable(
        
        
        data_dict()%>%filter(form_name%in%c(input$form_dict)),
        
        rownames= FALSE,
        options = list(autoWidth = TRUE,
                       scrollX=TRUE,
                       scrollY = "460px",
                       rowGroup = list(dataSrc = 2),
                       fixedColumns = FALSE))
    })
  })
  output$DD_TBL_dl <- downloadHandler(
    filename = function() {
      paste0("Data Dictionary", ".csv")
    },
    content = function(file) {
      write.csv(data_dict_dl(), file, row.names=FALSE)
    }
  )
  
  
  
  
}