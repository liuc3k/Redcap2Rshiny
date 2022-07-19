####========================server==========##
##Required Packages:
library(rlang)
library(shiny)
library(REDCapR)##Connect REDCap data system
library(dplyr)  ##Data Processing
library(DT)     ##Render a table widget
library(shinyjs) ## Use JS code in shiny
library(shinyWidgets) ## Special Shiny Widgets
library(data.table)
library(redcapAPI)
##External source
source('www/redcap_read_pyl.R')
source('www/redcap_report_pyl.R')
source('www/functions.R')
Sys.setenv(REDCAP_BYPASS_SANITIZE_TOKEN=1)
server <- function(input, output, session) {
  ###RDCRN REDCap API Gateway authentication
  Sys.setenv(TZ = "America/New_York")
  
  
  ##Projectid
  PID<-reactive({
    formData <- list(#"token"=input$token,
      "token"=TOKEN(),
      content='project',
      format='json',
      returnFormat='json'
    )
    response <- httr::POST("https://appapi.int.rdcrn.org/redcap/", body = formData, encode = "form")
    result <- httr::content(response)
    result
    table<-unlist(result)
    df <- data.frame(matrix(unlist(result), nrow=length(result), byrow=TRUE))
    setNames(as.character(df$X1),df$X2)
  })
  #choices_projectid<-reactive({ setNames(as.character(PID()$X1),PID()$X2) })
  output$projectid<-renderUI({
    selectInput("ProjectID","Project:",selected ='',c(PID()))
  })
  URL<-reactive({paste0("https://appapi.int.rdcrn.org/redcap/pid/",input$ProjectID)})
  TOKEN<-reactive({input_token <- readLines(token_file <- "/home/secrets/token")})
  
  
  # /home/secrets/token
  #url <- "https://devrc.rarediseasesnetwork.org/api/"
  #url <- "https://appapi.int.rdcrn.org/redcap/pid/105"
  #url <- "https://appapi.int.rdcrn.org/redcap/pid/"
  #url<-reactive({paste0("https://appapi.int.rdcrn.org/redcap/pid/",input$ProjectID)})
  
  #observe({
  #url<-"https://appapi.int.rdcrn.org/redcap/pid/"
  #updateTextInput(session, "api", value=paste0("https://appapi.int.rdcrn.org/redcap/pid/",input$ProjectID))
  #})
  
  #token_file <- "/home/secrets/token"
  #updateTextInput(session, "token_file", value=token_file)
  
  #input_token <- readLines(token_file)
  #updateTextInput(session, "token", value=input_token)
  
  #if (!grepl("[A-Za-z0-9]",substring(input_token, 1, 1))) {
  #token <- readLines(input_token)
  #}
  
  reactive({
    cat("token_file update")
    ###token_file <- input$token_file
    token_file <- "/home/secrets/token"
    if (file != "") {
      input_token <- readLines(token_file)
      #updateTextInput(session, "token", value=input_token)
      token_reread_sec <- 60
      invalidateLater(token_reread_sec*1000, session)
      
    }
  })  
  Api_key<-reactive({redcapConnection(url= URL(), token=TOKEN())})
  
  
  
  ########===Data Dict===#########
  project_title<-reactive({
    ##validate
    #validate(need(input$token!='' , 'Please fill Token!'))
    #validate(need(input_token='' , 'Please fill Token!'))
    formData <- list(#"token"=input$token,
      "token"=TOKEN(),
      content='project',
      format='csv',
      returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    #response <- httr::POST(url=url, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Fields_num<-reactive({
    
    formData <- list("token"=TOKEN(),
                     #"token"=input_token,
                     content='exportFieldNames',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    #response <- httr::POST(url=url, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Fields_label<-reactive({
    
    formData <- list("token"=TOKEN(),
                     #"token"=input_token,
                     content='metadata',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    
    result <- httr::content(response)
    
    setNames(as.character(result$field_name) ,result$field_label)
  })
  Instruments_num<-reactive({
    ##validate
    #validate(need(length(input$token)!='' , 'Please fill Token!'))
    #validate(need(input_token='' , 'Please fill Token!'))
    formData <- list("token"=TOKEN(),
                     #"token"=input_token,
                     content='instrument',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    #response <- httr::POST(url=url, body = formData, encode = "form")
    result <- httr::content(response)
  })
  ID<-reactive({
    ##validate
    #validate(need(length(input$token)!='' , 'Please fill Token!'))
    #validate(need(input_token='' , 'Please fill Token!'))
    formData <- list("token"=TOKEN(),
                     #"token"=input_token,
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
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    #response <- httr::POST(url=url, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Myproject<-reactive({
    ##validate
    #validate(need(length(input$token)!='' , 'Please fill Token!'))
    #validate(need(input_token='' , 'Please fill Token!'))
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
      #validate(need(length(input$token)!='' , 'Please fill Token!'))
      #validate(need(input_token='' , 'Please fill Token!'))
      datatable(Myproject(),
                rownames= FALSE,
                #caption ='My Projects' ,
                options = list(dom = 't')) # only display the table, and nothing else
      
    })  
  })
  
  
  
  Event<-reactive({
    formData <- list("token"=TOKEN(),
                     #"token"=input_token,
                     content='event',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    #response <- httr::POST(url=url, body = formData, encode = "form")
    result <- httr::content(response)
  })
  Event_label<-reactive({
    formData <- list("token"=TOKEN(),
                     #"token"=input_token,
                     content='event',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    result <- httr::content(response)
    choices = setNames(as.character(result$unique_event_name),result$event_name)
  })
  
  
  ### Data
  
  #output$Report_name<-renderUI({
  #selectInput("Report_name","Report name:",c("Whole Dataset"="whole",
  #                                           "Customized Dataset"="all",
  #                                           "Selected Instruments"="selected"))
  #})
  
  spec<-reactive({
    
    #formData <- list("token"=TOKEN(),
    
    #content='metadata',
    #format='csv',
    #returnFormat='json'
    #)
    #response <- httr::POST(url=URL(), body = formData, encode = "form")
    #result <- httr::content(response)
    exportMetaData(Api_key())
    
  })
  Instrument_label<-reactive({
    formData <- list("token"=TOKEN(),
                     
                     content='instrument',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    result <- httr::content(response)
    setNames(as.character(result$instrument_name ),result$instrument_label)
  }) 
  Instrument<-reactive({
    formData <- list("token"=TOKEN(),
                     
                     content='instrument',
                     format='csv',
                     returnFormat='json'
    )
    response <- httr::POST(url=URL(), body = formData, encode = "form")
    result <- httr::content(response)
    
  }) 
  output$Instrument_all<-renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "Instrument_All",
      label = "Instruments:",
      selected ='',
      choices=c(Instrument_label()),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All &   Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
                     
      )
    )
  })
  select_fields<-reactive({
    select_fields<-c(spec()%>%
                       filter(form_name==input$Instrument)%>%
                       select(field_name))
  })
  ###Ref:https://stackoverflow.com/questions/54409698/get-the-group-label-from-a-grouped-list-of-choices-in-selectinput-rshiny
  field_instrument<-reactive({
    data<-tibble(field_name=c(spec()$field_name),
                 instrument_name=c(spec()$form_name))
    
    data1<-data%>%
      left_join(Instrument(),by='instrument_name')
    
    
  })
  
  
  output$Columns_all<-renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "Columns_All",
      label = "Fields:",
      choices=split(field_instrument()$field_name,field_instrument()$instrument_label),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All &   Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
                     
      )
    )
    
    
  })
  output$Event_all<-renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "event_all",
      label = "Events:",
      choices= c(Event_label()),      
      selected =c(Event_label()),  
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
      #choices=c(unique(Event()$unique_event_name)),
      choices= c(Event_label()),
      #selected =c(unique(Event()$unique_event_name)) ,
      selected =c(Event_label()),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
      )
    )
    
    
  })
  
  
  
  ##Execute button for Customized dataset
  v <- reactiveValues(doTable_all = FALSE)
  
  observeEvent(input$execute_button_all, {
    
    v$doTable_all <- input$execute_button_all
  })
  
  
  DataListing_all<-reactive({
    if (v$doTable_all == FALSE) return()
    
    isolate({
      
      data <- redcap_read_large(     redcap_uri = URL(),
                                     token      = TOKEN(),
                                     batch_size=50L,
                                     raw_or_label=input$raw_label,
                                     events = c(input$event_all),
                                     forms=c(input$Instrument_All),
                                     raw_or_label_headers=input$raw_label_headers,
                                     export_data_access_groups=input$export_data_access_groups,
                                     filter_logic=input$filter_all,
                                     fields =c(Fields_num()$original_field_name[1],input$Columns_All)
                                     
      )$data
      
      
    })
  })
  output$DM_TBL_all <- renderDT({
    
    datatable(DataListing_all(),rownames= FALSE,options = list(autoWidth = TRUE,scrollX=TRUE))
  })
  
  
  
  ## Reset button for Customized data 
  output$reset_all<-renderUI({actionButton("reset_all", "Reset",
                                           #icon = icon("refresh"),
                                           icon=icon('glyphicon glyphicon-refresh'),
                                           class='glyphicon glyphicon-refresh',
                                           style = 'margin-top:25px')})
  
  
  observeEvent(
    input$reset_all,{shinyjs::reset("Table_all_div")}
  )
  
  ## Download button for Customized data 
  data_listing_dl_all<-reactive({
    data <- sapply(DataListing_all(), as.character)
    data[is.na(data)] <- ""   # Replace NA with blank
    data                      # Print updated data frame
  })
  output$Data_Listing_dl_BUTTON_all<-renderUI({downloadButton("Data_Listing_dl_all","Download")})
  
  output$Data_Listing_dl_all <- downloadHandler(
    filename = function() {
      paste0(Sys.time(),"Data Listing", ".csv")
    },
    content = function(file) {
      
      fwrite( data_listing_dl_all(), file, row.names=FALSE)
    }
  )
  
  ### Execute
  output$execute_all<-renderUI({
    actionButton("execute_button_all", "Execute", icon=icon('glyphicon glyphicon-play'),class='glyphicon glyphicon-play')
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
        redcap_report_pyl(
          redcap_uri = URL(),
          token      = TOKEN(),
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
      formData <- list("token"=TOKEN(),
                       content='report',
                       format='csv',
                       report_id=as.numeric(input$Reports_Name),
                       csvDelimiter='',
                       rawOrLabel=input$raw_label_report,
                       rawOrLabelHeaders=input$raw_label_headers_report,
                       exportCheckboxLabel='false',
                       returnFormat='json'
      )
      response <- httr::POST(URL(), body = formData, encode = "form")
      result <- httr::content(response)
      data <- sapply(result, as.character)
      data[is.na(data)] <- ""   # Replace NA with blank
      data                      # Print updated data frame
    })
  })
  output$Reports_dl <- downloadHandler(
    filename = function() {
      paste0(Sys.time(),"Reports", ".csv")
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
      
      validate(need(!(is.na(input$form_dict)) , 'Please select instrument forms!')) 
      
      spec()
    })
  })
  output$Instrument_dict<-renderUI({
    #selectInput("form_dict","Instrument:",selected ='',c(unique(spec()$form_name)))
    shinyWidgets::pickerInput(
      "form_dict",
      "Instrument:",
      selected =c(Instrument_label()) ,
      choices=c(Instrument_label()),
      
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
      paste0(Sys.time(),"Data Dictionary", ".csv")
    },
    content = function(file) {
      write.csv(data_dict_dl(), file, row.names=FALSE)
    }
  )
  
  
  
  
}