####========================server==========##
##Required Packages:
library(shiny)
library(REDCapR)##Connect REDCap data system
library(redcapAPI)
library(dplyr)  ##Data Processing
library(DT)     ##Render a table widget
library(shinyjs) ## Use JS code in shiny
library(shinyWidgets) ## Special Shiny Widgets
library(data.table)
# library(shinyDataFilter)##Dynamic data filter
server <- function(input, output, session) {
  
  ########===Data Dict===#########
  Myproject<-reactive({
    `My Project`<-data.frame(`Project Title`=exportProjectInformation(redcapConnection(url=input$api, token=input$token))$project_title,
                             
                             Fields=length(redcapProjectInfo(redcapConnection(url=input$api, token=input$token))$meta_data$field_name),
                             Instruments=paste(nrow(redcapProjectInfo(redcapConnection(url=input$api, token=input$token))$instruments)," forms"))
  })
  output$projectinfo<-renderDT({
    datatable(Myproject(),
              rownames= FALSE,
              caption ='My Projects' ,
              options = list(dom = 't')) # only display the table, and nothing else
    
    
  })
  field<-reactive({data<-exportFieldNames(redcapConnection(url=input$api, token=input$token))$original_field_name })
  
  Event<-reactive({Event<-redcap_read(redcap_uri = input$api,
                                      token      = input$token,
                                      
                                      fields=c(field()[1]))$data})
  # Event$event_name
  # output$project_title<-renderText({
  #   info <- ""
  #   if (input$api != "" && input$token != "") {
  #     cat(paste0("API:", input$api))
  #     cat(paste0("Token:", input$token))
  #     conn <- redcapConnection(url=input$api, token=input$token)
  #     if (!is.null(conn) && !is.na(conn)) {
  #       project_info<-exportProjectInformation(conn)
  #       info <- project_info$project_title
  #     } else {
  #       info<-"Invalid connection"
  #     }
  #   }
  #   info
  # })
  
  
  # ##Page 5 Data
  # ##Data Dict
  output$Report_name<-renderUI({
    selectInput("Report_name","Report name:",c("All Data"="all",
                                               "Selected Instruments"="selected"))
  })
  
  spec<-reactive({
    
    REDCapR::redcap_metadata_read(redcap_uri = input$api,
                                  token      = input$token)$data
    
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
      choices=c(unique(spec()$field_name)),
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
      choices=c(unique(Event()$redcap_event_name)),
      selected =c(unique(Event()$redcap_event_name)) ,
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
      choices=c(unique(Event()$redcap_event_name)),
      selected =c(unique(Event()$redcap_event_name)) ,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,#When set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All).
                     noneSelectedText='Nothing selected',#The text that is displayed when a multiple select has no selected options.
                     `live-search` = TRUE#adds a search box
                     
      )
    )
    
    
  })
  DataListing <- reactive({
    if(input$Report_name=='all'){
      data <- REDCapR::redcap_read(redcap_uri = input$api,
                                   token      = input$token,
                                   
                                   raw_or_label=input$raw_label,
                                   events = input$event_all,
                                   raw_or_label_headers=input$raw_label_headers,
                                   export_data_access_groups=input$export_data_access_groups,
                                   filter_logic=input$filter_all,
                                   fields =c(field()[1],input$Columns_All))$data
    }
    else if (input$Report_name=='selected'){
      data <- REDCapR::redcap_read(redcap_uri = input$api,
                                   token      = input$token,
                                   raw_or_label=input$raw_label_ins,
                                   raw_or_label_headers=input$raw_label_headers_ins,
                                   forms=input$Instrument,
                                   events = input$event_ins,
                                   filter_logic=input$filter_ins,
                                   export_data_access_groups=input$export_data_access_groups_ins,
                                   fields =c(field()[1])
      )$data
    }
  })
  
  output$DM_TBL<-renderDT({
    
    datatable(DataListing(),
              # filter = 'top',
              rownames= FALSE,
              
              options = list(autoWidth = TRUE,
                             
                             
                             scrollX=TRUE))
  })
  ##Page 5 Resetbutton
  
  
  output$reset<-renderUI({actionButton("reset", "Reset",
                                       icon = icon("refresh"),
                                       style = 'margin-top:25px')})
  
  
  observeEvent(
    input$reset,{shinyjs::reset("Table_div")}
  )
  
  ##Page 5 Download button
  output$Data_Listing_dl_BUTTON<-renderUI({downloadButton("Data_Listing_dl","Download")})
  
  output$Data_Listing_dl <- downloadHandler(
    filename = function() {
      paste0("Data Listing", ".csv")
    },
    content = function(file) {
      write.csv(DataListing(), file, row.names=FALSE)
    }
  )
  ##PAGE 5 Reports
  output$Reports_DT<-renderDT({
    datatable(
      REDCapR::redcap_report(
        redcap_uri = input$api,
        token      = input$token,
        raw_or_label=input$raw_label_report,
        raw_or_label_headers=input$raw_label_headers_report,
        report_id  = as.numeric(input$Reports_Name))$data,
      
      rownames= FALSE,
      options = list(autoWidth = TRUE,
                     scrollX=TRUE))
  })
  Reports_DT_DL<-reactive({
    REDCapR::redcap_report(
      redcap_uri = input$api,
      token      = input$token,
      raw_or_label=input$raw_label_report,
      raw_or_label_headers=input$raw_label_headers_report,
      report_id  = as.numeric(input$Reports_Name))$data
  })
  output$Reports_dl <- downloadHandler(
    filename = function() {
      paste0("Reports", ".csv")
    },
    content = function(file) {
      write.csv(Reports_DT_DL(), file, row.names=FALSE)
    }
  )
  
  ###PAGE 5 Data Dict
  
  output$DD_TBL<-renderDT({
    datatable(
           
      
      spec(),
      
      rownames= FALSE,
      options = list(autoWidth = TRUE,
                     scrollX=TRUE))
  })
  
  output$DD_TBL_dl <- downloadHandler(
    filename = function() {
      paste0("Data Dictionary", ".csv")
    },
    content = function(file) {
      write.csv(spec(), file, row.names=FALSE)
    }
  )

 
  
  
}