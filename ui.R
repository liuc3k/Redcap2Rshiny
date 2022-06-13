####====================UI===============####
library(shiny)
library(REDCapR)      ##Connect REDCap data system
library(redcapAPI)
library(dplyr)        ##Data Processing
library(DT)           ##Render a table widget
library(shinyjs)      ## Use JS code in shiny
library(shinyWidgets) ## Special Shiny Widgets
library(data.table)
# library(shinyDataFilter)##Dynamic data filter

##Shiny Program
##################=======UI####
ui <- fluidPage(  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # shinyjs::useShinyjs(),## Called from a Shiny app's UI in order for all other shinyjs functions to work.
  navbarPage("REDCap2RShiny",
    # h4('REDCap2RShiny'),
    # textOutput(outputId = "project_title"),
    ########===API & Token ===############ 
    tabPanel("API & Token",
             
             sidebarLayout(
               sidebarPanel(h3("API & Token"),
                            width = 2,
                            textInput('api', 'Url:', value = "", width = NULL, placeholder = NULL),
                            textInput('token', 'Token:', value = "", width = NULL, placeholder = NULL)
                            
               ),
               mainPanel(
                 
                 DTOutput("projectinfo")
               )
               
             )
             
             
             
             
             
             
             
             
             
             
             
    ),
    
    
    #####======Data Listing=======#####
    tabPanel("Data",
             tabsetPanel(
               tabPanel("Data Listing",
                        sidebarLayout(
                          sidebarPanel(h2("My Reports & Exports"),
                                       width = 2,
                                       shinyjs::useShinyjs(),
                                       id = "Table_div",
                                       htmlOutput("Report_name"),
                                       conditionalPanel(condition="input.Report_name == 'all'",
                                                        selectInput("raw_label",'Raw or Label(Rows)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                        selectInput("raw_label_headers",'Raw or Label(Headers)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                        htmlOutput('Event_all'),
                                                        textInput("filter_all","Filter Logic (Only accept Raw value) "),
                                                        h6("Ex: [gender] = '1'"),
                                                        checkboxInput("export_data_access_groups",'Data Access Groups')
                                       ),
                                       
                                       conditionalPanel(condition="input.Report_name == 'selected'",
                                                        selectInput("raw_label_ins",'Raw or Label(Rows)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                        selectInput("raw_label_headers_ins",'Raw or Label(Headers)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                        htmlOutput('Event_ins'),
                                                        textInput("filter_ins","Filter Logic (Only accept Raw value)"),
                                                        h6("Ex: [gender] = '1'"),
                                                        checkboxInput("export_data_access_groups_ins",'Data Access Groups'),
                                                        htmlOutput('Instrument')),
                                       conditionalPanel(condition="input.Report_name == 'all'",htmlOutput("Columns_all")),
                                       htmlOutput("Data_Listing_dl_BUTTON"),
                                       htmlOutput("reset")
                                       
                          ),
                          
                          mainPanel(width = 10,
                                    fluidRow(
                                      column(
                                        width=12,
                                        tags$head(tags$style(".dataTables_scrollHeadInner {float:left;} ")),
                                        tags$head(tags$style(".display.dataTable.no-footer {float:left;}")),
                                        DTOutput('DM_TBL')#%>% shinycssloaders::withSpinner()
                                      )
                                    ))
                        )
               ),
               tabPanel("Reports",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       h2("Reports:"),
                                       textInput(inputId="Reports_Name", label="Report ID:",value= ""),
                                       selectInput("raw_label_report",'Raw or Label(Rows)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                       selectInput("raw_label_headers_report",'Raw or Label(Headers)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                       downloadButton('Reports_dl', 'Download')

                          ),
                          mainPanel(width = 10,
                                    tabsetPanel(type = "tabs",
                                              
                                                tabPanel("View Report From REDCap", DTOutput("Reports_DT"))
                                    )

                          ) ) ),

               tabPanel("Data Dictionary",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       h2("Data Dictionary:"),
                                       # selectInput('form_name','Form Name',),
                                       
                                       downloadButton("DD_TBL_dl","Download")),
                          
                          
                          
                          
                          
                          mainPanel(width = 10,
                                    DTOutput('DD_TBL')#%>% shinycssloaders::withSpinner() 
                          )
                          
                          
                          
                          
                          
                          
                          
                          
                        )
               )
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             )
    )
  )
)