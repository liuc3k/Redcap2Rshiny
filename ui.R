####====================UI===============####
library(shiny)
library(REDCapR)      ##Connect REDCap data system
library(redcapAPI)
library(dplyr)        ##Data Processing
library(DT)           ##Render a table widget
library(shinyjs)      ## Use JS code in shiny
library(shinyWidgets) ## Special Shiny Widgets
library(data.table)
library(shinycssloaders)##Add loading animations
##Shiny Program
##################=======UI####
ui <- fluidPage(  
  
  # shinyjs::useShinyjs(),## Called from a Shiny app's UI in order for all other shinyjs functions to work.
  navbarPage("REDCap2RShiny",
             
             
             ########===API & Token ===############ 
             tabPanel("API & Token",
                      
                      sidebarLayout(
                        sidebarPanel(h3("API & Token"),
                                     width = 2,
                                     htmlOutput("projectid"),                                     
                                     #textInput('api', 'Url:', value = "", width = NULL, placeholder = NULL),
                                     #textInput('token_file', 'Token file:', value = "", width = NULL, placeholder = NULL),
                                     #textInput('token', 'Token:', value = "", width = NULL, placeholder = NULL),
                                     htmlOutput("execute_info")
                                     
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Project Info", DTOutput("projectinfo")%>% shinycssloaders::withSpinner()),
                                      tabPanel("Instruction",tags$iframe(style="height:1000px; width:1500px; scrolling=yes", src="Instruction.pdf"))
                                      
                          )            
                          
                          
                          
                        )
                        
                      )
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
             ),
             
             
             #####======Data Listing=======#####
             tabPanel("Data",
                      tabsetPanel(
                        tabPanel("Data Listing",
                                 sidebarLayout(
                                   sidebarPanel(h2("My Reports & Exports"),
                                                width = 3,
                                                shinyjs::useShinyjs(),
                                                
                                                
                                                
                                                
                                                id = "Table_all_div",
                                                selectInput("raw_label",'Raw or Label(Rows)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                selectInput("raw_label_headers",'Raw or Label(Headers)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                htmlOutput('Event_all'),
                                                textInput("filter_all",
                                                          
                                                          label= tags$span(
                                                            "Filter Logic", 
                                                            tags$i(
                                                              class = "glyphicon glyphicon-info-sign", 
                                                              style = "color:#0072B2;",
                                                              title = "1. Choose the field and specify the operator and value for the first filter. 2. Advanced logic (and, or) and filtering (=, not=, <, <=,>,>=,
contains)
 3. Choose field and specify the operator and value for the next filter. EX: [age] > 30 AND [ethnic]='1'"
                                                            )
                                                          ), 
                                                          placeholder ="Ex: [gender] = '1'"),
                                                
                                                
                                                checkboxInput("export_data_access_groups",'Data Access Groups'),
                                                htmlOutput("Columns_all"),
                                                htmlOutput('Instrument_all'),
                                                htmlOutput("execute_all"),
                                                htmlOutput("reset_all"),
                                                br(),
                                                htmlOutput("Data_Listing_dl_BUTTON_all")
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                   ),
                                   
                                   mainPanel(width = 9,
                                             fluidRow(
                                               column(
                                                 width=12,
                                                 tags$head(tags$style(".dataTables_scrollHeadInner {float:left;} ")),
                                                 tags$head(tags$style(".display.dataTable.no-footer {float:left;}")),
                                                 
                                                 
                                                 DTOutput('DM_TBL_all')%>% shinycssloaders::withSpinner()
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
                                                htmlOutput('execute_report'),
                                                br(),
                                                downloadButton('Reports_dl', 'Download')
                                                
                                   ),
                                   mainPanel(width = 10,
                                             tabsetPanel(type = "tabs",
                                                         
                                                         tabPanel("View Report From REDCap", DTOutput("Reports_DT")%>% shinycssloaders::withSpinner())
                                             )
                                             
                                   ) ) ),
                        
                        tabPanel("Data Dictionary",
                                 sidebarLayout(
                                   sidebarPanel(width = 2,
                                                h2("Data Dictionary:"),
                                                
                                                htmlOutput("Instrument_dict"),
                                                htmlOutput("execute_dict"),
                                                br(),
                                                downloadButton("DD_TBL_dl","Download")),
                                   
                                   
                                   
                                   
                                   
                                   mainPanel(width = 10,
                                             DTOutput('DD_TBL')%>% shinycssloaders::withSpinner() 
                                   )
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                 )
                        )
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                      )
             )
  )
)