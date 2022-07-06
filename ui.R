####====================UI===============####
library(shiny)
library(REDCapR)      ##Connect REDCap data system
#library(redcapAPI)
library(dplyr)        ##Data Processing
library(DT)           ##Render a table widget
library(shinyjs)      ## Use JS code in shiny
library(shinyWidgets) ## Special Shiny Widgets
library(data.table)
library(shinycssloaders)##Add loading animations
##Shiny Program
##################=======UI####
ui <- fluidPage(  
  #tags$style(type="text/css",
  #          ".shiny-output-error { visibility: hidden; }",
  #         ".shiny-output-error:before { visibility: hidden; }"
  #),
  
  # shinyjs::useShinyjs(),## Called from a Shiny app's UI in order for all other shinyjs functions to work.
  navbarPage("REDCap2RShiny",
             
             
             ########===API & Token ===############ 
             tabPanel("API & Token",
                      
                      sidebarLayout(
                        sidebarPanel(h3("API & Token"),
                                     width = 2,
                                     #textInput('api', 'URL:', value = "https://rc.rarediseasesnetwork.org/api/", width = NULL, placeholder = 'Please fill URL'),
                                     #textInput('token', 'TOKEN:', value = "", width = NULL, placeholder = 'Please fill TOKEN'),
                                     textInput('api', 'Url:', value = "", width = NULL, placeholder = NULL),
                                     textInput('token_file', 'Token file:', value = "", width = NULL, placeholder = NULL),
                                     textInput('token', 'Token:', value = "", width = NULL, placeholder = NULL),
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
                                                width = 2,
                                                shinyjs::useShinyjs(),
                                                # id = "Table_div",
                                                htmlOutput("Report_name"),
                                                conditionalPanel(condition="input.Report_name == 'all'",
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
                                                                               title = "1. Choose the field and specify the operator and value for the first filter. 2. Select And/Or for the next filter 3. Choose field and specify the operator and value for the next filter. EX: [age] > 30 AND [ethnic]='1'"
                                                                             )
                                                                           ), 
                                                                           placeholder ="Ex: [gender] = '1'"),
                                                                 
                                                                 
                                                                 checkboxInput("export_data_access_groups",'Data Access Groups'),
                                                                 htmlOutput("Columns_all"),
                                                                 htmlOutput("execute_all"),
                                                                 htmlOutput("reset_all"),
                                                                 br(),
                                                                 htmlOutput("Data_Listing_dl_BUTTON_all")
                                                ),
                                                
                                                conditionalPanel(condition="input.Report_name == 'selected'",
                                                                 id = "Table_selected_div",
                                                                 selectInput("raw_label_ins",'Raw or Label(Rows)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                                 selectInput("raw_label_headers_ins",'Raw or Label(Headers)?',selected = 'raw',choices=c("Raw"='raw','Label'='label')),
                                                                 htmlOutput('Event_ins'),
                                                                 textInput("filter_ins",
                                                                           
                                                                           label= tags$span(
                                                                             "Filter Logic", 
                                                                             tags$i(
                                                                               class = "glyphicon glyphicon-info-sign", 
                                                                               style = "color:#0072B2;",
                                                                               title = "1. Choose the field and specify the operator and value for the first filter. 2. Select And/Or for the next filter 3. Choose field and specify the operator and value for the next filter. EX: [age] > 30 AND [ethnic]='1'"
                                                                             )
                                                                           ), 
                                                                           placeholder ="Ex: [gender] = '1'"),
                                                                 
                                                                 checkboxInput("export_data_access_groups_ins",'Data Access Groups'),
                                                                 htmlOutput('Instrument'),
                                                                 htmlOutput("execute_selected"),
                                                                 htmlOutput("reset_selected"),
                                                                 br(),
                                                                 htmlOutput("Data_Listing_dl_BUTTON_selected")
                                                )
                                                
                                                
                                                
                                                
                                   ),
                                   
                                   mainPanel(width = 10,
                                             fluidRow(
                                               column(
                                                 width=12,
                                                 tags$head(tags$style(".dataTables_scrollHeadInner {float:left;} ")),
                                                 tags$head(tags$style(".display.dataTable.no-footer {float:left;}")),
                                                 # DTOutput('DM_TBL')#%>% shinycssloaders::withSpinner()
                                                 conditionalPanel(condition="input.Report_name == 'all'",DTOutput('DM_TBL_all')%>% shinycssloaders::withSpinner()),
                                                 conditionalPanel(condition="input.Report_name == 'selected'",DTOutput('DM_TBL_selected')%>% shinycssloaders::withSpinner())
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