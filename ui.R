shinyUI<-navbarPage("Trees for Tribs Statewide Data Explorer",id="tabs",
               
               tabPanel("Subwatersheds",value=NULL,
                        #absolutePanel(top=20,right=20,width=200,height=30,draggable=TRUE,verbatimTextOutput("test_text"),style="opacity: 0.92"),
                        ##User Interface Code
                        fluidRow(
                          column(6,h3(textOutput("subheadingID"))),column(6,selectInput("Region",label="Select a Region",c("All"="",state_regions),selectize=FALSE,selected=NULL,width="100%"),align="left")
                        ),
                        fluidRow(  
                          column(6,uiOutput("plotuiui")),column(6,withSpinner(leafletOutput("map_plot",height=500)))
                          #column(12,verbatimTextOutput("test_text")),
                        ),
                        fluidRow(
                          
                          column(2,selectInput("X_axis_Category",label="X axis Category",choices=list('Overall'=sub_overall,'Health'=sub_health,'Stress'=sub_stress,'Resilience'=sub_res),selected="S0")),
                          column(2,selectInput("Y_axis_Category",label="Y axis Category",choices=list('Overall'=sub_overall,'Health'=sub_health,'Stress'=sub_stress,'Resilience'=sub_res),selected="H0")),
                          column(2,selectInput("Z_axis_Category",label="Point Size/Color",choices=list('Overall'=sub_overall,'Health'=sub_health,'Stress'=sub_stress,'Resilience'=sub_res),selected="COMP")),
                          column(6,selectInput("Subwatershed_symbology",label="Map Symbology",choices=list('Overall'=sub_overall,'Health'=sub_health,'Stress'=sub_stress,'Resilience'=sub_res),selected="COMP",width="100%"))
                        ),
                        fluidRow(
                          column(6,uiOutput("hplotui"),style='padding:0px;'),column(6,uiOutput("splotui"),style='padding:0px;')
                          #column(12,verbatimTextOutput("test_text")),
                        ),
                        
                        fluidRow(
                          wellPanel(id="wellPanel",style="overflow-y:scroll",h4("Subwatersheds/HUC12 Selected"),uiOutput("table_ui"))
                          #tags$head(tags$style("#table_ui{height: 90vh; overflow-y: auto;}"))
                        )
                        
               ),
               tabPanel("Catchments",value="Tab_1",
                        ##User Interface Code
                        tags$head(tags$script(HTML(jscode))),
                        
                        fluidRow(column(12,h3(textOutput("headingID")))),
                        fluidRow(column(4,tagAppendAttributes(textInput("manual_huc",label=NULL,width='100%',placeholder="Enter new 12 digit HUC/subwatershed ID"),'data-proxy-click'="hucButton")),column(1,actionButton("hucButton","Pan to new subwatershed",icon=NULL,offset=0,align='left')),column(7,uiOutput("AGOLbutton"),align='right')
                        ),
                        fluidRow(
                          column(7,leafletOutput("catch_map",height=500)),column(5,uiOutput("catch_plotui"))),
                        #absolutePanel(top=550,right=100,width=200,height=30,draggable=TRUE,verbatimTextOutput("test_text_c"),style="opacity: 0.92"),
                        column(3,selectInput("symbology",label="Map Symbology",choices=list('Overall'=comp_per,'Themes'=themes_per,'Health'=health_per,'Stress'=stress_per,'Resilience'=res_per),selected="P_pre_Comp")),
                        column(3,selectInput("X_axis_Category_c",label="X axis Category",list('Overall'=comp_val,'Themes'=themes_val,'Health'=health_val,'Stress'=stress_val,'Resilience'=res_val),selected="S0")),
                        column(3,selectInput("Y_axis_Category_c",label="Y axis Category",list('Overall'=comp_val,'Themes'=themes_val,'Health'=health_val,'Stress'=stress_val,'Resilience'=res_val),selected="H0")),
                        column(3,selectInput("Z_axis_Category_c",label="Point Size/Color",list('Overall'=comp_val,'Themes'=themes_val,'Health'=health_val,'Stress'=stress_val,'Resilience'=res_val),selected="COMP"))
                        
                        
                        
               )
)


