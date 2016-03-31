shinyUI(navbarPage("Trees for Tribs Great Lakes Data Explorer",
      tabPanel("Critical Zones",
               fluidRow(
                 
                 column(4,selectInput("X_axis_Category",label="X axis Category",choices=(names(choices)),selected="Norm_Update_Ecological_Stress")),
                 column(4,selectInput("Y_axis_Category",label="Y axis Category",choices=(names(choices)),selected="Norm_Update_Ecological_Health")),
                 column(4,selectInput("Z_axis_Category",label="Z axis Category",choices=(names(choices)),selected="Norm_Update_Comprehensive_Score")),
                 #column(12, plotOutput("plotui",brush=brushOpts("plot_brush"))),
                 column(7,uiOutput("plotuiui")),column(5,plotOutput("map_plot",click="map_click",brush="map_brush",dblclick="map_double_click"))
                 #column(12,verbatimTextOutput("test_text")),
               ),
               fluidRow(
                 wellPanel(id="wellPanel",style="overflow-y:scroll",h4("Subwatersheds/HUC12 Selected"),uiOutput("table_ui"))
                 #tags$head(tags$style("#table_ui{height: 90vh; overflow-y: auto;}"))
               )
      ),
      tabPanel("Critical Locations")
      )
)
