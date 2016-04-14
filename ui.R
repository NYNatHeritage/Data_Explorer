shinyUI<-navbarPage("Trees for Tribs Great Lakes Data Explorer",
    tabPanel("Critical Zones",
        ##User Interface Code
        fluidRow(
            column(3,selectInput("Region",label="Region",c(All="",region_list),selectize=FALSE)),
            column(3,selectInput("X_axis_Category",label="X axis Category",choices=(names(choices)),selected="Norm_Ecological_Stress")),
            column(3,selectInput("Y_axis_Category",label="Y axis Category",choices=(names(choices)),selected="Norm_Ecological_Health")),
            column(3,selectInput("Z_axis_Category",label="Z axis Category",choices=(names(choices)),selected="Norm_Comprehensive_Score")),
            column(7,uiOutput("plotuiui")),column(5,plotOutput("map_plot",click="map_click",brush="map_brush",hover=hoverOpts(id="map_hover",nullOutside=FALSE),dblclick="map_double_click"))
            #column(12,verbatimTextOutput("test_text")),
        ),
        absolutePanel(top=550,right=100,width=200,height=30,draggable=TRUE,verbatimTextOutput("test_text"),style="opacity: 0.92"),
        fluidRow(
             wellPanel(id="wellPanel",style="overflow-y:scroll",h4("Subwatersheds/HUC12 Selected"),uiOutput("table_ui"))
             #tags$head(tags$style("#table_ui{height: 90vh; overflow-y: auto;}"))
        )
    ),
    tabPanel("Critical Locations")
)
