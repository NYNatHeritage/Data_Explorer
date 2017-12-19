##Step 1- Load Libraries####
library(rintrojs)
library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(leaflet)
library(tools)
library(shinycssloaders)
library(plotly)

server<-function(input,output,session){
  
  
  #Intro Action Button----
  
  steps <- reactive(data.frame(
    #         1     2        3              4         5         6               7         8            9            10          11          12                                           13             14         15       16           17              18                                       19                                       20                                        21            22            23        24          25      26           27         28         29          30         31                 32             33           34            35           36      
    element=c(NA,"#tabs","#plotuiui","#plotuiui","#plotuiui", "#map_plot","#Region","#centerpane","#plotuiui","#map_plot","#map_plot","#Subwatershed_symbology + .selectize-control","#map_plot","#map_plot","#map_plot","#centerpane","#plotcontrols","#X_axis_Category + .selectize-control","#Y_axis_Category + .selectize-control","#Z_axis_Category + .selectize-control","#centerpane","#centerpane","#centerpane","#centerpane",NA,"#dashboard","#hplotui","#splotui","#table_ui","#tabs","#catch_headings","#catch_map","#catch_plotui","#catch_controls","#AGOLbutton","#tabs"),
    intro=c("<p style=text-align:center><img src=http://www.dec.ny.gov/images/wildlife_images/nynhplogo.gif /></p><p><h2><font color=firebrick>Welcome</font> to the <strong>Data Explorer!</strong></h2> <p><font size=2> A product of the New York Natural Heritage program.</font></p><h3> Use this tool to identify areas of highest priority for riparian restoration or protection projects.</h3><p><font size=1>Completed as a part of the <a href=http://www.nynhp.org/treesfortribsny>New York Riparian Opportunity Assessment</a>. This project was made possible by New York&#8217;s Trees for Tribs program, a program of the New York State Department of Environmental Conservation&#8217;s Division of Lands and Forests, with funding from the New York State Environmental Protection Fund.</font>",
            "<strong> <font face=verdana>Tab: Subwatersheds </strong></font> <br>We are currently looking at the <strong>Subwatersheds</strong> tab. This is a natural place to start exploring environmental condition at the level of the subwatershed (HUC 12) across New York State.",
            "<strong> <font face=verdana>Feature: Bubble Plot </strong></font> <br>This is the <strong> bubble plot</strong>.  Every <strong>point</strong> on the plot represents a single <strong>subwatershed</strong>.<p>It displays how subwatersheds are ranked relative to each other according to the scores of two <strong>ecological indicators</strong>.", 
            "<strong> <font face=verdana>Feature: Bubble Plot X Axis </strong></font> <br>The plot is divided into quadrants by two dashed lines. <br>The <strong>dashed vertical line</strong> indicates the average value of the indicator on the x axis. When the page first loads, it will be the average <strong><font color=firebrick>Ecological Stress</strong></font> score. Points to the <strong>left</strong> of this line indicate subwatersheds that have <strong>lower stress</strong> than average. Points to the <strong>right</strong> of this line indicate subwatersheds that are <strong>more stressed</strong than average.",
            "<strong> <font face=verdana>Feature: Bubble Plot Y Axis </strong></font> <br>The <strong>horizontal dashed line</strong> indicates the average value of the indicator on the y axis. When the page first loads, it will be the <strong><font color=green>Ecological Health</strong></font> score. Points falling <strong>above</strong> this line have <strong>higher</strong> ecological health scores than average, points falling <strong>below</strong> this line have <strong>lower</strong>  ecological health scores than average. <p><font size=2><em>The positions of the lines will automatically adjust to reflect the average scores of the selected region.</font></em>",
            
            "<strong> <font face=verdana>Feature: Map </strong></font> <br>The <strong>map</strong> shows the boundaries of all of New York's 1662 subwatersheds (HUC12s). Use the +/- controls in the upper left to zoom in and out. Choose preferred basemap using the radio buttons in the upper right.",
            "<strong> <font face=verdana>Feature Control: Region Selector </strong></font> <br>To focus on an area of interest, select a <strong>region</strong> from the drop down menu.<br><em><strong><font color=firebrick>Try This:</font></strong> Select the Upper Delaware region</em>",
            "<strong> <font face=verdana>Feature Control: Region Selector </strong></font> <br>The <strong>bubble plot</strong> and <strong>map</strong> are now zoomed and focused on only those subwatersheds in the selected <strong>region.</strong>",
            "<strong> <font face=verdana>Feature Control: Selected Points </strong></font> <br>To select points,click and drag over them on the bubble plot.<br><em><strong><font color=firebrick>Try This:</font></strong> Click and drag to select all the points that are <strong>above</strong> the horizontal line and to the <strong>left</strong> of the vertical line.",
            "<strong> <font face=verdana>Feature Control: Selected Points </strong></font> <br>The locations of subwatersheds selected in the <strong>bubble plot</strong> are added to the <em><font color=fuchsia>'Selected'</em></font> layer of the <strong>map</strong> and outlined in <font color=fuchsia>pink</font>.",
            "<strong> <font face=verdana>Feature Control: Selected Points </strong></font> <br>This example selected subwatersheds with low <font color=firebrick>Ecological Stress</font> scores and high <font color=green>Ecologial Health</font> scores. <p>Subwatersheds meeting both these criteria have the highest overall Comprehensive scores, which is reflected in their <font color=blue>bluer</font> <strong>Comprehensive Score</strong> colors on the map.",
            "<strong> <font face=verdana>Feature Control: Map Symbology </strong></font> <br>The Map Symbology menu controls which ecological indicator the map displays.<p><em><strong><font color=firebrick>Try This:</font></strong> Change the colors by using the Map Symbology drop down menu to select 'H9: Native Fish Richness'.",
            "<strong> <font face=verdana>Feature Control: Map Symbology </strong></font> <br><em>The colors have now changed to reflect the Native Fish Richness Score of each subwatershed.</em> ",
            "<strong> <font face=verdana>Feature Control: Map Layers </strong></font> <br><p> Turn a <strong>layer</strong> on or off by clicking on the box next to the layer name in the upper right corner of the map.<p><em><strong><font color=firebrick>Try This:</font></strong> Click once on the box next to <font color=fuchsia>'Selected'</font> to turn it off. Click the box again to make the layer reappear.",
            "<strong> <font face=verdana>Feature Control: Clicked Subwatersheds </strong></font> <br>To better understand a single subwatershed of interest, click on it on the <strong>map.</strong><p><em><strong><font color=firebrick>Try This:</font></strong> Click on one of the <font color=fuchsia>'Selected'</font> subwatersheds with a high Native Fish Richness score (darker green).</em>",
            
            #"<strong> <font face=verdana>Feature Control: Clicked Subwatersheds </strong></font> <br>When a subwatershed is clicked it is added to the <em>'Clicked'</em> layer on the <strong>map</strong> and outlined in a <strong>dashed black line</strong>.The <em>'Clicked'</em> layer only contains a single subwatershed, the most recently clicked.",
            "<strong> <font face=verdana>Feature Control: Clicked Subwatersheds </strong></font> <br>On the <strong>map</strong> the 'Clicked' subwatershed is now outlined in a <strong>dashed bold line</strong>.<p>On the <strong>bubble plot</strong> it is <strong> highlighted</strong> with a black circle .<p><em><strong><font color=firebrick>Try This:</font></strong> Click on a few subwatersheds on the <strong>map</strong> and see how the position of the <strong>black circle</strong> changes to show where each ranks on the <strong>bubble plot</strong>",
            "<strong> <font face=verdana>Feature Control: X and Y Axis Category </strong></font> <br>The information displayed in the bubble plot can be adjusted by choosing new indicators for the X and Y axis categories from the dropdown menus.",
            "<strong> <font face=verdana>Feature Control: X and Y Axis Category </strong></font> <br><em><strong><font color=firebrick>Try This:</font></strong> From the <strong>X axis Category menu</strong> select 'Comprehensive' </em>.<br> Because the <em>'Comprehensive'</em> score includes all Health and Stress indicators, using it to define the X axis will rank subwatersheds in the order of their <strong>overall condition</strong> from <strong>poorest</strong> on the left to <strong>best</strong> on the right. ",
            "<strong> <font face=verdana>Feature Control: X and Y Axis Category </strong></font> <br><em><strong><font color=firebrick>Try This:</font></strong> From the <strong>Y axis Category menu</strong> select 'H9: Native Fish Richness'</em>. <br>This will rank subwatersheds from <strong>highest</strong> Native Fish Richness at the top of the plot, to <strong>lowest</strong> at the bottom.",
            "<strong> <font face=verdana>Feature Control: Point Size/Color  Menu </strong></font> <br><em><strong><font color=firebrick>Try This:</font></strong> From the <strong>Point Size/Color menu</strong> select 'Resilience'</em>. <br>Changing the category in the Point Size/Color menu adjusts the <strong>size and color</strong> of each point.<p> In this example, <strong>more resilient</strong> subwatersheds will have <strong>larger and bluer</strong> points, while <strong>less resilient</strong> subwatersheds will be <strong>smaller and redder</strong>. <p>Adjusting the size of the points to reflect a third category can be useful when <strong>trying to decide between subwatersheds with otherwise similar ranks</strong>.",
            
            "<strong> <font face=verdana>Feature Control: X and Y Axis Category </strong></font> <p>Notice that even though the categories have changed and the points rearranged/resized, the <strong>black circle</strong> on the <strong>bubble plot</strong> still highlights the position of the Clicked subwatershed.",
            "<strong> <font face=verdana>Feature Control: Selected Points </strong></font> <br>With these new category settings it is easy quickly single out the highest (or lowest) scoring subwatersheds for a given indicator.<br><em><strong><font color=firebrick>Try This:</font></strong> Click and drag on the plot to select the subwatersheds with the highest score for Native Fish Richness (the points in the top most section of the plot).<em>",
            "<strong> <font face=verdana>Feature Control: Selected Points </strong></font> <br>The subwatersheds now highlighted would be of high priority for a project aimed at <strong>supporting native fish</strong>. We can roughly estimate from a point's <strong>position on the plot</strong> what type of conservation action might be most appropriate.",
            "<strong> <font face=verdana>Feature Control: Selected Points </strong></font> <br><strong>Protection:</strong> On the <strong>bubble plot</strong> drag the left edge of the highlight box to the edge of the dashed vertical line to highlight those subwatersheds with high native fish richness and good overall condition. These may be good candidates for protection.<br> <strong> Restoration:</strong> Selecting points on the opposite side of the plot, between the left edge and the dashed vertical line, highlights area with high native fish richness and poor overall environmental condition. These could be good targets for restoration, to improve conditions for the valuable resources that occur there by ameliorating existing sources of ecological stress.",
            "After using the map and the bubble plot to narrow down your choice of subwatersheds, you can see an overview of the habitat within each subwatershed individually using the <strong>Dashboard</strong> below.",
            "<strong> <font face=verdana>Feature: Dashboard </strong></font> <br>The <strong>Dashboard</strong> displays all of a subwatershed's <strong>Health and Stress indicator scores</strong>.",
            "<strong> <font face=verdana>Feature: Dashboard </strong></font> <br>The <strong>name</strong> of the subwatershed is displayed at the top. The <strong>length</strong> of each bar represents the <strong>score</strong> for that indicator. For some indicators where the value is 0, the bar will be absent.",
            "<strong> <font face=verdana>Feature: Dashboard </strong></font> <br>For reference, the <strong>statewide average</strong> for each indicator is always displayed as a <strong>small vertical line</strong>. If a bar extends to the <strong>right</strong> of that line, the subwatershed scored <strong>higher</strong> than average. If the bar <strong>stops before it reaches that line</strong>, the subwatershed scored <strong>below</strong> average. <p><font size=2>The whiskers to the right and left of the vertical line represent the statewide standard deviation.</font>",
            #"<strong> <font face=verdana>Feature: Dashboard </strong></font> <br>Reviewing the Health and Stress Dashboard is a good way to get ideas about what problems might need to to be alleviated or which resources could benefit from protection in a subwatershed.",
            "<strong> <font face=verdana>Feature: Data Table </strong></font> <br>To view all scores for any point selected in the plot, scroll down to the <strong>Data Table</strong>",
            
            "<strong> <font face=verdana>Tab: Catchments </strong></font> <br>Now that we have used the <strong> Subwatershed Tab</strong> to hone in on subwatershed of interest,<strong><font color=firebrick>click</font></strong> on the <strong> Catchments Tab</strong> to explore how habitat varies within that subwatershed. ",
            "<strong> <font face=verdana>Tab: Catchments </strong></font> <br>The <strong>Catchments</strong> tab automatically displays scores, at the catchment level, for the selected subwatershed, whose name and HUC 12 number are displayed at the top.<br><em>To select a subwatershed, click on it on the <strong>map</strong> in the Subwatersheds tab, or directly enter its 12 digit HUC ID number into the <strong>search box</strong> on the Catchment tab.</em> ",
            
            "<strong> <font face=verdana>Feature: Map </strong></font> <br>The catchments are displayed for only one subwatershed at a time. Catchment scores represent <strong>percentiles</strong>.They are ranked only relative to the other catchments in the subwatershed.<p>The catchment map has the same controls and features as the map on the previous tab.",
            "<strong> <font face=verdana>Feature: Plot </strong></font> <br>Points in the bubble plot represent the scores of catchments within the subwatershed.<br> The controls for the Catchment plot are the same as those on the previous tab.",
            "<strong> <font face=verdana>Feature Control: New Themes </strong></font> <br>On the Catchment tab, it is also possible to visualize data in the <strong>map</strong> and <strong>bubble plot</strong> using the <strong>Theme Scores</strong>, which are not available on the Subwatersheds tab.They are listed below the 'Overall' Scores in the dropdown menus.",
            "<strong> <font face=verdana>Feature: Shortcut to ArcGIS Online Map </strong></font> <br>There is <strong>additional information available</strong> about catchments that is more easily viewed using our <strong>online map service</strong>, including <strong>filters</strong> to focus on Agricultural/Urban/Public Lands, details on <strong>sources</strong> of water stress, water classification,etc.<p>Clicking on the <strong>'Open in ArcGIS Online' button</strong> will open the online map in a new window, and then zoom to the subwatershed selected.",
            
            "<strong> <font face=verdana>Tab: Definitions </strong></font> <br> To find more information on how a <strong>Theme</strong> or any indicator was calculated,<strong><font color=firebrick>click</font></strong> on the <strong>Definitions Tab</strong> to view a searchable table with descriptions, reasoning, and calculation methods for all indicators."
            
    ),
    #     1        2        3     4         5      6     7    8        9       10     11  12-MSym   13      14      15     16    17      18     19   20      21     22  23_a  24_b  25_c   D_26   27    28      29        30                  31     32       33    34       35    36
    position=c("bottom","bottom","top", "bottom","right","top","left","top","bottom","left","left","top","bottom","left","left","bottom","top","top","top","top","bottom","top","top","top","auto","top","top","auto","top","bottom-middle-aligned","right","right","left","top","bottom","bottom-middle-aligned")
  ))
  observeEvent(input$help, { introjs(session, options=list("skipLabel"="End Tour","doneLabel"="Tour is Complete. Click to Exit","exitOnOverlayClick"=FALSE,"hidePrev"=TRUE,"hideNext"=TRUE,steps=steps())) })
  
  #Indicator Tab----
  
  output$indicator_table<-DT::renderDataTable({
    dindc<-as.data.frame(attributes)
    #DT::datatable(dindc,rownames = FALSE) %>%
    datatable(dindc,options=list(paging = FALSE,columnDefs = list(list(targets = 1, visible = FALSE))),rownames=FALSE) %>% 
      formatStyle(c('Indicator','Code','Category'),fontWeight="bold") %>%
      formatStyle(c('Indicator','Code','Category'),'Category',
                  
                  backgroundColor = styleEqual(c("Health",'Stress','Resilience','Community','Themes',NA),c('PaleGreen','IndianRed','CornflowerBlue','Pink','MediumAquaMarine','White'))
      )
  })
  
  
  #Subwatersheds Setup----
  #Reactive means/ranges for ggplot-subwatershed----
  sample_scores<-reactive({
    
    sample_scores<-subset(original_scores,HUC6%like%input$Region)
    
  })
  
  barplot_scores<-reactive({
    barplot_scores<-melt(original_scores[original_scores$HUC_12_Nam==latest_huc(),6:27])
    barplot_scores$Category[barplot_scores$variable %in% sub_health]<-"Health"
    barplot_scores$Category[barplot_scores$variable %in% sub_stress]<-"Stress"
    barplot_scores
    
    
  })
  
  sub_map_val<-reactiveValues(reg=NULL,palette_="RdYlBu",clck_pnt=NULL)
  
  observeEvent(input$Region,{
    sub_map_val$reg<-input$Region
  })
  
  #Record Click ID SUb
  observeEvent(input$map_plot_shape_click,{
    if ({substring(input$map_plot_shape_click$id,1,4)!="HUC_"}){
      raw<-substr(input$map_plot_shape_click$id,(nchar(input$map_plot_shape_click$id)+1)-12,nchar(input$map_plot_shape_click$id))
      sub_map_val$clck_pnt<-paste0("HUC_",raw)
    }
    else{
      sub_map_val$clck_pnt<-input$map_plot_shape_click$id
    }
  })
  
  #Delete Click ID on basemape click----
  observeEvent(input$map_plot_click,{
    sub_map_val$clck_pnt<-NULL
    proxy<-leafletProxy("map_plot")
    proxy %>%
      clearGroup("Clicked")
  }) 
  
  #Title Text----  
  output$subheadingID<-renderText({
    if(sub_map_val$reg!=""){
      reg_val<-sub_map_val$reg
      reg_nam<-toTitleCase(tolower(names(region_list[region_list %in% sub_map_val$reg])))
      heading<-paste0(reg_nam, " Subwatersheds ","(HUC ",reg_val,")")
    }
    else {
      heading<-"New York Subwatersheds"
    }
    heading
  })
  
  
  
  
  x_means<-reactive({mean(sample_scores()[,input$X_axis_Category])})
  y_means<-reactive({mean(sample_scores()[,input$Y_axis_Category])})
  x_var<-reactive({as.character(input$X_axis_Category)})
  y_var<-reactive({as.character(input$Y_axis_Category)})
  
  
  ranges<-reactiveValues(x= NULL,y = NULL)
  #HUC12_selected: How to keep track of HUC12 ID that is selected by plot brush----
  test_res<-reactive({
    brushedPoints(sample_scores(),input$plot_brush)
  })
  
  HUC12_selected<-reactive({
    s<-subset(test_res(),select=c("HUC12"))
    #s$HUC12<-factor(s$HUC12)
    s
  })
  
  
  
  #Subwatershed GPlot ----
  output$plotuiui<-renderUI({
    plotOutput("plotui",brush="plot_brush")
  })
  output$plotui<- renderPlot({
    
    
    c_vector<-as.vector(substring(sub_map_val$clck_pnt,5))
    
    pc<-ggplot(data=sample_scores(),aes_string(x=input$X_axis_Category,y=input$Y_axis_Category,size=input$Z_axis_Category,colour=input$Z_axis_Category))+ geom_point(data=sample_scores())+geom_vline(xintercept=x_means(),linetype=2)+geom_hline(yintercept=y_means(),linetype=2) + scale_colour_gradientn(colours=rainbow(2))+
      guides(color=guide_legend(),size=guide_legend())+
      annotate(geom="text",label=paste("Avg.",names(spchoices[spchoices %in% input$Y_axis_Category])),x=1,y=y_means(),hjust=1,vjust=1)+
      annotate(geom="text",label=paste("Avg.",names(spchoices[spchoices %in% input$X_axis_Category])),x=x_means(),y=0,angle=90,hjust=0,vjust=-1)+
      theme_bw()+theme(plot.title=element_text(size=16,face="bold"),axis.title=element_text(size=14,face="bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      labs(title="Subwatershed Scores",subtitle="Each point represents a single subwatershed",x=names(spchoices[spchoices %in% input$X_axis_Category]),y=names(spchoices[spchoices %in% input$Y_axis_Category]))
    #print (pc)
    if (length(c_vector)){
      clicked_mask<-which(sample_scores()$HUC12 %in% c_vector)
      clicked_subset<-sample_scores()[clicked_mask,]
      pc + geom_point(data=clicked_subset,aes_string(x=input$X_axis_Category,y=input$Y_axis_Category),shape=1,color="Black",stroke=2,size=5,show.legend=FALSE)
    }
    else{
      pc
    }
    
    
  })
  
  #######Health Bar Plot GGPlot----
  
  output$hplotui<-renderUI({
    plotOutput("hplot",height=250)
  })
  output$hplot<- renderPlot({
    hucname<-state_mapData$NAME[state_mapData$HUC_12_Nam==latest_huc()]
    heading<-paste0(hucname)
    c_vector<-as.vector(substring(sub_map_val$clck_pnt,5))
    if (length(c_vector)){
      barplt<-barplot_scores()
      barplt$value<-round(as.numeric(barplt$value),3)
      t<-ggplot(data=barplt[barplt$Category=="Health",],aes(variable,value))+geom_bar(stat="identity",width=.7,aes(fill=variable),position="dodge")+coord_flip()+theme_bw(base_size=10)+theme(legend.position="none",aspect.ratio=.4)+scale_fill_manual(values=color_vals)+scale_x_discrete(labels=names(sub_health_p))+ scale_y_continuous(limits=c(0,1),name="Score",expand = c(0,0)) +
        labs(title="Health Scores",subtitle=heading)+
        geom_errorbar(data=tcomp_sd[tcomp_sd$Category=="Health",],position=position_dodge(width=0.4),aes(variable,mean,ymin=ymin,ymax=mean+sd),color="grey30",alpha=0.7,width=0)+
        geom_point(data=tcomp_sd[tcomp_sd$Category=="Health",],position=position_dodge(width=0.4),aes(variable,mean),shape=124,size=4,color="grey12")+
        theme(plot.title=element_text(size=16,face="bold"),axis.title.y=element_blank(),axis.text.y=element_text(size=11,hjust=0),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
      t
      
    }
    else{
      to<-ggplot(data=tcomp_sd[tcomp_sd$Category=="Health",],aes(variable,mean))+geom_bar(stat="identity",width=.7,aes(fill=variable),position="dodge")+coord_flip()+theme_bw(base_size=10)+theme(legend.position="none",aspect.ratio=.4)+scale_fill_manual(values=color_vals)+scale_x_discrete(labels=names(sub_health_p))+ scale_y_continuous(limits=c(0,1),name="Score",expand = c(0,0)) +
        labs(title="Health Scores",subtitle="Statewide Average")+
        geom_errorbar(data=tcomp_sd[tcomp_sd$Category=="Health",],position=position_dodge(width=0.4),aes(variable,ymin=ymin,ymax=mean+sd),color="grey30",alpha=0.7,width=0)+
        geom_point(data=tcomp_sd[tcomp_sd$Category=="Health",],position=position_dodge(width=0.4),aes(variable,mean),shape=124,size=4,color="grey12")+
        theme(plot.title=element_text(size=16,face="bold"),axis.title.y=element_blank(),axis.text.y=element_text(size=11,hjust=0),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
      to
    }
    
    
  })
  
  #######Stress Bar Plot GGPlot----
  
  output$splotui<-renderUI({
    plotOutput("splot",height=250)
  })
  output$splot<- renderPlot({
    hucname<-state_mapData$NAME[state_mapData$HUC_12_Nam==latest_huc()]
    heading<-paste0(hucname)
    c_vector<-as.vector(substring(sub_map_val$clck_pnt,5))
    if (length(c_vector)){
      barplt<-barplot_scores()
      barplt$value<-round(as.numeric(barplt$value),3)
      t<-ggplot(data=barplt[barplt$Category=="Stress",],aes(variable,value))+geom_bar(stat="identity",width=.7,aes(fill=variable),position="dodge")+coord_flip()+theme_bw(base_size=10)+theme(legend.position="none",aspect.ratio=.4)+scale_fill_manual(values=color_vals[15:length(color_vals)])+scale_x_discrete(labels=names(sub_stress_p))+ scale_y_continuous(limits=c(0,1),name="Score",expand = c(0,0)) +
        labs(title="Stress Scores",subtitle=heading)+
        geom_errorbar(data=tcomp_sd[tcomp_sd$Category=="Stress",],position=position_dodge(width=0.4),aes(variable,mean,ymin=ymin,ymax=mean+sd),color="grey30",alpha=0.7,width=0)+
        geom_point(data=tcomp_sd[tcomp_sd$Category=="Stress",],position=position_dodge(width=0.4),aes(variable,mean),shape=124,size=5,color="grey12")+
        theme(plot.title=element_text(size=16,face="bold"),axis.title.y=element_blank(),axis.text.y=element_text(size=11,hjust=0),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
      t
      
    }
    else{
      to<-ggplot(data=tcomp_sd[tcomp_sd$Category=="Stress",],aes(variable,mean))+geom_bar(stat="identity",width=.7,aes(fill=variable),position="dodge")+coord_flip()+theme_bw(base_size=10)+theme(legend.position="none",aspect.ratio=.4)+scale_fill_manual(values=color_vals[15:length(color_vals)])+scale_x_discrete(labels=names(sub_stress_p))+ scale_y_continuous(limits=c(0,1),name="Score",expand = c(0,0)) +
        labs(title="Stress Scores",subtitle="Statewide Average")+
        geom_errorbar(data=tcomp_sd[tcomp_sd$Category=="Stress",],position=position_dodge(width=0.4),aes(variable,ymin=ymin,ymax=mean+sd),color="grey30",alpha=0.7,width=0)+
        geom_point(data=tcomp_sd[tcomp_sd$Category=="Stress",],position=position_dodge(width=0.4),aes(variable,mean),shape=124,size=5,color="grey12")+
        theme(plot.title=element_text(size=16,face="bold"),axis.title.y=element_blank(),axis.text.y=element_text(size=11,hjust=0),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
      to
    }
    
    
  })
  
  
  
  
  
  
  ###Change Datatable to UI so it re-renders when click happens----
  output$table_ui<-renderUI({
    DT::dataTableOutput('plot_brushed_points')
  })
  
  
  
  output$plot_brushed_points= DT::renderDataTable({
    
    req(input$plot_brush)  
    #huc_value<-as.numeric(clicked_hucs())
    res<-brushedPoints(sample_scores(),input$plot_brush)
    
    subset_res<-res[,c("HUC12","NAME","HUC6",input$X_axis_Category,input$Y_axis_Category,input$Z_axis_Category,unlist(sschoices,TRUE,TRUE))]
    setnames(subset_res,c("HUC12","Name_of_HUC12_subwatershed","HUC6",names(sschoices[sschoices %in% input$X_axis_Category]),names(sschoices[sschoices %in% input$Y_axis_Category]),names(sschoices[sschoices %in% input$Z_axis_Category]),names(sschoices)))
    
    
    datatable(subset_res, rownames=FALSE,options = list(searching=FALSE,lengthChange=FALSE,scrollX=TRUE,scrollY='200px',scrollCollapse=TRUE,paging=FALSE,rowCallback= DT::JS(
      paste0('function(row,data){
             //Bold cells for those >=5 in the first column
             if (data[0] == "',latest_huc(),'")
             $("td",row).css("background","red");
             
  }')
      )))
    
    
    
    
})
  
  
  
  output$test_text=renderText({
    
    test_print<-paste("clicked",sub_map_val$clck_pnt,"lh",latest_huc())
    test_print
  })  
  
  output$test_text_c=renderText({
    
    test_print_c<-paste(input$symbology,target_huc_data$palette_)
    test_print_c<-paste(latest_huc()," ",input$symbology,input$map_plot_shape_click$id," ",input$tabs)
    test_print_c
  })  
  
  #Subwatershed Map_Intitalize----
  output$map_plot<-renderLeaflet({
    states<-leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,group="Imagery",options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(providers$OpenStreetMap,group="OpenStreetMap",options = providerTileOptions(noWrap = TRUE)) %>%
      #addPolygons(data=state_mapData,stroke=TRUE,color="Black",weight = 1,opacity=1,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=TRUE),layerId=state_mapData@data$HUC_12_Nam,label=~as.character(state_mapData@data$HUC_12_Nam),group="Subwatersheds") %>%
      setView(lat=42.94794,lng=-75.57487,zoom=6) %>%
      addLayersControl(
        baseGroups=c("Imagery","OpenStreetMap"),
        overlayGroups=c("Clicked","Selected","Subwatershed"),
        position=c("topright"),
        options=layersControlOptions(collapsed=FALSE))
    states
  })
  #Reactive_Set_of_Subwatershed_Polygons----
  current_subs<-reactive(
    if(!is.null(sub_map_val$reg)){
      current_subs<-subset(state_mapData,HUC6==paste(sub_map_val$reg))
    }
    else{
      current_subs<-state_mapData
    }
  )
  
  
  #Subwatershed_Map Update 1- Regions ----  
  observeEvent(input$Region,{
    #if (is.null(input$Region)){}
    if ({sub_map_val$reg==""}){
      regional_subs<-state_mapData
      lat_r<-(42.080755)
      lng_r<-(-75)
      zoom_level<-6
    }
    else{
      regional_subs<-current_subs()
      long_and_lat<-regional_subs %>% fortify() %>% select(long,lat)
      lat_r<-mean(long_and_lat$lat)
      lng_r<-mean(long_and_lat$long)+.3  
      zoom_level<-8}
    
    symbol_holder<-as.character(input$Subwatershed_symbology)
    pal<-colorBin(palette=sub_map_val$palette_,domain=regional_subs@data[,symbol_holder],n=8,pretty=TRUE)
    #pal<-colorBin(palette="RdYlBu",domain=catch_view@data[,symbol_holder],5,pretty=TRUE)
    
    sproxy<-leafletProxy("map_plot")
    sproxy %>%
      clearGroup("Subwatershed") %>%
      setView(lat=lat_r,lng=lng_r,zoom=zoom_level) %>%
      addPolygons(data=regional_subs, stroke=TRUE,color="Black",opacity=1,weight = 1,fillOpacity=0.7,fillColor=~pal(regional_subs@data[,symbol_holder]),highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=TRUE),layerId=regional_subs@data$HUC_12_Nam,label=~as.character(regional_subs@data$HUC_12_Nam),group="Subwatershed" ) %>%
      addLegend(pal=pal,values=(regional_subs@data[,symbol_holder]),title=names(sschoices[sschoices %in% input$Subwatershed_symbology]),position=c("topright"),layerId="legend")
  })
  
  #Reactive Symbology Palette Sub----
  observeEvent(input$Subwatershed_symbology,{
    selection<-(input$Subwatershed_symbology)
    
    if (any(selection %in% sub_blue)){
      sub_map_val$palette_<-"Blues"
    } 
    if (any(selection %in% sub_green)){
      sub_map_val$palette_<-"Greens"
    } 
    
    if(any(selection %in% sub_red)) {
      sub_map_val$palette_<-"Reds"
    }
    
    if(any(selection %in% sub_multi)){
      sub_map_val$palette_<-"RdYlBu"
    } 
    
  })
  
  #Delete Click ID on basemape click sub
  observeEvent(input$map_plot_click,{
    sub_map_val$clck_pnt<-NULL
    proxy<-leafletProxy("map_plot")
    proxy %>%
      clearGroup("Clicked")
  })
  
  #Subwatershed Map Update2- symbology Selection----
  observeEvent(input$Subwatershed_symbology,{
    rproxy<-leafletProxy("map_plot")
    zoom_level<-input$map_plot_zoom
    if ({sub_map_val$reg==""}){
      regional_subs<-state_mapData
    }
    
    else{regional_subs<-current_subs()}
    
    symbol_holder<-as.character(input$Subwatershed_symbology)
    pal<-colorBin(palette=sub_map_val$palette_,domain=regional_subs@data[,symbol_holder],n=8,pretty=TRUE)
    rproxy %>%
      #clearGroup("Region") %>%
      #clearControls() %>%
      addPolygons(data=regional_subs, stroke=TRUE,color="Black",opacity=1,weight = 1,fillOpacity=0.7,fillColor=~pal(regional_subs@data[,symbol_holder]),highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=TRUE),layerId=regional_subs@data$HUC_12_Nam,label=~as.character(regional_subs@data$HUC_12_Nam),group="Subwatershed" ) %>%
      addLegend(pal=pal,values=(regional_subs@data[,symbol_holder]),title=names(sschoices[sschoices %in% input$Subwatershed_symbology]),position=c("topright"),layerId="legend")
    
  })
  
  
  #Make Reactive brush Subwatersheds----
  
  brush_pnts<-reactive({
    res<-brushedPoints(sample_scores(),input$plot_brush)
    if (nrow(res)==0)
      return()
    res
  })
  
  pnts_brush<-reactive({
    if (is.null(brush_pnts()))return ()
    if (!is.null(brush_pnts())){
      tada<-brush_pnts()$HUC12
      tada
    }
  })
  
  #Subwatershed Update 3 Brushed Points ----
  observeEvent(input$plot_brush,{
    if (is.null(brush_pnts())){
      proxy<-leafletProxy("map_plot")
      proxy %>%
        clearGroup("Selected")
    }
    if (!is.null(brush_pnts())){
      
      pnts_brush_vector<-as.vector(brush_pnts()$HUC12)
      #highlight_catch<-subset(catchmentData,FEATUREID %in% cpnts_brush_vector)
      highlight_mask<-which(state_mapData$HUC12 %in% pnts_brush_vector)
      highlight_subs<-state_mapData[highlight_mask,]
      proxy<-leafletProxy("map_plot")
      proxy %>%
        clearGroup("Selected") %>%
        addPolygons(data=highlight_subs, stroke=TRUE,color="Magenta",opacity=.8,weight = 3,fillOpacity=0,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=FALSE),layerId=highlight_subs@data$HUC12,label=~as.character(highlight_subs@data$HUC_12_Nam),group="Selected" )
    }
    
  })
  #Subwatershed Update 4: Clicked Subwaterhsed----
  observeEvent(input$map_plot_shape_click,{
    click_sel<-as.vector(substring(sub_map_val$clck_pnt,5))
    if (!is.null(click_sel)){
      click_mask<-which(state_mapData$HUC12 %in% click_sel)
      click_sub<-state_mapData[click_mask,]
      proxy<-leafletProxy("map_plot")
      proxy %>%
        clearGroup("Clicked") %>%
        addPolygons(data=click_sub, stroke=TRUE,color="Black",opacity=1,weight = 5,dashArray="5,10",fillOpacity=0,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=FALSE),layerId=paste0("huc_",click_sub@data$HUC12),label=~as.character(click_sub@data$HUC_12_Nam),group="Clicked" )
    }
    
  })
  
  
  #Cachment Tab Code=================================================  
  
  
  #Keep track of the HUC12s that have been clicked or searched: start a list to hold values
  #Reactive vaues for catchments----
  target_huc_data<-reactiveValues(Clicks=list(),palette_="RdYlBu",clck_pnt=NULL,symbol="P_pre_Comp")
  
  #Every time someone clicks on a subwatershed in the main pane- add that HUC ID to the vector
  #Assign HUCid to reactive values on Map_click----
  observeEvent(input$map_plot_shape_click,{ 
    get_huc_click<-sub_map_val$clck_pnt
    if (is.null(get_huc_click))return()
    #if (is.element(get_huc_click,no_catch_ids))return()#
    else{
      huc_id<-as.vector(get_huc_click)
      if(length(s_hucids[grep(get_huc_click,s_hucids)])>0){
        target_huc_data$Clicks<-c(target_huc_data$Clicks,huc_id)
      }
    }
  })
  
  #Keep track of changes to the symbology tab
  observeEvent(input$symbology,{
    target_huc_data$symbol<-input$symbology
  }) 
  
  #latest_huc----
  #Have one variable that reports the latest HUC to be clicked or searched
  latest_huc<-reactive(target_huc_data$Clicks[length(target_huc_data$Clicks)])
  
  
  #Catchment-Map Initialize----
  #Start up the map
  output$catch_map<-renderLeaflet({
    leafmap<-leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,group="Imagery",options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(providers$OpenStreetMap,group="OpenStreetMap",options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lat=42.94794,lng=-75.57487,zoom=6) %>%
      addLayersControl(
        baseGroups=c("Imagery","OpenStreetMap"),
        overlayGroups=c("Clicked","Selected","Catchments"),
        position=c("topleft"),
        options=layersControlOptions(collapsed=FALSE))
    
    
    
  })
  #Catchment Map Update 0: add Catchments if HUC12 is clicked----
  # 
  observeEvent({input$tabs
    input$map_plot_shape_click
  },{
    if (is.element(input$map_plot_shape_click$id,no_catch_ids))return()           
    huc_id<-as.vector(latest_huc())
    gobbledeygook<-hucids[grep(huc_id,hucids)]
    if (length(gobbledeygook)==0)return ()
    huc_id<-as.vector(latest_huc())
    # sub_mask<-which(catchmentData$HUC_12_Nam %in% huc_id)
    # catch_view<-catchmentData[sub_mask,]
    #
    catch_view<-subset(catchmentData,HUC_12_Nam==paste(huc_id))
    
    symbol_holder<-isolate((target_huc_data$symbol))
    current_zoom<-isolate(input$catch_map_zoom)
    initial_zoom<-11
    long_and_lat<-catch_view %>% fortify() %>% select(long,lat)
    pal<-colorBin(palette=target_huc_data$palette_,domain=catch_view@data[,symbol_holder],5,pretty=TRUE)
    proxy<-leafletProxy("catch_map")
    proxy %>%
      clearControls() %>%
      clearGroup("Catchments") %>%
      clearGroup("Selected") %>%
      clearGroup("Clicked") %>%
      addPolygons(data=catch_view,stroke=TRUE,color="Black",weight = 1,smoothFactor=0.5,opacity=1.0,fillOpacity=0.4,fillColor=~pal(catch_view@data[,symbol_holder]),highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=TRUE),layerId=catch_view@data$FEATUREID,label=~as.character(catch_view@data$FEATUREID),group="Catchments") %>%
      setView(lat=mean(long_and_lat$lat),lng=mean(long_and_lat$long),zoom=max(current_zoom,initial_zoom)) %>%
      addLegend(pal=pal,values=(catch_view@data[,symbol_holder]),title=names(schoices[schoices %in% symbol_holder]),position=c("topleft"),layerId="legend")
    
    
    
  })
  # 
  #TextInput HUC12 to list of HUCs----
  #If someone searches for the HUC12, add that HUCID to the list
  #Check input against list of hucids to ensure nonsense is ignored
  observeEvent(input$hucButton,{
    req(input$manual_huc)
    raw_entry<-trimws(as.character(input$manual_huc),"both")
    gobbledeygook<-hucids[grep(raw_entry,hucids)]
    if (length(gobbledeygook)==0)return ()
    if (length(gobbledeygook)>0){
      huc_id<-as.vector(gobbledeygook)
      target_huc_data$Clicks<-c(target_huc_data$Clicks,huc_id)
    }
  })
  
  
  
  #Catchment Heading ID reactive----
  #Have the heading reflect the latest HUC 12 Searched
  output$headingID<-renderText({
    if(!is.null(latest_huc())){
      hucname<-state_mapData$NAME[state_mapData$HUC_12_Nam==latest_huc()]
      heading<-paste0(hucname," Subwatershed"," (",latest_huc(),")")
    }
    else {
      heading<-"Subwatershed"
    }
    heading
  })
  
  # blue_vector<-c("P_pre_R0","P_R1_TR","P_R1_TR_Ri","P_R2_TC", "P_R3_SC","P_R4_GC")
  # red_vector<-c("P_pre_S0","P_S1_DSR","P_S2_IS","P_S2_IS_Ri","P_S3_LCA","P_S3_LCA_R","P_S4_WQ","P_S5_EI","P_S6_TWI")
  # green_vector<-c("T1_CON","P_pre_H0","P_H1_CAN","P_H1_CAN_R","P_H2_NAT","P_H2_NAT_R","P_H3_BAP","P_H4_BKT","P_H5_FC","P_H5_FC_Ri","P_H6_FRN","P_H7_MFB","P_H7_MFB_R","P_H8_ES","P_H8_ES_Ri","P_H9_FD")
  # multi_vector<-c("P_pre_Comp","P_T2_WR","P_T3_RR","P_T4_ST",)
  
  
  
  #Reactive Symbology Palette Catchment----
  observeEvent(input$symbology,{
    if (length(target_huc_data$Clicks)<1){}
    else{
      selection<-(input$symbology)
      
      if (any(selection %in% blue_vector)){
        target_huc_data$palette_<-"Blues"
      } 
      if (any(selection %in% green_vector)){
        target_huc_data$palette_<-"Greens"
      } 
      
      if(any(selection %in% red_vector)) {
        target_huc_data$palette_<-"Reds"
      }
      
      if(any(selection %in% multi_vector)){
        target_huc_data$palette_<-"RdYlBu"
      } 
      
      # else{
      #   target_huc_data$palette_<-"Spectral"
      # }
    }
  })
  
  #Catchment Map Update1- symbology Selection----
  observeEvent(input$symbology,{
    proxy<-leafletProxy("catch_map")
    zoom_level<-input$catch_map_zoom
    if (length(target_huc_data$Clicks)<1){}
    
    else{
      huc_id<-latest_huc()
      
      catch_view<-subset(catchmentData,HUC_12_Nam==paste(huc_id))
      symbol_holder<-as.character(input$symbology)
      pal<-colorBin(palette=target_huc_data$palette_,domain=catch_view@data[,symbol_holder],5,pretty=TRUE)
      proxy %>%
        clearGroup("Catchments") %>%
        #clearControls() %>%
        addPolygons(data=catch_view,stroke=TRUE,color="Black",weight = 1,smoothFactor=0.5,opacity=1.0,fillOpacity=0.4,fillColor=~pal(catch_view@data[,symbol_holder]),highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=TRUE),layerId=catch_view@data$FEATUREID,label=~as.character(catch_view@data$FEATUREID),group="Catchments") %>%
        addLegend(pal=pal,values=(catch_view@data[,symbol_holder]),title=names(schoices[schoices %in% input$symbology]),position=c("topleft"),layerId="legend")
    } 
  })
  
  #Catchment Map Update 2: Manual HUC12 entry----
  observeEvent(input$hucButton,{
    req(input$manual_huc)
    proxy<-leafletProxy("catch_map")
    
    raw_entry<-trimws(as.character(input$manual_huc),"both")
    gobbledeygook<-hucids[grep(raw_entry,hucids)]
    if (length(gobbledeygook)==0)return ()
    if (length(gobbledeygook)>0){
      
      
      if (substr(raw_entry,1,4)=="HUC_"){
        huc_id<-as.vector(raw_entry)
      }
      else {
        huc_id<-as.vector(paste0("HUC_",raw_entry))
      }
      catch_view<-subset(catchmentData,HUC_12_Nam==paste(huc_id))
      zoom_level<-input$catch_map_zoom
      if (nrow(catch_view)==0)return()
      if(nrow(catch_view)>0){
        
        
        
        if(zoom_level <=7){
          zoom_level<-11
        }
        symbol_holder<-as.character(input$symbology)
        pal<-colorBin(palette=target_huc_data$palette_,domain=catch_view@data[,symbol_holder],5,pretty=TRUE)
        #pal<-colorBin(palette="RdYlBu",domain=catch_view@data[,symbol_holder],5,pretty=TRUE)
        long_and_lat<-catch_view %>% fortify() %>% select(long,lat)
        proxy %>%
          clearGroup("Catchments") %>%
          clearGroup("Selected")%>%
          clearControls() %>%
          setView(lat=mean(long_and_lat$lat),lng=mean(long_and_lat$long),zoom=zoom_level) %>%
          addPolygons(data=catch_view,stroke=TRUE,color="Black",weight = 1,smoothFactor=0.5,opacity=1.0,fillOpacity=0.4,fillColor=~pal(catch_view@data[,symbol_holder]),highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=TRUE),layerId=catch_view@data$FEATUREID,label=~as.character(catch_view@data$FEATUREID),group="Catchments") %>%
          addLegend(pal=pal,values=(catch_view@data[,symbol_holder]),title=names(schoices[schoices %in% input$symbology]),position=c("topleft"),layerId="legend")
        
      }
    } 
  })
  
  
  
  #Catchment GGplot- Reactive Scores----
  catch_scores<-reactive({
    huc_id<-latest_huc()
    subset(catchmentData@data,HUC_12_Nam==huc_id)
    
  })
  #Catchment- reactive means/ranges----
  x_means_c<-reactive({mean(catch_scores()[,input$X_axis_Category_c])})
  y_means_c<-reactive({mean(catch_scores()[,input$Y_axis_Category_c])})
  x_var_c<-reactive({as.character(input$X_axis_Category_c)})
  y_var_c<-reactive({as.character(input$Y_axis_Category_c)})
  
  #Record Click ID
  observeEvent(input$catch_map_shape_click,{
    target_huc_data$clck_pnt<-input$catch_map_shape_click$id
  })
  
  #Delete Click ID on basemape click
  observeEvent(input$catch_map_click,{
    target_huc_data$clck_pnt<-NULL
    proxy<-leafletProxy("catch_map")
    proxy %>%
      clearGroup("Clicked")
  })
  #Catchment ggplot----
  output$catch_plotui<-renderUI({
    plotOutput("catch_plot",brush="cplot_brush")
  })
  
  output$catch_plot<- renderPlot({
    
    #special_vector<-as.vector(input$catch_map_shape_click$id)
    special_vector<-as.vector(target_huc_data$clck_pnt)
    
    c_points<-ggplot(data=catch_scores(),aes_string(x=input$X_axis_Category_c,y=input$Y_axis_Category_c,size=input$Z_axis_Category_c,colour=input$Z_axis_Category_c))+ geom_point(data=catch_scores())+geom_vline(xintercept=x_means_c(),linetype=2)+geom_hline(yintercept=y_means_c(),linetype=2) + scale_colour_gradientn(colours=rainbow(2))+
      guides(color=guide_legend(),size=guide_legend())+theme(legend.direction="horizontal",legend.position="bottom")+
      labs(title="Catchment Scores",subtitle="Each point represents a single catchment",x=names(pchoices[pchoices %in% input$X_axis_Category_c]),y=names(pchoices[pchoices %in% input$Y_axis_Category_c]))+
      annotate(geom="text",label=paste("Avg.",names(pchoices[pchoices %in% input$Y_axis_Category_c])),x=1,y=y_means_c(),hjust=1,vjust=1)+
      annotate(geom="text",label=paste("Avg.",names(pchoices[pchoices %in% input$X_axis_Category_c])),x=x_means_c(),y=0,angle=90,hjust=0,vjust=-1)+
      theme_bw()+theme(plot.title=element_text(size=16,face="bold"),axis.title=element_text(size=14,face="bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
    #print (pc)
    c_points
    if (length(special_vector)){
      c_click_mask<-which(catch_scores()$FEATUREID %in% special_vector)
      catch_of_interest<-catch_scores()[c_click_mask,]
      
      c_points+geom_point(data=catch_of_interest,aes_string(x=input$X_axis_Category_c,y=input$Y_axis_Category_c),shape=1,color="Black",stroke=2,size=5,show.legend=FALSE)
      
    }
    else{
      c_points
    }
    
    
  })
  
  #Catchment selected brush----
  #Capture those points that are brushed
  cbrush_pnts<-reactive({
    cres<-brushedPoints(catch_scores(),input$cplot_brush)
    if (nrow(cres)==0)
      return()
    cres
  })
  
  cpnts_brush<-reactive({
    if (is.null(cbrush_pnts()))return ()
    if (!is.null(cbrush_pnts())){
      tada<-subset(cbrush_pnts(),select=c("FEATUREID"))
      tada
    }
  })
  
  catch_of_interest<-reactive({
    coi<-catch_scores()[catch_scores()["FEATUREID"]==input$catch_map_shape_click$id,]
    if (nrow(coi)==0)return()
    coi
  })
  
  #as.vector(clicked_hucs()[,"HUC12"])
  #Catchment Map Update 3: Outline Brushed subwatersheds----
  observeEvent(input$cplot_brush,{
    if (is.null(cpnts_brush())){
      proxy<-leafletProxy("catch_map")
      proxy %>%
        clearGroup("Selected")
    }
    if (!is.null(cpnts_brush())){
      
      cpnts_brush_vector<-as.vector(cpnts_brush()[,"FEATUREID"])
      #highlight_catch<-subset(catchmentData,FEATUREID %in% cpnts_brush_vector)
      highlight_mask<-which(catchmentData$FEATUREID %in% cpnts_brush_vector)
      highlight_catch<-catchmentData[highlight_mask,]
      proxy<-leafletProxy("catch_map")
      proxy %>%
        clearGroup("Selected") %>%
        addPolygons(data=highlight_catch, stroke=TRUE,color="Magenta",opacity=.8,weight = 3,fillOpacity=0,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=FALSE),layerId=highlight_catch@data$FEATUREID,label=~as.character(highlight_catch@data$FEATUREID),group="Selected" )
    }
    
  })
  #Catchment Map Update 4: Clicked Catchments----
  observeEvent(input$catch_map_shape_click,{
    click_sel<-as.vector(target_huc_data$clck_pnt)
    if (!is.null(click_sel)){
      click_mask<-which(catchmentData$FEATUREID %in% click_sel)
      click_catch<-catchmentData[click_mask,]
      proxy<-leafletProxy("catch_map")
      proxy %>%
        clearGroup("Clicked") %>%
        addPolygons(data=click_catch, stroke=TRUE,color="Black",opacity=1,weight = 5,dashArray="5,10",fillOpacity=0,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=FALSE),layerId=click_catch@data$FEATUREID,label=~as.character(click_catch@data$FEATUREID),group="Clicked" )
    }
    
  })
  
  
  output$image1<-renderImage({
    width=10
    height=10
    list(src="http://4.bp.blogspot.com/-2Jl96r0QP2Q/U4dTsNVSc8I/AAAAAAAABLU/2g-PEd1q7Dc/s1600/arcMap.png",contentType="image/png",width=width,height=height)
  },deleteFile = FALSE)
  
  #AGOL Link Button----
  output$AGOLbutton<-renderUI({
    
    shiny::a(h4("Open in ArcGIS Online",class ="btn btn-default action-button",
                style = "fontweight:600"), target = "_blank",
             href= paste0("https://www.arcgis.com/home/webmap/viewer.html?webmap=db102402c7bb45c5b1cf21e93db0f4e5&find=",latest_huc())
    )
  })
  
}
##END SHINY APP CODE
