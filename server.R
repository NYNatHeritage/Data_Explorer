##Step 1- Load Libraries####

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

shinyServer<-function(input,output){
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
        overlayGroups=c("Clicked","Brushed","Subwatershed"),
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
        clearGroup("Brushed")
    }
    if (!is.null(brush_pnts())){
      
      pnts_brush_vector<-as.vector(brush_pnts()$HUC12)
      #highlight_catch<-subset(catchmentData,FEATUREID %in% cpnts_brush_vector)
      highlight_mask<-which(state_mapData$HUC12 %in% pnts_brush_vector)
      highlight_subs<-state_mapData[highlight_mask,]
      proxy<-leafletProxy("map_plot")
      proxy %>%
        clearGroup("Brushed") %>%
        addPolygons(data=highlight_subs, stroke=TRUE,color="Magenta",opacity=.8,weight = 3,fillOpacity=0,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=FALSE),layerId=highlight_subs@data$HUC12,label=~as.character(highlight_subs@data$HUC_12_Nam),group="Brushed" )
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
    else{
      huc_id<-as.vector(get_huc_click)
      if(length(hucids[grep(get_huc_click,hucids)])>0){
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
        overlayGroups=c("Clicked","Brushed","Catchments"),
        position=c("topleft"),
        options=layersControlOptions(collapsed=FALSE))
    
    
    
  })
  #Catchment Map Update 0: add Catchments if HUC12 is clicked----
  # 
  observeEvent({input$tabs
    input$map_plot_shape_click
  },{
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
      clearGroup("Brushed") %>%
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
          clearGroup("Brushed")%>%
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
        clearGroup("Brushed")
    }
    if (!is.null(cpnts_brush())){
      
      cpnts_brush_vector<-as.vector(cpnts_brush()[,"FEATUREID"])
      #highlight_catch<-subset(catchmentData,FEATUREID %in% cpnts_brush_vector)
      highlight_mask<-which(catchmentData$FEATUREID %in% cpnts_brush_vector)
      highlight_catch<-catchmentData[highlight_mask,]
      proxy<-leafletProxy("catch_map")
      proxy %>%
        clearGroup("Brushed") %>%
        addPolygons(data=highlight_catch, stroke=TRUE,color="Magenta",opacity=.8,weight = 3,fillOpacity=0,highlightOptions=highlightOptions(color="white",weight=2,fillOpacity=0,bringToFront=FALSE),layerId=highlight_catch@data$FEATUREID,label=~as.character(highlight_catch@data$FEATUREID),group="Brushed" )
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
