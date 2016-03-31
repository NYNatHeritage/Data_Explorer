##Step 1- Load Libraries

library(shiny)
library(ggplot2)
library(data.table)
#library(ggvis)
library(DT)
#library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
#require(rgeos)
library(maptools)

	options(scipen=999)  #### tgh mod

###Server Side Code
shinyServer(function(input, output) {
  dat<-sample_scores
  map_data<-simple_huc_12_watershedsDF
  #sample_scores=read.csv(file="D:\\GIS Projects\\TreesforTribs\\HUC12_Watershed_Scores.csv",sep=",")
  #sample_scores$HUC12<-as.factor(sample_scores$HUC12)
  #sample_scores$HUC12<-paste("0",sample_scores$HUC12,sep="")
  x_means<-reactive({mean(sample_scores[,input$X_axis_Category])})
  y_means<-reactive({mean(sample_scores[,input$Y_axis_Category])})
  x_var<-reactive({as.character(input$X_axis_Category)})
  y_var<-reactive({as.character(input$Y_axis_Category)})
  
  ranges<-reactiveValues(x= NULL,y = NULL)
  
  test_res<-reactive({
    brushedPoints(sample_scores,input$plot_brush)
  })
  
  HUC12_selected<-reactive({
    s<-subset(test_res(),select=c("HUC12"))
    #s$HUC12<-factor(s$HUC12)
    s
  })
  
  clicked_res<-reactive({
    
    vector<-as.vector(HUC12_selected()[,"HUC12"])
    selected_mask<-which(simple_huc_12_watershedsDF$HUC12 %in% vector)
    selected_subset<-simple_huc_12_watershedsDF[selected_mask,]
    nearPoints(selected_subset,input$map_click,"long","lat",threshold=10,maxpoints=1)
  })
  
  clicked_hucs<-reactive({
    c<-subset(clicked_res(),select=c("HUC12"))
    c
  })
  
  output$plotuiui<-renderUI({
    plotOutput("plotui",brush="plot_brush")
  })
  output$plotui<- renderPlot({
    c_vector<-as.vector(clicked_hucs()[,"HUC12"])
    
    
    pc<-ggplot(data=sample_scores,aes_string(x=input$X_axis_Category,y=input$Y_axis_Category,size=input$Z_axis_Category,colour=input$Z_axis_Category))+ geom_point(data=sample_scores)+geom_vline(xintercept=x_means())+geom_hline(yintercept=y_means()) +
      guides(color=guide_legend(),size=guide_legend())+theme(legend.direction="horizontal",legend.position="bottom")
    #print (pc)
    if (length(c_vector)){
      clicked_mask<-which(sample_scores$HUC12 %in% c_vector)
      clicked_subset<-sample_scores[clicked_mask,]
      pc + geom_point(data=clicked_subset,aes_string(x=input$X_axis_Category,y=input$Y_axis_Category),colour="red",size=3)
    }
    else{
      pc
    }
    
    
  })
  
  ###Change Datatable to UI so it re-renders when click happens
  output$table_ui<-renderUI({
    DT::dataTableOutput('plot_brushed_points')
  })
  
 
  
  output$plot_brushed_points= DT::renderDataTable({
    
    req(input$plot_brush)  
    #huc_value<-as.numeric(clicked_hucs())
    res<-brushedPoints(sample_scores,input$plot_brush)
    #subset_res<-subset(res,select=c(HUC12,Ecological_Health,Ecological_Stress,Comprehensive_Score))
    #print (subset_res)
    #test<-paste("HUC12","Name","County","HUC6",sep=",")
    subset_res<-subset(res,select=c(HUC12,Name,County,HUC6,Norm_Update_Ecological_Stress,Norm_Update_Ecological_Health,Norm_Update_Comprehensive_Score))
    setnames(subset_res,c("HUC12","Name","County","HUC6","Ecological Stress","Ecological Health","Comprehensive"))
    datatable(subset_res, options = list(lengthChange=FALSE,scrollY='200px',scrollCollapse=TRUE,paging=FALSE,rowCallback= DT::JS(
      paste0('function(row,data){
             //Bold cells for those >=5 in the first column
             if (data[1] == "',clicked_hucs()$HUC12,'")
             $("td",row).css("background","red");
             
             }')
      )))
    
   
    
    
  })
  

  
  output$brush_info<-renderPrint({   
    cat("input$map_click:\n")
    str(input$map_click)
  })
  
  
  output$test_text=renderPrint({
    #     s=input$plot_brushed_points_rows_selected
    #     sample_scores[s,"HUC12"]
    req(input$map_click)
    isolate(clicked_hucs())
    
  })  
  
  observeEvent(input$map_double_click,{
    brush<-input$map_brush
    if (!is.null(brush)){
      ranges$x<-c(brush$xmin,brush$xmax)
      ranges$y<-c(brush$ymin,brush$ymax)
    } else{
      ranges$x<-NULL
      ranges$y<-NULL
    }
  })
  
  output$map_plot<-renderPlot({
    
    
	vector<-as.vector(HUC12_selected()[,"HUC12"])
    
    ggwatershed<-ggplot(data=map_data,aes(x=long,y=lat,group=group))+geom_polygon()+
      coord_cartesian(xlim=ranges$x,ylim=ranges$y) + labs(x = "UTM X", y= "UTM Y") #### tgh mod
    if (length(vector)){
      selected_mask<-which(simple_huc_12_watershedsDF$HUC12 %in% vector)
      selected_subset<-simple_huc_12_watershedsDF[selected_mask,]
      ggwatershed+geom_polygon(data=selected_subset,color="black",fill="red")
      
    }
    else{
      ggwatershed
    }
    
    #print (ggwatershed)
    
    
    
    
  })
})

##END SHINY APP CODE
