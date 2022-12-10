library(shiny)
library(shinythemes)
library(plotly)
library(shinycssloaders)


library(ggplot2)
library(data.table)
library(dplyr)
library(heatmaply)
#library(trelliscopejs)

######
#source("R/tab2_gene_pathway_da_fraction_plot.R")
#source("R/tab2_gene_pathway_da_fraction_data_processing.R")

#source("R/tab2_metabolite_pathway_da_fraction_plot.R")
#source("R/tab2_metabolite_pathway_da_fraction_data_processing.R")

#source("R/tab2_gene_metabolite_da_pathway_heatmap.R")

source("R/tab3_multimodal_correlation_data_processing.R")
source("R/tab3_multimodal_correlation_plot.R")


##### Load dataset #####
#masterData<-fread("data/MasterMapping_MetImmune_03_16_2022_revised_dataset_name.csv")
masterData<-fread("data/MasterMapping_MetImmune_03_16_2022.csv")


cancerTypeList<-c("RC20","RC18MR","RC18Flow","RC12",
                  "BRCA_Terunuma","BRCA_Tang","PRAD",
                  "GBM","DLBCL","PDAC",
                  "LiCa1","LiCa2","OV","COAD","HCC")

#cancerTypeList<-c("RC20","RC18MR",
#                            "BRCA_Terunuma","PRAD",
#                            "GBM","PDAC",
#                            "COAD")

cancerTypeWithNormalList<-c("RC20","RC18MR",
                  "BRCA_Terunuma","PRAD",
                  "GBM","PDAC",
                  "COAD")

#immuneOptions<-data.frame(value=c("ImmuneScore","TIS","PD1"))

#testOptions<-c("ImmuneScore","TIS","PD1")
#names(testOptions)<-c("ImmuneScore","TIS","PD1")


metabolite_name_dat<-readRDS("data/selection_options/pancancer_metabolite_annotations.rds")
metabolite_name_options<-metabolite_name_dat$H_name
names(metabolite_name_options)<-metabolite_name_dat$H_name

gene_name_dat<-readRDS("data/selection_options/pancancer_gene_annotations.rds")
gene_name_options<-gene_name_dat$index
names(gene_name_options)<-gene_name_dat$index


immune_signature_dat<-readRDS("data/selection_options/pancancer_immune_signature_annotations.rds")
immune_signature_options<-immune_signature_dat$index
#names(immune_signature_options)<-gsub("\\.","_",immune_signature_dat$index)
names(immune_signature_options)<-immune_signature_dat$index

tab3_content_choices<-list()
tab3_content_choices[["metabolite"]]<-metabolite_name_options
tab3_content_choices[["gene"]]<-gene_name_options
tab3_content_choices[["tme_signature"]]<-immune_signature_options




#####



ui <- navbarPage(
  "Cancer Atlas of Metabolic Profiles (cAMP)",
  
  theme=shinytheme("flatly"),
  
  ##### Start of navbar - Home tab ###
  
  tabPanel("Home",
           
      fluidPage(
        fluidRow(
          
          column(8,
                 
                 HTML("
                                     
                       <section class='banner'>
                       <h2><strong>Introduction</strong></h2>
                       <p>
                       The Cancer Atlas of Metabolic Profiles (cAMP) is a curated collection of
                       matched metabolomic and transcriptomic data from public and in-house studies.
                       Data in this collection has been harmonized for integrative, 
                       cross-study analysis. Currently, the cAMP dataset comprises the largest 
                       harmonized collection of multimodal matched metabolic and transcriptomic data on primary tumor specimens.
                       </p>
                       </section>
                       
                       <br>
                       <section class='tab3_section'>
                       <h4><strong>Data overview</strong></h4>
                       <p>
                       We have curated a collection of matched metabolomic and 
                       transcriptomic data from 988 specimens in 11 cancer
                       types across 15 independently collected studies. Seven out of 15 studies also
                       include data of tumor and matched normal samples.
                       </p>
                       </section>
                       
                       <br>
                       
                       <section class='tab3_section'>
                       <h4><strong>Multimodal correlation</strong></h4>
                       <p>
                       In this tab, cAMP produces scatter plots from selected metabolite, 
                       gene or TME signature attributes for users to explore the multimodal correlation.      
                       </p>
                       </section>
                       
                       
                       
                ") # end of HTML()
                  
          ) # end of column()
          
        ) # end of fluidRow
      ) # end of fluidPage()     
           
  ), ### End of navbar - Browse tab ###
  
  
  ### Start of navbar - Data Overview tab ###
  
  tabPanel("Data overview",
           
     fluidPage(
       # Application title
       #titlePanel("Hello Shiny!"),
       
       sidebarLayout(
         
         # Sidebar with a slider input
         sidebarPanel(
           
           checkboxGroupInput("sample_type", 
                              h5("Sample type"), 
                              choices = list( 
                                             "Tumor" = c("Tumor"), 
                                             "Normal" = c("Normal")),
                              selected = "Tumor")
           
           #submitButton("Update Filter")
           
           
          ),
         
         # Show a plot of the generated distribution
         mainPanel(
           "Summary",
           plotlyOutput("tab1_summaryPlot"),
           br(),
           br(),
           DT::dataTableOutput("tab1_dataSummaryTable")
         )
       )
     
             
     )
           
  ), ### End of navbar - Browse tab ###
  
  
  
  ##### Start of navbar - Correlation tab ###
  
  tabPanel("Multimodal correlation",
           fluidPage(
             # Application title
             #titlePanel("black: tumor samples, blue: normal samples"),
             
             sidebarLayout(
               
               # Sidebar with a slider input
               sidebarPanel(
                 
                 
                 #uiOutput("tab3_select_var1"),
                 
                 selectInput(inputId = 'tab3_select_var1', 
                             label = 'X Axis Data Type', 
                             choices = list(
                               "Gene"=c("gene"),
                               "Metabolite"=c("metabolite"),
                               "TME Signature"=c("tme_signature")
                             ),
                             selected = "metabolite",
                             selectize = TRUE),
                 
                 uiOutput(outputId = "tab3_output_var1_content")%>% withSpinner(color="#0dc5c1"),
                 
                 hr(style="border-color: grey"),
                 
                 selectInput(inputId = 'tab3_select_var2', 
                             label = 'Y Axis Data Type', 
                             choices = list(
                               "Gene"=c("gene"),
                               "Metabolite"=c("metabolite"),
                               "TME Signature"=c("tme_signature")
                             ),
                             selected = "tme_signature",
                             selectize = TRUE),
                 
                 uiOutput(outputId = "tab3_output_var2_content")%>% withSpinner(color="#0dc5c1"),
                 
                 hr(style="border-color: grey"),
                 
                 selectInput(inputId = 'tab3_cancerType_var', 
                             label = 'Cancer Type Options', 
                             choices = list(
                               "All" = c("all"),
                               "BRCA_Terunuma" = c("BRCA_Terunuma"),
                               "BRCA_Tang" = c("BRCA_Tang"),
                               "GBM" = c("GBM"),
                               "COAD" = c("COAD"), 
                               "PDAC" = c("PDAC"), 
                               "PRAD" = c("PRAD"),
                               "LiCa1" = c("LiCa1"),
                               "LiCa2" = c("LiCa2"),
                               "DLBCL" = c("DLBCL"),
                               "OV" = c("OV"),
                               "RC12" = c("RC12"),
                               "RC18Flow" = c("RC18Flow"),
                               "RC18MR" = c("RC18MR"),
                               "RC20" = c("RC20")
                             ),
                             selected = c("all"),
                             multiple = TRUE,
                             selectize = TRUE)
                 
                 
                 #submitButton("Update Filter")
                 
                 
               , width=3),
               
               # Show a plot of the generated distribution
               mainPanel(
                 #"Multimodal correlation",
                 
                 fluidRow(HTML("<h4>Multimoal correlation</h4>
                          <p>black: tumor samples, &nbsp
                          blue: normal samples</p>")),
                 
                 fluidRow(
                   
                   #column(3,
                          plotlyOutput("tab3_scatter_plot", width="100%",height="700px") %>% withSpinner(color="#0dc5c1")
                          #uiOutput(outputId = "outTab3_scatter_plot_tumor_out") %>% withSpinner(color="#0dc5c1")
                           #plotlyOutput(outputId = "outTab3_scatter_plot_tumor_out") %>% withSpinner(color="#0dc5c1")
                   #)
                   
                 ),
                 
                 br(),
                 br(),
                 br(),
                 
                 DT::dataTableOutput("tab3_dataSummaryTable") %>% withSpinner(color="#0dc5c1")
                 
                 #fluidRow(
                 #
                 #  column(4, 
                 #         plotlyOutput("tab3_metabolite_gene_scatter_plot")
                 #  ),       
                  
                 #  column(4, 
                 #         plotlyOutput("tab3_metabolite_immune_scatter_plot")
                 #  ),       
                  
                 #  column(4, 
                 #         plotlyOutput("tab3_gene_immune_scatter_plot")
                 #  ),
                           
                   
                 #  width=9),
                 
               , width=9), # end of main panel
               
               position = "left",
               fluid = TRUE
               
             ) # end of sidebar layout
             
             
           ) # end of fluid page 
           
    
  ) # end of tab panel 3
  
  
  ##### End of navbar - Correlation tab ###
  
  #tabPanel("In silico metabolomics"),
  #tabPanel("Documentation"),
  #tabPanel("About")
)

server <- function(input, output, session) {
  
  ######
  
  masterSummaryTable<-reactive({
    masterData<-masterData[,c("CommonID","Dataset","Histology","TN")]
    
    
  })
  
  #pathway_gene
  
  
  #####
  
  output$tab1_summaryPlot <- renderPlotly({
    
    dataset <- masterSummaryTable()
    
    dataset<-dataset[dataset$TN %in% input$sample_type,]
    #dataset<-dataset[dataset$TN %in% "Tumor",]
    
    plotData<-dataset %>% 
      dplyr::count(Dataset,TN,sort=TRUE)
    
    if(length(unique(plotData$TN))>1){
       xAxisOrder<-plotData[plotData$TN %in% "Tumor",]$Dataset
    }else{
       xAxisOrder<-plotData$Dataset
    }
    
    plotData$Dataset<-factor(plotData$Dataset,levels=xAxisOrder)
    
    
    graph <- ggplot(data=plotData,aes(x=Dataset,y=n,fill=TN))
    graph <- graph + geom_bar(stat="identity", position=position_dodge())
    graph <- graph + theme_classic()
    graph <- graph + theme(axis.text.x=element_text(angle=45))
    
    ggplotly(graph)
    
  })
  
  output$tab1_dataSummaryTable <- DT::renderDataTable(DT::datatable({
    dataset <- masterSummaryTable()
    #dataset<-masterData
    dataset
  }))  
  
  ###### start  tab2 #####
  
  output$tab2_gene_da_fraction_plot <- renderPlotly({
    
    graph2<-plot_gene_pathway_da_fraction(pathway_gene)
    
    ggplotly(graph2)
    
  })
  
  output$tab2_metabolite_da_fraction_plot <- renderPlotly({
    
    graph3<-plot_metabolite_pathway_da_fraction(pathway_metabolite)
    
    ggplotly(graph3)
    
  })
  
  output$tab2_pathway_da_score_plot <- renderPlotly({
    if(input$tab2_cancerType %in% "All"){
      heatmaply(#DA_score_frame_new[,grepl("COAD",colnames(DA_score_frame_new))],
        DA_score_frame_plot,
        grid_gap=1,
        grid_color = "black",
        column_text_angle = 45,
        colors=myColor,
        breaks=myBreaks,
        dendrogram="none",
        #Rowv=FALSE,
        #Colv=FALSE,
        scale="none",
        row_dend_left = TRUE,
        plot_method = "plotly"
      )
    }else{
      heatmaply(DA_score_frame_plot[,grepl(input$tab2_cancerType,colnames(DA_score_frame_plot))],
        #DA_score_frame_plot,
        grid_gap=1,
        grid_color = "black",
        column_text_angle = 45,
        colors=myColor,
        breaks=myBreaks,
        dendrogram="none",
        #Rowv=FALSE,
        #Colv=FALSE,
        scale="none",
        row_dend_left = TRUE,
        plot_method = "plotly"
      ) %>% layout(width=500)
    }
    
    
  })
  
  
  output$tab2_dataSummaryTable <- DT::renderDataTable(DT::datatable({
    dataset <- pathway_gene
    #dataset<-masterData
    dataset
  }))  
  
  
  ###### end tab2 #####
  
  ##### start tab3 #####
  
  load_plotData<-reactive({
  
  #selectedDataSet<-"GBM"
      
    cancerTypeWithNormalList<-c("RC20","RC18MR",
                                "BRCA_Terunuma","PRAD",
                                "GBM","PDAC",
                                "COAD")
    
    
    getPlotData<-function(selected_cancerType,dataClass,var1,var2,var1_content,var2_content){
      
      picked_metabolite<-NA
      picked_gene<-NA
      picked_immune_signature<-NA
      
      picked_metabolite_var1<-NA
      picked_metabolite_var2<-NA
      
      picked_gene_var1<-NA
      picked_gene_var2<-NA
      
      picked_immune_signature_var1<-NA
      picked_immune_signature_var2<-NA
      
      
      if(var1 == "metabolite"){
         picked_metabolite<-var1_content
      }
      
      if(var1 == "gene"){
        picked_gene<-var1_content
      }
      
      if(var1 == "tme_signature"){
        picked_immune_signature<-var1_content
      }
      
      if(var2 == "metabolite"){
        picked_metabolite<-var2_content
      }
      
      if(var2 == "gene"){
        picked_gene<-var2_content
      }
      
      if(var2 == "tme_signature"){
        picked_immune_signature<-var2_content
      }
      
      if( var1 == "metabolite" &&  var2 == "metabolite" ){
        
        picked_metabolite_var1<-var1_content
        picked_metabolite_var2<-var2_content
        
      }
      
      if( var1 == "gene" &&  var2 == "gene" ){
        
        picked_gene_var1<-var1_content
        picked_gene_var2<-var2_content
        
      }
      
      if( var1 == "tme_signature" &&  var2 == "tme_signature" ){
        
        picked_immune_signature_var1<-var1_content
        picked_immune_signature_var2<-var2_content
        
      }
      
      ###
      
      log2_metabolite_abundance<-load_metabolite_data(selected_cancerType,dataClass)
      metabolite_list_data<-rownames(log2_metabolite_abundance)
      
      expressionData<-load_expression_data(selected_cancerType,dataClass)
      gene_list_data<-rownames(expressionData)
      
      immune_signature<-load_immune_signature_data(selected_cancerType,dataClass)
      immune_signature_list_data<-rownames(immune_signature)
      
      
      if( ( var1 %in% c("metabolite") & var2 %in% c("gene") ) | ( var1 %in% c("gene") & var2 %in% c("metabolite") ) )
      {
        
        common_sample_id<-intersect(colnames(log2_metabolite_abundance),colnames(expressionData))
        
        if( ( picked_metabolite %in% rownames(log2_metabolite_abundance) ) & (picked_gene %in% rownames(expressionData)) )
        {
            plotData<-data.frame("cancer_type"=selected_cancerType,
                             "sampleId" = common_sample_id,     
                             "var1"=log2_metabolite_abundance[picked_metabolite,common_sample_id],
                             "var2"=expressionData[picked_gene,common_sample_id],
                             stringsAsFactors = FALSE)
        }else{
          
            plotData<-data.frame("cancer_type"=selected_cancerType,
                                 "sampleId" = common_sample_id, 
                               "var1"=NA,
                               "var2"=NA,
                               stringsAsFactors = FALSE)
          
        }   
           
        
      }
      
      if( ( var1 %in% c("metabolite") & var2 %in% c("tme_signature") ) | ( var1 %in% c("tme_signature") & var2 %in% c("metabolite") ) )
      {
        
        common_sample_id<-intersect(colnames(log2_metabolite_abundance),colnames(immune_signature))
        
        
        if( ( picked_metabolite %in% rownames(log2_metabolite_abundance) ) & (picked_immune_signature %in% rownames(immune_signature)) )
        {
        
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                             "var1"=log2_metabolite_abundance[picked_metabolite,common_sample_id],
                             "var2"=immune_signature[picked_immune_signature,common_sample_id],
                             stringsAsFactors = FALSE)
        
        }else{
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=NA,
                               "var2"=NA,
                               stringsAsFactors = FALSE)
          
        }
        
        
      }
      
      if( ( var1 %in% c("tme_signature") & var2 %in% c("gene") ) | ( var1 %in% c("gene") & var2 %in% c("tme_signature") ) )
      {
        
        common_sample_id<-intersect(colnames(expressionData),colnames(immune_signature))
        
        if( ( picked_gene %in% rownames(expressionData) ) & (picked_immune_signature %in% rownames(immune_signature)) )
        {
            plotData<-data.frame("cancer_type"=selected_cancerType,
                                 "sampleId" = common_sample_id, 
                             "var1"=expressionData[picked_gene,common_sample_id],
                             "var2"=immune_signature[picked_immune_signature,common_sample_id],
                             stringsAsFactors = FALSE)
        }else{
          
            plotData<-data.frame("cancer_type"=selected_cancerType,
                                 "sampleId" = common_sample_id, 
                                 "var1"=NA,
                                 "var2"=NA,
                                 stringsAsFactors = FALSE)
          
        }
        
        
      }
      
      #####
      # placeholder for same data modalities 
      #####
      
      if( (var1 %in% c("metabolite") ) && (var2 %in% c("metabolite")) ){
        
        #message(sprintf("enter this loop"))
        common_sample_id<-intersect(colnames(log2_metabolite_abundance),colnames(log2_metabolite_abundance))
        
        
        if( ( picked_metabolite_var1 %in% rownames(log2_metabolite_abundance) ) & (picked_metabolite_var2 %in% rownames(log2_metabolite_abundance)) )
        {
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=log2_metabolite_abundance[picked_metabolite_var1,common_sample_id],
                               "var2"=log2_metabolite_abundance[picked_metabolite_var2,common_sample_id],
                               stringsAsFactors = FALSE)
          
        }else{
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=NA,
                               "var2"=NA,
                               stringsAsFactors = FALSE)
          
        }
        
      } # end of metabolite - metabolite
      
      if( (var1 %in% c("gene") ) && (var2 %in% c("gene")) ){
        
        #message(sprintf("enter this loop"))
        common_sample_id<-intersect(colnames(expressionData),colnames(expressionData))
        
        
        if( ( picked_gene_var1 %in% rownames(expressionData) ) & (picked_gene_var2 %in% rownames(expressionData)) )
        {
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=expressionData[picked_gene_var1,common_sample_id],
                               "var2"=expressionData[picked_gene_var2,common_sample_id],
                               stringsAsFactors = FALSE)
          
        }else{
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=NA,
                               "var2"=NA,
                               stringsAsFactors = FALSE)
          
        }
        
      } # end of gene - gene
      
      if( (var1 %in% c("tme_signature") ) && (var2 %in% c("tme_signature")) ){
        
        #message(sprintf("enter this loop"))
        common_sample_id<-intersect(colnames(immune_signature),colnames(immune_signature))
        
        
        if( ( picked_immune_signature_var1 %in% rownames(immune_signature) ) & (picked_immune_signature_var2 %in% rownames(immune_signature)) )
        {
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=immune_signature[picked_immune_signature_var1,common_sample_id],
                               "var2"=immune_signature[picked_immune_signature_var2,common_sample_id],
                               stringsAsFactors = FALSE)
          
        }else{
          
          plotData<-data.frame("cancer_type"=selected_cancerType,
                               "sampleId" = common_sample_id, 
                               "var1"=NA,
                               "var2"=NA,
                               stringsAsFactors = FALSE)
          
        }
        
      } # end of metabolite - metabolite
      
      ######
      # end of same data modalities
      ######
      
      
      #colnames(plotData)<-c("cancer_type",var1,var2)
      
      return(plotData)
    }
      
    
    #selected_cancerType<-input$tab3_cancerType_var
    #dataClass<-"tumor"
    
    
    if(length(input$tab3_cancerType_var) == 1 & sum(input$tab3_cancerType_var %in% "all") != 1 ){
    
      plotData<-getPlotData(input$tab3_cancerType_var,
                            "tumor",
                            input$tab3_select_var1,
                            input$tab3_select_var2,
                            input$tab3_var1_content,
                            input$tab3_var2_content)
    }
    
    if(length(input$tab3_cancerType_var) > 1 & sum(input$tab3_cancerType_var %in% "all") != 1 ){
      
       cancerTypeList<-input$tab3_cancerType_var
      
       plotData<-lapply(1:length(cancerTypeList),function(x){
              
                     
                     cancerType<-cancerTypeList[x]
                     
                     
                     plotData_single<-getPlotData(cancerType,
                                                  "tumor",
                                                  input$tab3_select_var1,
                                                  input$tab3_select_var2,
                                                  input$tab3_var1_content,
                                                  input$tab3_var2_content)
                     
                     plotData_single$sampleType<-"tumor"
         
                     
                     if(cancerType %in% cancerTypeWithNormalList )
                     {
                       
                       plotData_normal_single<-getPlotData(cancerType,
                                                    "normal",
                                                    input$tab3_select_var1,
                                                    input$tab3_select_var2,
                                                    input$tab3_var1_content,
                                                    input$tab3_var2_content)
                       
                       plotData_normal_single$sampleType<-"normal"
                       
                       plotData_single<-rbind(plotData_single,plotData_normal_single)
                       
                     }
                     
                      return(plotData_single)
         
                     })
       
       names(plotData)<-cancerTypeList
       
       plotData<-plyr::rbind.fill(plotData)
    }
    
    if( sum(input$tab3_cancerType_var %in% "all") == 1 ){
      
      
      plotData<-lapply(1:length(cancerTypeList),function(x){
        
        
        cancerType<-cancerTypeList[x]
        
        plotData_single<-getPlotData(cancerType,
                                     "tumor",
                                     input$tab3_select_var1,
                                     input$tab3_select_var2,
                                     input$tab3_var1_content,
                                     input$tab3_var2_content)
        
        plotData_single$sampleType<-"tumor"
        
        
        if( cancerType %in% cancerTypeWithNormalList )
        {
          
          #print("here2")
          #print(cancerType)
          #print(cancerTypeWithNormalList)
          
          
          plotData_normal_single<-getPlotData(cancerType,
                                              "normal",
                                              input$tab3_select_var1,
                                              input$tab3_select_var2,
                                              input$tab3_var1_content,
                                              input$tab3_var2_content)
          
          plotData_normal_single$sampleType<-"normal"
          
          plotData_single<-rbind(plotData_single,plotData_normal_single)
          
          
        }
        
        return(plotData_single)
        
      })
      
      names(plotData)<-cancerTypeList
      
      plotData<-plyr::rbind.fill(plotData)
      
    
    }
    
    
    
      
      return(list(plotData=plotData,
                  metabolite_name=input$tab3_var1_content,
                  gene_name=input$tab3_var2_content))
                  
  
  })
  

  load_num_cancerTypes<-reactive({
    
    if(input$tab3_cancerType_var == "all"){
      n_cancerTypes<-15
    }else{
      n_cancerTypes<-length(input$tab3_cancerType_var)
    }
    
    return(n_cancerTypes)
    
  })
  
  
  
  output$tab3_cancerType_options_out <-renderText({
         input$tab3_cancerType_var
  })
  
  output$tab3_xAxis_var_out <-renderText({
       input$tab3_select_var1
  })
  
  
  output$tab3_yAxis_var_out <-renderText({
       input$tab3_select_var2
  })
  
  output$tab3_xAxis_var_content_out <-renderText({
    input$tab3_var1_content
  })
  
  
  output$tab3_yAxis_var_content_out <-renderText({
    input$tab3_var2_content
  })
  
  
  output$tab3_output_var1_content <- renderUI({
    
       selectInput(inputId = 'tab3_var1_content', 
                   label = input$tab3_select_var1, 
                   choices = tab3_content_choices[[input$tab3_select_var1]],
                   #choices = list(
                   #  "pyruvate"=c("pyruvate"),
                   #  "glucose"=c("glucose")
                   #),
                   selectize = TRUE
                   
    ) # <- put the reactive element here
    
    
  })
  
  output$tab3_output_var2_content <- renderUI({
    
    selectInput(inputId = 'tab3_var2_content', 
                   label = input$tab3_select_var2, 
                   choices = tab3_content_choices[[input$tab3_select_var2]],
                   #choices = list(
                   #  "CYT"=c("CYT"),
                   #  "TIS"=c("TIS")
                   #),
                   selectize = TRUE
    ) # <- put the reactive element here
    
    
  })
  
  ###
  
  #n_cancerTypes <- ifelse(input$tab3_cancerType_var == "all",15,length(input$tab3_cancerType_var))
  
  #n_cols <- 5
  
  ######
  
  if(TRUE){
    
    output$tab3_scatter_plot <-renderPlotly({
      
      data<-load_plotData()
      
      #xVar<-data$metabolite_name
      #yVar<-data$gene_name
      
      cancerTypeList<-unique(data$plotData$cancer_type)
      num_cancerType<-length(cancerTypeList)
      
      #xVar<-input$tab3_select_var1
      #yVar<-input$tab3_select_var2
      
      xVar<-"var1"
      yVar<-"var2"
        
      xVar_content<-input$tab3_var1_content
      yVar_content<-input$tab3_var2_content  
        
      graphList<-list()
        
      for(i in 1:num_cancerType){  
        
        
        cancerType<-cancerTypeList[i]
        plotData<-data$plotData
        plotData<-plotData[plotData$cancer_type %in% cancerType,]
        
        graph<-plot_multimodal_scatter_plot(plotData,
                                            xVar,
                                            yVar, 
                                            xVar_content,
                                            yVar_content)
        
        graphList[[i]]<-ggplotly(graph) %>% add_annotations(
          text = cancerType,
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          yshift = 20,
          showarrow = FALSE,
          font = list(size = 15)
                                            )
                         #tooltip = c("sampleId",xVar,yVar))
        
      } # end of for loop
      
      subplot(graphList,nrows=3, margin=0.04)
      
    })
  
  
  }
  ######
  
  
  
  ###
  
  #output$tab3_dataSummaryTable <- renderPrint({
  #  data<-load_plotData()
  #  data$plotData
  #})  
  
  output$tab3_dataSummaryTable<-DT::renderDataTable(DT::datatable({
    
    data<-load_plotData()
    tableContent<-data$plotData
    
    tableContent$var1<-round(tableContent$var1,3)
    tableContent$var2<-round(tableContent$var2,3)
    
    colnames(tableContent)<-c("cancerType",
                              "sampleID",
                              input$tab3_select_var1,
                              input$tab3_select_var2,
                              "sampleType")
    
    return(tableContent)
    
  }, options=list(
             columnDefs = list(list(className = 'dt-center', targets = "_all"),
                               list(target=10, visible=FALSE)),
             pageLength=10,
             lengthMenu=c(10,15,20,25)
             )
  ))  
  
  
  ##### end tab3 #####
  
  ######
  # Automatically bookmark every time an input changes
  #observe({
  #  reactiveValuesToList(input)
  #  session$doBookmark()
  #})
  
  # Update the query string
  #onBookmarked(updateQueryString)
  
  #####
}

shinyApp(ui, server, enableBookmarking = "url")