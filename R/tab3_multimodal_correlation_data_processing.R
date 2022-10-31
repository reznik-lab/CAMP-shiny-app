#library(ggplot2)
#library(plotly)

#setwd("~/work/Ed_lab/code_dev/mmm-shiny-app")

load_metabolite_data<-function(selectedDataSet,dataClass){

#selectedDataSet<-"GBM"
#dataClass<-"tumor"

fileName<-paste("data/metabolite/",selectedDataSet,"_log2_metabolite_abundance_",dataClass,".rds",sep="")
log2_metabolite_abundance<-readRDS(fileName)

return(log2_metabolite_abundance)

}

load_immune_signature_data<-function(selectedDataSet,dataClass){

fileName<-paste("data/tme_signature/",selectedDataSet,"_immune_signature_zscale_",dataClass,".rds",sep="")
immune_signature<-readRDS(fileName)

return(immune_signature)

}

load_expression_data<-function(selectedDataSet,dataClass){

fileName<-paste("data/gene_expression/",selectedDataSet,"_gene_expression_",dataClass,".rds",sep="")
expressionData<-readRDS(fileName)

return(expressionData)

}

#####

if(FALSE){

metabolite_list_data<-rownames(log2_metabolite_abundance)
gene_list_data<-rownames(expressionData)
immune_signature_list_data<-rownames(immune_signature)

picked_metabolite<-metabolite_list_data[1]
picked_gene<-gene_list_data[1]
picked_immune_signature<-immune_signature_list_data[1]

common_sample_id<-intersect(colnames(immune_signature),
                            intersect(colnames(log2_metabolite_abundance),colnames(expressionData)))


plotData<-data.frame("metabolite_value"=log2_metabolite_abundance[picked_metabolite,common_sample_id],
                     "gene_value"=expressionData[picked_gene,common_sample_id],
                     "immune_signature_value"=immune_signature[picked_immune_signature,common_sample_id],
                     stringsAsFactors = FALSE)

xVar<-"metabolite_value"
yVar<-"immune_signature_value"

# aesthetics mapping
#https://ggplot2.tidyverse.org/reference/aes_.html

graph <- ggplot(data=plotData,aes_(x=as.name(xVar),y=as.name(yVar)) )
graph <- graph + geom_point()
graph <- graph + theme_classic()


print(graph)

ggplotly(graph)

}
