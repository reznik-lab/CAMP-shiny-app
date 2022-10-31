

plot_multimodal_scatter_plot<-function(plotData, xVar, yVar, xVar_content,yVar_content){

#xVar<-"metabolite_value"
#yVar<-"immune_signature_value"

# aesthetics mapping
#https://ggplot2.tidyverse.org/reference/aes_.html

graph <- ggplot(data=plotData,aes_(x=as.name(xVar),y=as.name(yVar), label= ~sampleId, color= ~sampleType) )
graph <- graph + geom_point()
graph <- graph + scale_color_manual( name = "sampleType",
                                     values = c("tumor"="black","normal"="royalblue"))
#graph <- graph + labs(title=plotData$cancer_type[1])
#graph <- graph + labs(title=plotData$cancer_type[1])
graph <- graph + xlab(xVar_content)
graph <- graph + ylab(yVar_content)
#graph <- graph + facet_wrap(cancer_type~. , scale="free")
#graph <- graph + facet_trelliscope(~as.name(xVar)+as.name(yVar))
#graph <- graph + theme_classic()
graph <- graph + theme_bw()
graph <- graph + theme(plot.title = element_text(hjust = 0.5),
                       axis.title.x = element_text(margin = margin(t = 15, r = 00, b = 0, l = 0)),
                       axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
                       legend.position = "none"
                       )

#print(graph)

return(graph)

}

#ggplotly(graph)


