#' Shinyapp based on KoladaAPI
#' @author Hirbod & Muhaiminul
#' @field noinput 

#' @import readxl
#' @import httr
#' @import ggplot2
#' @import shiny
#' @import methods
#' @export plotfunction_ind
#plot all functions using corresponding names
plotfunction_ind=function(){
  # library(httr)
  # library(shiny)
  # library(readxl)
  # library(ggplot2)
  url1="https://raw.githubusercontent.com/Hgh-studentacc/personal_uploads/main/List_kommun.xlsx"
  url2="https://raw.githubusercontent.com/Hgh-studentacc/personal_uploads/main/factors.xlsx"
  GET(url1, write_disk(tfile1 <- tempfile(fileext = ".xlsx")))
  GET(url2, write_disk(tfiles <- tempfile(fileext = ".xlsx")))
  List_kommun = read_excel(tfile1,col_types = "text")
  List_factors = read_excel(tfiles,col_types = "text",col_names=FALSE)
  outlist=KoladaAPI()
  da=matrix(outlist[[1]],ncol=4)
  colnames(da)=names(outlist[[1]])
  for (i in 2:length(outlist)){da=rbind(da,(outlist[[i]]))}
  da=as.data.frame(da)
  ui <- fluidPage(checkboxGroupInput(inputId="factors", label="Choose economic factor variable", choices = NULL,
                                     inline = FALSE, width = NULL, choiceNames = List_factors[[2]],
                                     choiceValues = List_factors[[1]]),
                  checkboxGroupInput(inputId="Municipalities", label="Choose city variable", choices = NULL,
                                     inline = FALSE, width = NULL, choiceNames = List_kommun$Kommun[c(29,122,236,253,27,152,130,252,200,33,262,126)-1],
                                     choiceValues = List_kommun$Kod[c(29,122,236,253,27,152,130,252,200,33,262,126)-1]),
                  
                  plotOutput("densPlot"))
  
  server <- function(input, output) {
    output$densPlot <- renderPlot({
      first_filter=da[da[["kpi"]]==input$factors,]
      second_filter=first_filter[first_filter[["municipality"]]==input$Municipalities,]
      mufflin=ggplot(data=second_filter,aes(x=unlist(.data$period),y=unlist(.data[["value"]])))+
        geom_point()+
        geom_line()+
        xlab("period")+
        ylab(paste(List_factors[[2]][List_factors[[1]]==input$factors]))
      
      print(mufflin)
      #print(input$Municipalities)
      print(first_filter)
      print(second_filter)
      
    })
  }
shinyApp(ui = ui, server = server)
  
}



# Run the application 
