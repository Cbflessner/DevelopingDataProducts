#setwd("C:/Users/christian.flessner/Dropbox (ZirMed)/Christian Flessner/Coursera/Developing Data Products/DevelopingDataProducts/GameOfThronesNames")
#source("GameOfThronesNames.R")
library(shiny)
library(RCurl)
library(ggplot2)
library(dplyr)

url<-getURL("https://raw.githubusercontent.com/Cbflessner/DevelopingDataProducts/gh-pages/usa_names_data.csv")
names<-read.csv(text=url)

characters<-c("Tyrion","Khaleesi", "Meera"
              ,"Arya","Sansa","Brienne")

fill<-function(name)
{
        switch(name
               ,"Tyrion"="firebrick4"
               ,"Khaleesi"="black"
               ,"Meera"="palegreen4"
               ,"Arya"="palegreen2"
               ,"Sansa"="palegreen2"
               ,"Brienne"="lightpink")
}

border<-function(name)
{
        switch(name
               ,"Tyrion"="gold3"
               ,"Khaleesi"="red"
               ,"Meera"="black"
               ,"Arya"="grey70"
               ,"Sansa"="grey70"
               ,"Brienne"="blue2")
}

for(i in characters)
{
        assign(i,names%>%filter(name==i))
}

note<-"First Season of 
Game of Thrones Starts"


interface<-fluidPage(

        titlePanel("Impact of Game of Thrones on US Baby Names"),
        h5("The HBO show Game of Thrones has become a pop culture phenomenon and one of the most watched television programs of all time.  The below app invesigates its effects on baby names in the United States, based on information taken from the Social Security Administration.  To use the app simply check one or multiple Game of Thrones characters names and see a historgram of how often the name has appeared across the years with an vertical line to indicate when the first season of Game of Thrones premiered."),
        sidebarPanel(  
                checkboxGroupInput("character", "Game of Thrones Characters",
                                   characters
                )
        ),
        
        mainPanel(
                uiOutput("ui_plot")
                
        )
        
)

s<-function(input, output) {
output$ui_plot<-renderUI({
        out<- list()
        if(length(input$character)==0){return(NULL)}
        for (i in 1:length(input$character))
        {
                out[[i]]<-plotOutput(outputId = paste0("plot",i))
        }
        return(out)
})

observe({
        for(i in 1:6){
                local({
                        ii<-i
                        output[[paste0("plot",ii)]]<-renderPlot({
                                if(length(input$character)>ii-1){
                                        ggplot(data=get(
                                                input$character[ii]), 
                                               aes(x=year, y=count))+
                                                geom_bar(stat="identity"
                                                         ,fill=fill(input$character[ii])
                                                         ,color=border(input$character[ii])
                                                         ,size=1)+
                                                xlim(1950,2020)+
                                                ggtitle(input$character[ii])+
                                                geom_vline(xintercept=2011, 
                                                           color="black",size=1)+
                                                annotate("text",x=2011
                                                         ,y=max(get(input$character[ii])$count)
                                                         ,label=note)+
                                                theme(plot.title = 
                                                              element_text(color="#666666"
                                                                           , face="bold"
                                                                           , size=32
                                                                           , hjust=0))
                                                
                                }
                                else{NULL}
                        })

                })
        }

})

}
  

shinyApp(ui=interface, server=s)

