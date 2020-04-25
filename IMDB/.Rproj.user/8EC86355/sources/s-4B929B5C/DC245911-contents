library(lubridate)
library(dplyr)
library(plyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

df<-read.table(file = "imdb.csv", header = TRUE, sep = ",")
df$Month<-month(parse_date_time(x = df$Release.Date,orders =c("d m y", "d B Y", "m/d/y"),locale = "eng"), label = TRUE, abbr = TRUE)
df$Genre<-gsub("\t","",df$Genre,fixed = ,TRUE)

df$Keywords<-as.character(df$Keywords)
df$Keywords<-gsub(" ","",df$Keywords,fixed = TRUE)
kfreq<-setNames(as.data.frame(table(unlist(strsplit(df$Keywords, ",", fixed=TRUE)))),c("Keyword","Number"))
kfreq$Keyword<-as.character(kfreq$Keyword)
kfreq<-kfreq[order(-kfreq$Number),]
kfreq<-kfreq[!grepl("nudity", kfreq$Keyword),]
kfreq<-kfreq[!grepl("bare", kfreq$Keyword),]
kfreq<-kfreq[1:10,]
keys<-as.array(kfreq$Keyword)
keys<-append(keys,"None",0)

df$Certificate<-as.character(df$Certificate)
df$Certificate<-gsub(" ","",df$Certificate,fixed = TRUE)
cfreq<-setNames(as.data.frame(table(unlist(strsplit(df$Certificate, ",", fixed=TRUE)))),c("Certificate","Number"))
cfreq$Certificate<-as.character(cfreq$Certificate)
cfreq<-cfreq[cfreq$Number>6,]
certs<-as.array(cfreq$Certificate)
certs<-append(certs,"None",0)

genres<-as.array(unique(df$Genre))
genres<-append(genres,"None",0)

#Left Join kfreq?

ui <- dashboardPage(
    
    dashboardHeader(title = "IMDB Analysis"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         selectInput("flt","Filter by Year/Decade?",c("None","Year","Decade")),
                        conditionalPanel(
                            condition = "input.flt=='Decade'",
                            selectInput("ipdc","Choose Decade",c("1910","1920","1930","1940","1950","1960","1970","1980","1990","2000","2010"))
                            
                        ),
                        conditionalPanel(
                            condition = "input.flt=='Year'",
                            selectInput("ipyr","Choose Year",c(1913,1915:2017))
                            
                        ),
                        h5("Filters:"),
                        fluidRow(
                        column(6,style='padding-left:0px; padding-right:0px;',   
                        selectInput("gnr","Genre Filter 1",genres)),
                        column(6, style='padding-left:0px; padding-right:0px;',
                        selectInput("gnr2","Genre Filter 2",genres))
                        ),
                        fluidRow(
                        column(6,style='padding-left:0px; padding-right:0px;', selectInput("rnt","Runtime Filter 1",c("None","<90","90-120","120-150","150-180","180-210",">210"))
                        ),
                        column(6,style='padding-left:0px; padding-right:0px;',
                               selectInput("rnt2","Runtime Filter 2",c("None","<90","90-120","120-150","150-180","180-210",">210"))
                     )),
                    fluidRow(    
                     column(6,style='padding-left:0px; padding-right:0px;',  selectInput("crt","Certificate Filter 2",certs)
                     ),
                     column(6,style='padding-left:0px; padding-right:0px;', 
                
                          selectInput("crt2","Certificate Filter 1",certs)
                     )
                    ),
                    fluidRow(
                    column(6,style='padding-left:0px; padding-right:0px;', 
                        selectInput("kwd","Keyword Filter 1",keys),
                    ),
                    column(6,style='padding-left:0px; padding-right:0px;', 
                          selectInput("kwd2","Keyword Filter 2",keys)
                        
                    ))
                        
                         
                         )
    ),
    dashboardBody(
      
        tags$head( 
          tags$style(HTML(".main-sidebar { font-size: 12px; }")) #change the font size to 20
        ),
        tabsetPanel(
            tabPanel( "Graphs",
                      fluidPage(
                     
                                fluidRow(          
                                    column(8,
                                        
                                           box(solidHeader = FALSE, status = "primary", width = 12,
                                                plotOutput("bar1",height = 335)
                                           )
                                    ),
                                    column(4,
                                           
                                           box( solidHeader = FALSE, status = "primary", width = 12
                                                ,plotOutput("bar2",height = 335)
                                           )
                                    )
                                  ),
                                
                                
                                fluidRow(
                                  column(6,
                                  box(solidHeader = FALSE, status = "primary", width = 12,
                                       plotOutput("bar3",height = 335))
                                  ),
                                  column(6,
                                         box(  solidHeader = FALSE, status = "primary", width = 12,
                                              plotOutput("bar4",height = 335))
                                         ),
                                ),
                                
                                
                                
                                fluidRow(
                                  column(6,
                                         box( solidHeader = FALSE, status = "primary", width = 12
                                              ,plotOutput("bar5",height = 335))
                                         ),
                                  
                                  column(6,
                                         
                                         box( solidHeader = FALSE, status = "primary", width = 12
                                              ,plotOutput("bar6",height = 335))
                                         
                                  ),
                                )
                                   
                                    
                                    
                                    
                              
                          
                                
                                
                                
                                
                     
                      
                      
                      )       
                      
            ),
            
            
            tabPanel("Tables and List",
                     fluidPage(
                       
                       fluidRow(
                         column(4,
                                box( title = "Films by Year", solidHeader = TRUE, status = "primary", width = 12,
                                     dataTableOutput("tab1")
                                )
                                
                                ),
                         column(4,
                                box( title = "Films by Runtime", solidHeader = TRUE, status = "primary", width = 12,
                                     dataTableOutput("tab2")
                                )
                                
                                
                                ),
                         column(4,
                                box( title = "Films by Month", solidHeader = TRUE, status = "primary", width = 12,
                                     dataTableOutput("tab3")
                                )
                           
                         )
                         
                       ),
                       fluidRow(
                         column(4,
                                box( title = "Films by Genre", solidHeader = TRUE, status = "primary", width = 12,
                                     dataTableOutput("tab4")
                                )
                                
                         ),
                         column(4,
                                
                                box( title = "Films by Certificate", solidHeader = TRUE, status = "primary", width = 12,
                                     dataTableOutput("tab5")
                                )
                                
                         ),
                         column(4,
                                box( title = "Films by Keyword", solidHeader = TRUE, status = "primary", width = 12,
                                     dataTableOutput("tab6")
                                )
                                
                         )
                         
                         
                       )
                       
                     ),
                     
                     
                     
                     
                     
            ),
        
            
            tabPanel("About",  h4("Authors:Ansul Goenka, Parikshit Solunke"), h4(""))
        ))
)



server <- function(input, output) 
{
  
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }
  
  #Apply filters according to user input and store the filtered data in df2
  df2 <- reactive({
    rntlow=0
    rnthigh=0
    rntlow2=0
    rnthigh2=0
    
    if(input$rnt=='<90')
    {
     rntlow=60
     rnthigh=90
    }
    if(input$rnt=='90-120')
    {
      rntlow=90
      rnthigh=120
    }
    if(input$rnt=='120-150')
    {
      rntlow=120
      rnthigh=150
    }
    if(input$rnt=='150-180')
    {
      rntlow=150
      rnthigh=180
    }
    if(input$rnt=='180-210')
    {
      rntlow=180
      rnthigh=210
    }
    if(input$rnt=='>210')
    {
      rntlow=210
      rnthigh=1000
    }
    if(input$rnt2=='<90')
    {
      rntlow2=60
      rnthigh2=90
    }
    if(input$rnt2=='90-120')
    {
      rntlow2=90
      rnthigh2=120
    }
    if(input$rnt2=='120-150')
    {
      rntlow2=120
      rnthigh2=150
    }
    if(input$rnt2=='150-180')
    {
      rntlow2=150
      rnthigh2=180
    }
    if(input$rnt2=='180-210')
    {
      rntlow2=180
      rnthigh2=210
    }
    if(input$rnt2=='>210')
    {
      rntlow2=210
      rnthigh2=1000
    }
  
      
    
    df %>%
      filter(
        conditional(input$gnr != 'None' & input$gnr2=='None', Genre == input$gnr ),
        conditional(input$gnr != 'None' & input$gnr2!='None', Genre == input$gnr|Genre == input$gnr2 ),
        conditional(input$gnr == 'None' & input$gnr2 !='None', Genre == input$gnr2 ),
        
        conditional(input$rnt != 'None'& input$rnt2=='None', Run.Time>=rntlow & Run.Time<rnthigh ),
        conditional(input$rnt != 'None'& input$rnt2!='None', (Run.Time>=rntlow & Run.Time<rnthigh) |(Run.Time>=rntlow2 & Run.Time<rnthigh2)  ),
        conditional(input$rnt == 'None'& input$rnt2!='None', Run.Time>=rntlow2 & Run.Time<rnthigh2 ),
        
        conditional(input$kwd != 'None'& input$kwd2 == 'None' , grepl(input$kwd,Keywords)),
        conditional(input$kwd != 'None'& input$kwd2 != 'None' , grepl(input$kwd,Keywords)|grepl(input$kwd2,Keywords)),
        conditional(input$kwd == 'None'& input$kwd2 != 'None' , grepl(input$kwd2,Keywords)),

        conditional(input$crt !='None' & input$crt2=='None', grepl(input$crt,Certificate ) ),
        conditional(input$crt !='None' & input$crt2!='None', grepl(input$crt,Certificate )|grepl(input$crt2,Certificate ) ),
        conditional(input$crt =='None' & input$crt2!='None', grepl(input$crt2,Certificate ) )
        
      )
  })
  
  
  
  
  
  
  

    
  output$bar1 <- renderPlot({
    plot<-count(df2(), 'Year')
    ggplot(data=plot, aes(x=Year, y=freq)) + geom_bar(stat="identity",fill="#0072B2" ,width=0.8)+ylab("Number") +scale_x_continuous("Year",breaks=seq(1913, 2017, 5))
    
    
    })
  output$bar2 <- renderPlot({
    plot<-as.data.frame(table(cut(df2()$Run.Time,breaks=c(59,89,119,149,179,209,2000),labels=c("<90","90-120","120-150","150-180","180-210",">210"))) )
    colnames(plot)<-c("Runtime","Number")
    
    ggplot(data=plot, aes(x=Runtime, y=Number)) + geom_bar(stat="identity",fill="#0072B2" ,width=0.8)+ggtitle("Films by Runtime")+xlab("Runtimes(Minutes)") 

    
    
  })
  
  
  output$bar3 <- renderPlot({
    plot<-count(df2(), 'Month')
    plot<-plot[complete.cases(plot$Month), ]

    
    ggplot(data=plot, aes(x=Month, y=freq)) + geom_bar(stat="identity",fill="#0072B2" ,width=0.8)+ylab("Number")
    
    
  })
  output$bar4 <- renderPlot({
    plot<-count(df2(), 'Genre')
    
    ggplot(data=plot, aes(x=Genre, y=freq)) + geom_bar(stat="identity",fill="#0072B2" ,width=0.8)+ylab("Number") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
  })
  
  output$bar5 <- renderPlot({

    ggplot(data=cfreq, aes(x=Certificate, y=Number)) + geom_bar(stat="identity",fill="#0072B2" ,width=0.8)+ scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000,14000))
    
  })
  
  output$bar6 <- renderPlot({

    ggplot(data=kfreq, aes(x=Keyword, y=Number)) + geom_bar(stat="identity",fill="#0072B2" ,width=0.8)+coord_flip()
    
  })
  
  
  output$tab1<-renderDataTable({
   plot<-count(df2(), 'Year')
   colnames(plot)<-c("Year","Number")
  datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 5,dom='tp'))
  })
  
  
  output$tab2<-renderDataTable({
    plot<-as.data.frame(table(cut(df2()$Run.Time,breaks=c(59,89,119,149,179,209,2000),labels=c("60-90","90-120","120-150","150-180","180-210",">210"))) )
    colnames(plot)<-c("Runtime(Minutes)","Number")

    datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
  })
  
  output$tab3<-renderDataTable({
    plot<-count(df2(), 'Month')
    plot<-plot[complete.cases(plot$Month), ]
    colnames(plot)<-c("Month","Number")
    datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
  })
  
  output$tab4<-renderDataTable({
    plot<-count(df2(), 'Genre')
    colnames(plot)<-c("Month","Number")
    datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
  })
  output$tab5<-renderDataTable({
    
    datatable(cfreq,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 7,dom='tp'))
  })
  output$tab6<-renderDataTable({
  
    datatable(kfreq,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
  })
    
    
}
shinyApp(ui = ui, server = server)
