library(lubridate)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(reshape)

df<-read.table(file = "imdb.csv", header = TRUE, sep = ",")
df$Month<-month(parse_date_time(x = df$Release.Date,orders =c("d m y", "d B Y", "m/d/y"),locale = "eng"), label = TRUE, abbr = TRUE)
df$Genre<-gsub("\t","",df$Genre,fixed = ,TRUE)

df$Keywords<-as.character(df$Keywords)
df$Keywords<-gsub(" ","",df$Keywords,fixed = TRUE)

kfreq<-setNames(as.data.frame(table(unlist(strsplit(df$Keywords, ",", fixed=TRUE)))),c("Keyword","Number"))
kfreq$Keyword<-as.character(kfreq$Keyword)
kfreq<-kfreq[order(-kfreq$Number),]
kfreq<-kfreq[!grepl("nudity|sex|breast|chest|softcore|rape|rapist|porn", kfreq$Keyword),]
allkeys<-kfreq$Keyword[1:3000]
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

totmov=nrow(df)
ayr=round(totmov/length(unique(df$Year)),1)
amt=round(ayr/12,1)
art=round(sum(df$Run.Time)/totmov)
#Left Join kfreq?

ui <- dashboardPage(skin="black",
                    
                    dashboardHeader(title = "IMDB Visualization"),
                    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                                     sidebarMenu(
                                         selectInput("flt","Filter by Year/Decade?",c("None","Year","Decade")),
                                         conditionalPanel(
                                             condition = "input.flt=='Decade'",
                                             selectInput("ipdc","Choose Decade",c("1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s"),selected="2010s")
                                             
                                         ),
                                         conditionalPanel(
                                             condition = "input.flt=='Year'",
                                             selectInput("ipyr","Choose Year",c(1913,1915:2017),selected="2013")
                                             
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
                                         radioButtons("krd", "Keyword Method", choices = c("Select From top 10","Type Keyword"), inline = FALSE,),
                                         conditionalPanel(                            
                                             condition = "input.krd!='Type Keyword'",
                                             
                                             fluidRow(
                                                 column(6,style='padding-left:0px; padding-right:0px;', 
                                                        selectInput("kwd","Keyword Filter 1",keys),
                                                 ),
                                                 column(6,style='padding-left:0px; padding-right:0px;', 
                                                        selectInput("kwd2","Keyword Filter 2",keys)
                                                        
                                                 ))
                                         ),
                                         conditionalPanel(                            
                                             condition = "input.krd=='Type Keyword'",
                                             
                                             selectizeInput(
                                                 'typekwd', 'Type Keyword(upto 3)', choices = allkeys,multiple=TRUE,
                                                 options = list(maxItems = 3,maxOptions = 15,placeholder = 'Type & Select from Suggestions'))
                                         )
                                         
                                     )
                    ),
                    dashboardBody(
                        
                        tags$head( 
                            tags$style(HTML(".main-sidebar { font-size: 12px; }"))
                        ),
                        tabsetPanel(
                            tabPanel( "Graphs",
                                      fluidPage(
                                          fluidRow(
                                              column(12, 
                                                     
                                                     box(solidHeader=TRUE,width=12, htmlOutput("overview"))
                                                     
                                                     
                                              )
                                              
                                              
                                              
                                          ),
                                          
                                          fluidRow(          
                                              tabBox(
                                                  id = "tabset4", height = "380px",width=8,
                                                  tabPanel("Distribution by Year",  plotOutput("bar1",height = 335))
                                                  
                                              ),
                                              tabBox(
                                                  id = "tabset5", height = "380px",width=4,
                                                  tabPanel("Distribution by Runtime",  plotOutput("bar2",height = 335))
                                                  
                                              )
                                          ),
                                          fluidRow(
                                              br()
                                          ),
                                          fluidRow(
                                              
                                              tabBox(
                                                  id = "tabset2", height = "380px",width=6,
                                                  tabPanel("Distribution by Month",  plotOutput("bar3",height = 335)),
                                                  tabPanel("Distribution by Certificate",  plotOutput("bar5",height = 335))
                                              ),
                                              
                                              
                                              tabBox(
                                                  # The id lets us use input$tabset1 on the server to find the current tab
                                                  id = "tabset3", height = "380px",width=6,
                                                  tabPanel("Distribution by Genre", plotOutput("bar4",height = 335) ),
                                                  tabPanel("Distribution by Keyword",plotOutput("bar6",height = 335))
                                              )
                                          ) 
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                      )       
                                      
                            ),
                            
                            
                            tabPanel("Tables",
                                     fluidPage(
                                         
                                         fluidRow(
                                             column(4,
                                                    box( title = "Films by Year", solidHeader = TRUE, status = "info", width = 12,height=370,
                                                         dataTableOutput("tab1")
                                                    )
                                             ),
                                             
                                             column(4,
                                                    box( title = "Films by Runtime", solidHeader = TRUE, status = "info", width = 12,height=370,
                                                         dataTableOutput("tab2")
                                                    )
                                                    
                                                    
                                             ),
                                             column(4,
                                                    box( title = "Films by Month", solidHeader = TRUE, status = "info", width = 12,height=370,
                                                         dataTableOutput("tab3")
                                                    )
                                                    
                                             )
                                             
                                         ),
                                         fluidRow(
                                             column(4,
                                                    box( title = "Films by Genre", solidHeader = TRUE, status = "info", width = 12,height=375,
                                                         dataTableOutput("tab4")
                                                    )
                                                    
                                             ),
                                             column(4,
                                                    
                                                    box( title = "Films by Certificate", solidHeader = TRUE, status = "info", width = 12,height=375,
                                                         dataTableOutput("tab5")
                                                    )
                                                    
                                             ),
                                             column(4,
                                                    box( title = "Films by Keyword", solidHeader = TRUE, status = "info", width = 12,height=375,
                                                         dataTableOutput("tab6")
                                                    )
                                                    
                                             )
                                             
                                             
                                         )
                                         
                                     ),
                                     
                                     
                                     
                                     
                                     
                            ),
                            tabPanel("Highest Rated Movies",  
                                     fluidRow(),
                                     
                                     fluidRow(
                                         
                                         column(12,
                                                box( title = "Highest Rated Movies based on Input Criteria", solidHeader = TRUE, status = "info", width = 12,
                                                     dataTableOutput("tab7")
                                                     
                                                     
                                                )
                                                
                                                
                                                
                                         )
                                         
                                         
                                     ),
                            ),
                            
                            tabPanel("About",h3("Authors: Ansul Goenka, Parikshit Solunke",h3("Libraries Used: lubridate,plyr,dplyr,shiny,shinydashboard,ggplot2,DT,reshape"),h3("Data Source: IMDB data from ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/")))
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
        
        if(input$gnr != 'None'| input$rnt != 'None'|input$kwd != 'None'|input$crt != 'None'|input$gnr2 != 'None'|input$rnt2 != 'None'|input$kwd2 != 'None'|input$crt2 != 'None'|!is.null(input$typekwd[1]))
        {
            if(input$krd!='Type Keyword')
            {
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
            }
            else
            {
                df %>%
                    filter(
                        conditional(input$gnr != 'None' & input$gnr2=='None', Genre == input$gnr ),
                        conditional(input$gnr != 'None' & input$gnr2!='None', Genre == input$gnr|Genre == input$gnr2 ),
                        conditional(input$gnr == 'None' & input$gnr2 !='None', Genre == input$gnr2 ),
                        
                        conditional(input$rnt != 'None'& input$rnt2=='None', Run.Time>=rntlow & Run.Time<rnthigh ),
                        conditional(input$rnt != 'None'& input$rnt2!='None', (Run.Time>=rntlow & Run.Time<rnthigh) |(Run.Time>=rntlow2 & Run.Time<rnthigh2)  ),
                        conditional(input$rnt == 'None'& input$rnt2!='None', Run.Time>=rntlow2 & Run.Time<rnthigh2 ),
                        
                        
                        conditional(input$crt !='None' & input$crt2=='None', grepl(input$crt,Certificate ) ),
                        conditional(input$crt !='None' & input$crt2!='None', grepl(input$crt,Certificate )|grepl(input$crt2,Certificate ) ),
                        conditional(input$crt =='None' & input$crt2!='None', grepl(input$crt2,Certificate ) ),
                        
                        conditional(is.null(input$typekwd[2]) & is.null(input$typekwd[3]) , grepl(input$typekwd[1],Keywords)),
                        conditional(!is.null(input$typekwd[2]) & is.null(input$typekwd[3]) , grepl(input$typekwd[1],Keywords)|grepl(input$typekwd[2],Keywords)),
                        conditional(!is.null(input$typekwd[2]) & !is.null(input$typekwd[3]) , grepl(input$typekwd[1],Keywords)|grepl(input$typekwd[2],Keywords)|grepl(input$typekwd[3],Keywords)),
                        
                        
                    )
            }
        }
        else
        {
            df
        }
    })
    
    cfreq2<-reactive({
        cf1<-setNames(as.data.frame(table(unlist(strsplit(df2()$Certificate, ",", fixed=TRUE)))),c("Certificate","Number"))
        cf1$Certificate<-as.character(cf1$Certificate)
        
        cf1<-left_join(cfreq,cf1,by=c("Certificate"))
        colnames(cf1)<-c("Certificate","Number","Number2")
        cf1$Number[is.na(cf1$Number)] = 0
        cf1$Number2[is.na(cf1$Number2)] = 0
        if(input$crt!='None'|input$crt2!='None')
        {
            if(input$crt2=='None')
            {
                cf1<-cf1[cf1$Certificate==input$crt,]
                
            }
            else if(input$crt=='None')
            {
                cf1<-cf1[cf1$Certificate==input$crt2,]
            }
            else
            {
                cf1<- cf1[(cf1$Certificate==input$crt2|cf1$Certificate==input$crt ),] 
            }
            
        }
        
        cf1
    })
    
    
    kfreq2<-reactive({
        kf1<-setNames(as.data.frame(table(unlist(strsplit(df2()$Keywords, ",", fixed=TRUE)))),c("Keyword","Number"))
        kf1$Keyword<-as.character(kf1$Keyword)
        
        kf1<-left_join(kfreq,kf1,by=c("Keyword"))
        colnames(kf1)<-c("Keyword","Number","Number2")
        kf1$Number[is.na(kf1$Number)] = 0
        kf1$Number2[is.na(kf1$Number2)] = 0
        if(input$kwd!='None'|input$kwd2!='None')
        {
            if(input$kwd2=='None')
            {
                kf1<-kf1[kf1$Keyword==input$kwd,]
                
            }
            else if(input$kwd=='None')
            {
                kf1<-kf1[kf1$Keyword==input$kwd2,]
            }
            else
            {
                kf1<- kf1[(kf1$Keyword==input$kwd2|kf1$Keyword==input$kwd ),] 
            }
            
        }
        
        kf1
    })
    
    
    df3<-reactive({
        if(input$flt=='Year')
        {
            df2()[df2()$Year==input$ipyr,]
        }
        else if(input$flt=='Decade')
        {
            if(input$ipdc=='1910s')
            {
                df2()[(df2()$Year>=1910 & df2()$Year<1920 ),]
            }
            else if(input$ipdc=='1920s')
            {
                df2()[(df2()$Year>=1920 & df2()$Year<1930 ),]
            }
            else if(input$ipdc=='1930s')
            {
                df2()[(df2()$Year>=1930 & df2()$Year<1940 ),]
            }
            else if(input$ipdc=='1940s')
            {
                df2()[(df2()$Year>=1940 & df2()$Year<1950 ),]
            }
            else if(input$ipdc=='1950s')
            {
                df2()[(df2()$Year>=1950 & df2()$Year<1960 ),]
            }
            else if(input$ipdc=='1960s')
            {
                df2()[(df2()$Year>=1960 & df2()$Year<1970 ),]
            }
            else if(input$ipdc=='1970s')
            {
                df2()[(df2()$Year>=1970 & df2()$Year<1980 ),]
            }
            else if(input$ipdc=='1980s')
            {
                df2()[(df2()$Year>=1980 & df2()$Year<1990 ),]
            }
            else if(input$ipdc=='1990s')
            {
                df2()[(df2()$Year>=1990 & df2()$Year<2000 ),]
            }
            else if(input$ipdc=='2000s')
            {
                df2()[(df2()$Year>=2000 & df2()$Year<2010 ),]
            }
            else if(input$ipdc=='2010s')
            {
                df2()[(df2()$Year>=2010 & df2()$Year<2020 ),]
            }
            
        }
        else
        {
            df2()
        }
    })
    
    
    cfreq3<-reactive({
        cf2<-setNames(as.data.frame(table(unlist(strsplit(df3()$Certificate, ",", fixed=TRUE)))),c("Certificate","Number"))
        cf2$Certificate<-as.character(cf2$Certificate)
        
        
        cf2<-left_join(cfreq2()[,c(1,3)],cf2,by=c("Certificate"))
        colnames(cf2)<-c("Certificate","Number","Number2")
        cf2$Number[is.na(cf2$Number)] = 0
        cf2$Number2[is.na(cf2$Number2)] = 0
        
        cf2
        
    })
    
    kfreq3<-reactive({
        kf2<-setNames(as.data.frame(table(unlist(strsplit(df3()$Keywords, ",", fixed=TRUE)))),c("Keyword","Number"))
        kf2$Keyword<-as.character(kf2$Keyword)
        
        
        kf2<-left_join(kfreq2()[,c(1,3)],kf2,by=c("Keyword"))
        colnames(kf2)<-c("Keyword","Number","Number2")
        kf2$Number[is.na(kf2$Number)] = 0
        kf2$Number2[is.na(kf2$Number2)] = 0
        
        kf2
        
    })
    
    
    output$overview <- renderText({
        
        if(input$gnr != 'None'| input$rnt != 'None'|input$kwd != 'None'|input$crt != 'None'|input$gnr2 != 'None'|input$rnt2 != 'None'|input$kwd2 != 'None'|input$crt2 != 'None'|!is.null(input$typekwd[1]))
        {
            if(input$flt=='None')
            {  
                paste(h4(HTML('&emsp;')," Movies in database: ",totmov,HTML('&emsp;'), "||",  HTML('&emsp;'), "Movies fitting criteria: ",nrow(df2())))
            }
            else
            {
                paste(h4(HTML('&emsp;')," Movies in database: ",totmov,HTML('&emsp;'), "||",  HTML('&emsp;'), "Movies fitting criteria: ",nrow(df3())))
            }
        }
        else
        {
            if(input$flt=='None')
            {  
                paste(h4(HTML('&emsp;')," Movies in database: ",totmov,HTML('&emsp;'), "||" ,HTML('&emsp;'), "Avg. per Year: ",ayr,HTML('&emsp;'),"||",HTML('&emsp;'),"Avg. per Month: ",amt,HTML('&emsp;'),"||",HTML('&emsp;'),"Avg. Runtime(mins): ",art ))
            }
            else
            {
                paste(h4(HTML('&emsp;')," Movies in database: ",nrow(df),HTML('&emsp;'), "||",  HTML('&emsp;'), "Movies fitting criteria: ",nrow(df3())))
            }
            
            
        }
    })
    
    
    output$bar1 <- renderPlot({
        
        plot<-df3() %>% plyr::count("Year")
        if(input$flt=='None')
        {
            ggplot(data=plot, aes(x=Year, y=freq)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+ylab("Number") +scale_x_continuous("Year",breaks=seq(1913, 2017, 3)) +theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        else
        {
            if(input$flt=='Year')
            {
                ggplot(data=plot, aes(x=Year, y=freq)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.1)+ylab("Number") +scale_x_continuous("Year",breaks=seq(1913, 2017, 1))
                
            }
            else
            {
                ggplot(data=plot, aes(x=Year, y=freq)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+ylab("Number") +scale_x_continuous("Year",breaks=seq(1913, 2017, 1))
                
            }
        }
        
    })
    output$bar2 <- renderPlot({
        plot<-as.data.frame(table(cut(df2()$Run.Time,breaks=c(59,89,119,149,179,209,2000),labels=c("<90","90-120","120-150","150-180","180-210",">210"))) )
        colnames(plot)<-c("Runtime","Number")
        
        if(input$flt=='None')
        {
            
            ggplot(data=plot, aes(x=Runtime, y=Number)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+xlab("Runtimes(Minutes)")
        }
        else
        {
            
            plot2<-as.data.frame(table(cut(df3()$Run.Time,breaks=c(59,89,119,149,179,209,2000),labels=c("<90","90-120","120-150","150-180","180-210",">210"))) )
            colnames(plot2)<-c("Runtime","Number")
            plot$Number2<-plot2$Number
            
            m_dat <- melt(plot,id="Runtime")
            
            #create grouping variable
            m_dat$group <- gsub("2","",m_dat$variable)
            
            #plot based on selection
            ggplot(m_dat[m_dat$group=="Number",],
                   aes(x=Runtime,y=value,group=variable,fill=variable)) +
                geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_discrete(name = "Number", labels = c("Overall", "Selection"))+ylab("Number") 
            
            
            
        }
        
        
    })
    
    
    output$bar3 <- renderPlot({
        plot<-df2() %>% plyr::count("Month")
        colnames(plot)<-c("Month","Number")
        
        plot<-plot[complete.cases(plot$Month), ]
        if(input$flt=='None')
        {
            ggplot(data=plot, aes(x=Month, y=Number)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+ylab("Number")
        }
        else
        {
            plot2<-df3() %>% plyr::count("Month")
            colnames(plot2)<-c("Month","Number")
            
            plot2<-plot2[complete.cases(plot2$Month), ] 
            
            plot<-left_join(plot,plot2,by=c("Month"))
            colnames(plot)<-c("Month","Number","Number2")
            
            m_dat <- melt(plot,id="Month")
            
            #create grouping variable
            m_dat$group <- gsub("2","",m_dat$variable)
            
            #plot based on selection
            ggplot(m_dat[m_dat$group=="Number",],
                   aes(x=Month,y=value,group=variable,fill=variable)) +
                geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_discrete(name = "Number", labels = c("Overall", "Selection"))+ylab("Number")
            
        }
        
    })
    
    
    
    output$bar4 <- renderPlot({
        plot<-df2() %>% plyr::count("Genre")
        colnames(plot)<-c("Genre","Number")
        if(input$flt=='None')
        {
            ggplot(data=plot, aes(x=Genre, y=Number )) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+ylab("Number") +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Number")
        }
        else
        {
            plot2<-df3() %>% plyr::count("Genre")
            colnames(plot2)<-c("Genre","Number")
            plot<-left_join(plot,plot2,by=c("Genre"))
            colnames(plot)<-c("Genre","Number","Number2")
            
            m_dat <- melt(plot,id="Genre")
            
            #create grouping variable
            m_dat$group <- gsub("2","",m_dat$variable)
            ggplot(m_dat[m_dat$group=="Number",],
                   aes(x=Genre,y=value,group=variable,fill=variable)) +
                geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_discrete(name = "Number", labels = c("Overall", "Selection"))+ylab("Number")
            
            
            
            
        }
        
    })
    
    output$bar5 <- renderPlot({
        
        if(input$flt=='None')
        {
            ggplot(data=cfreq2(), aes(x=Certificate, y=Number2)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+ scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000,14000))+ylab("Number")
        }
        else
        {
            
            
            
            m_dat <- melt(cfreq3(),id="Certificate")
            
            #create grouping variable
            m_dat$group <- gsub("2","",m_dat$variable)
            ggplot(m_dat[m_dat$group=="Number",],
                   aes(x=Certificate,y=value,group=variable,fill=variable)) + geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Number") + scale_fill_discrete(name = "Number", labels = c("Overall", "Selection"))
            
        }
    })
    
    output$bar6 <- renderPlot({
        
        
        if(input$flt=='None')
        {
            ggplot(data=kfreq2(), aes(x=Keyword, y=Number2)) + geom_bar(stat="identity",fill="#FF9999" ,width=0.8)+ coord_flip()+ylab("Number")
        }
        else
        {
            
            
            
            m_dat <- melt(kfreq3(),id="Keyword")
            
            #create grouping variable
            m_dat$group <- gsub("2","",m_dat$variable)
            ggplot(m_dat[m_dat$group=="Number",],
                   aes(x=Keyword,y=value,group=variable,fill=variable)) + geom_bar(stat="identity",position="dodge")+ coord_flip()+ scale_fill_discrete(name = "Number", labels = c("Overall", "Selection"))+ylab("Number")
            
        }    
    })
    
    output$tab1<-renderDataTable({
        if(input$flt=='None')
        {
            plot<-df2() %>% plyr::count("Year")
        }
        else
        {
            plot<-df3() %>% plyr::count("Year")
            
        }
        colnames(plot)<-c("Year","Number")
        datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 5,dom='tp'))
    })
    
    
    output$tab2<-renderDataTable({
        if(input$flt=='None')
        {
            plot<-as.data.frame(table(cut(df2()$Run.Time,breaks=c(59,89,119,149,179,209,2000),labels=c("60-90","90-120","120-150","150-180","180-210",">210"))) )
        }
        else
        {
            plot<-as.data.frame(table(cut(df3()$Run.Time,breaks=c(59,89,119,149,179,209,2000),labels=c("60-90","90-120","120-150","150-180","180-210",">210"))) )
            
        }
        colnames(plot)<-c("Runtime(Minutes)","Number")
        
        datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
    })
    
    output$tab3<-renderDataTable({
        if(input$flt=='None')
        {
            plot<-df2() %>% plyr::count("Month")
        }
        else
        {
            plot<-df3() %>% plyr::count("Month")
        }
        plot<-plot[complete.cases(plot$Month), ]
        colnames(plot)<-c("Month","Number")
        datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
    })
    
    output$tab4<-renderDataTable({
        if(input$flt=='None')
        {
            plot<-df2() %>% plyr::count("Genre")
        }
        else
        {
            plot<-df3() %>% plyr::count("Genre")
        }
        colnames(plot)<-c("Genre","Number")
        datatable(plot,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 5,dom='tp'))
    })
    
    output$tab5<-renderDataTable({
        if(input$flt=='None')
        {
            
            datatable(cfreq2()[,c(1,3)],colnames=c("Certificate", "Number"),selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
        }
        else
        {
            datatable(cfreq3()[,c(1,3)],colnames=c("Certificate", "Number"),selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
        }
    })
    
    output$tab6<-renderDataTable({
        if(input$flt=='None')
        {
            
            datatable(kfreq2()[,c(1,3)],colnames=c("Keyword", "Number"),selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
        }
        else
        {
            datatable(kfreq3()[,c(1,3)],colnames=c("Keyword", "Number"),selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 6,dom='tp'))
        }
    })
    output$tab7<-renderDataTable({
        if(input$flt=='None')
        {
            plot<- df2()[,c(2,3,6,7,9,5)]
            
        }
        else
        {
            plot<- df3()[,c(2,3,6,7,9,5)]
            
        }
        plot<-plot[!duplicated(plot$Movie),]
        plot<-plot[order(-plot$Rating),]
        plot<-plot[1:10,]
        datatable(plot,colnames=c("Movie", "Year","Certificate","Genre","Runtime(Mins)","Rating"),rownames=FALSE,selection =list(mode = 'none'),options = list(pageLength = 10,dom='t'))
        
    })
}
shinyApp(ui = ui, server = server)