#loading the libraries========================================
library(shiny)
library(shinyWidgets)#shinyWidgets: Custom inputs widgets for Shiny
library(tidyr)#contains tools for changing the shape of a dataset
library(stats)#for summarise 
library(dplyr)#data manipulation easier.
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(readr)# to read rectangular text data
library(stringr) #provide a cohesive set of functions designed to make working with strings as easy as possible
library(DT)#Helper functions for using DT in Shiny
library(base)


#Loading rhe Dataset==============================================
load('fileAirBnB.Rdata')

#================Landing page with description of the dataset ==================
ui <- navbarPage(strong(
    "AirBnB Paris"),
    intro_panel <- tabPanel(strong(
        "Introduction"),
        h1(strong("AirBnB")),
        p(strong("Analysis and Visualization of AirBnB Dataset")),
        img(src = "Airbnb_Cover.jpg", 
            height = 400, 
            width = 800),
        p("Image Source: www.iphonemod.net/wp-content/uploads/2018/06/Airbnb_Cover.jpg"),
        br(),
        br(),
        
    ),
    #========= Question 1:Relationship between prices and apartment ================
    second_panel <- tabPanel(strong(
        "Visualisation"),
        navbarPage('',
                   tabPanel(strong('Price and apartment'),
                            titlePanel(h4("Select type")),
                            sidebarLayout(
                                sidebarPanel(width = 2,
                                             radioButtons(
                                                 inputId = "rbed",
                                                 label = "Types:",
                                                 c("Room_type"="rt","Bed_type"="bt")
                                             )
                                ),
                                mainPanel( h3("Relationship between prices and apartment",align='center',width=8),
                                           plotOutput('Boxplot')
                                )
                            )
                   ),
                   
                   #========================Question2:Number of apartments per owner===============
                   tabPanel(strong('Number of apartment per owner'),
                            titlePanel(h4("Select analysis")),
                            sidebarLayout(
                                sidebarPanel(width=2,
                                             radioButtons(
                                                 inputId = "apartment",
                                                 label = "options:",
                                                 c("Barplot"="bp","Scatterplot"="sc")),
                                             sliderInput(
                                                 inputId = "obs",
                                                 label = "Number of owner",
                                                 min=1,
                                                 max = 50,
                                                 value = 10)),
                               mainPanel (h3("Number of apartments per owner",align='center',width=8),
                                    plotOutput('numplot'),
                                   
                                )
                            )
                            
                   ),
                   #========================Question2:Ownership information===============
                   tabPanel(strong('Ownership information'),
                                    DT::dataTableOutput("tableplot",width = 550,height = 300)
                   ),
                   #==========Question3:Renting prices per city quarter ("arrondissements") =======
                   tabPanel(strong('Renting Prices'),
                            titlePanel(h4("List of Prices")),
                            sidebarLayout(
                                sidebarPanel(width = 2,
                                             radioButtons(
                                                 inputId = "Arrod",
                                                 label = "Prices:",
                                                 c("Average Price"="op","All Arrodisement"="Al","Per Arrodisement"="qp")),
                                             selectInput(
                                                 inputId = "inSelect",
                                                 label = "Select the Arrondisement",
                                                 choices = c("Paris-17E-Arrondissement",
                                                             "Paris-7E-Arrondissement",
                                                             "Paris-19E-Arrondissement ",
                                                             "Paris-20E-Arrondissement",
                                                             "Paris-9E-Arrondissement",
                                                             "Paris 9e Arrondissement",
                                                             "Paris-10E-Arrondissement",
                                                             "Paris-11E-Arrondissement",
                                                             "10th arrondissement, Paris",
                                                             "Paris-13E-Arrondissement",
                                                             "Paris-14E-Arrondissement",
                                                             "Paris-15E-Arrondissement",
                                                             " Paris-12E-Arrondissement",
                                                             "Paris-1ER-Arrondissement",
                                                             "Paris-2E-Arrondissement",
                                                             "Paris-6E-Arrondissement",
                                                             "Paris 6e arrondissement",
                                                             "Paris-8E-Arrondissement",
                                                             "Paris-3E-Arrondissement",
                                                             "Paris-16E-Arrondissement",
                                                             "Paris 11e arrondissement",
                                                             "Paris-18E-Arrondissement",
                                                             "Paris-4E-Arrondissement",
                                                             "Paris 4e arrondissement",
                                                             "Paris, 4th arrondissement"),
                                                 multiple = TRUE
                                             ),
                                             sliderInput(
                                                 inputId = "range",
                                                 label = "Average prices",
                                                 min=60,
                                                 max = 500,
                                                 value = 300
                                                 
                                             )
                                ),
                                mainPanel(h3("Renting prices per city quarter arrondissements",align='center'),
                                          plotOutput('rentplot')
                                )
                            )
                   ), 
                   
                   #======Question4: Visit frequency of the different quarters according to time ==
                   tabPanel(strong('Frequency visit'),
                            titlePanel(h4("Frequency visit")),
                            sidebarLayout(
                                sidebarPanel(width = 2,
                                             radioButtons("mtyr","Time:",c("Year"="yr","Month"="mt")),
                                             sliderInput(
                                                 inputId = "yrr",
                                                 label = "Year Range",
                                                 min= 2012,
                                                 max = 2016,
                                                 value = 2014 ,
                                                 sep = ""),
                                             sliderTextInput(
                                                 inputId = "mtr",
                                                 label = "Month Range",
                                                 choices = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))),
                                mainPanel(h3("Visit frequency of the different quarters according to time",
                                             align='center'),
                                          plotOutput('barplot'),width = 8
                                )
                            )
                   )
        )
    )
)

#================================server==============================
# Create server ====================================
server <- function(input, output,session) {

#==================== Question 1 =============================================
#=====Visualization and plotting for Q1 =======================================
#==== Relationship between prices and apartment features =======================
#==== Display box plots showing variation of prices according to room types===
#=====and bed types ===========================================================
output$Boxplot <- renderPlot({

rbed <- switch(input$rbed,
               rt = 1,
               bt = 2)
if (rbed == 1){
    Price_apartment <- AirBnB %>%select(price,room_type,property_type,bed_type)%>%
        filter(property_type=='Apartment',price<200)%>%
        group_by(room_type)
    ggplot(Price_apartment)+geom_boxplot(aes(x=room_type,y= price),
                                         fill ="coral",colour = "#3366FF")+
        labs(
            x="Room_type" ,
            y="Prices ($)" ,
            title="Statistical Analysis of prices by room_type")+
        theme(
            plot.title = element_text(size = 16,color = 'blue',face = 'bold'))
}
else{
    Price_apartment <- AirBnB %>%select(price,property_type,bed_type)%>%
        filter(property_type=='Apartment',price<200)%>%
        group_by(bed_type)
    ggplot(Price_apartment)+geom_boxplot(aes(x=bed_type,y= price),
                                         fill = "coral",colour = "#3366FF")+
        labs(x="Bed_type" ,
             y="Prices ($)" ,
             title="Statistical Analysis of prices by Bed_type")+
        theme(plot.title = element_text(size = 16,color = 'blue',face = 'bold'))
    
}
})
#=========================Question 2================================
#=====Server components for visualization of analysis results =======
#====== on number of apartments per owner ===========================
#======================================================================
output$numplot <- renderPlot({
    apt <- switch(input$apartment,
                   bp = 1,
                   sc = 2,
                   tb =3)
    if (apt == 1){
    NumberOfApartmentsPerOwner <- AirBnB %>% select(Apartment_id,host_id,host_name,property_type)%>%
        filter(property_type == "Apartment")%>%
        group_by(host_name)%>%summarise(Total=n_distinct(Apartment_id))%>%
        arrange(desc(Total))
    num_ow = input$obs
    num_list <- rep(0,num_ow)
    for (i in 1:num_ow){
        if (i < 1){
            num_list[i] = length(which(NumberOfApartmentsPerOwner$Total == i))
        }
        else{
            num_list[i] = length(which(NumberOfApartmentsPerOwner$Total >= i))
        }
    }
    df_number_apartment <- data.frame(num_list,Number_per_owner=c(1:num_ow))
    ggplot(df_number_apartment,aes(Number_per_owner,num_list))+
        geom_bar(stat ="identity",fill='coral')+
        labs(
            y="Number of persons" ,
            x="Number of apartment per owner",
            title="Analysis of number of apartments per owner: x indicates >= x",
            subtitle = "Slide to increase the number")+
        theme(
            plot.title = element_text(size = 16,color = 'blue',face = 'bold'),    
            plot.subtitle = element_text(size = 14,color = 'red','italic'))
    


}else{
    ggplot(NumberOfApartmentsPerOwner,aes(Total,Total))+geom_point(aes(colour ="red"))+
        theme(legend.position = "none")+
        labs(
            x ="Number of apartment per owner",
            y ="Number of apartment per owner",
            title = "Scatter plot for number of apartment per owner"
        )+ theme(
            plot.title = element_text(size = 16,color = 'blue',face = 'bold'))

}  
})

#===================
output$tableplot <- DT::renderDataTable({
    NumberOfApartmentsPerOwner <- AirBnB %>% select(Apartment_id,host_id,host_name,property_type)%>%
        filter(property_type == "Apartment") %>%
        group_by(host_name) %>%
        summarise(Total=n_distinct(Apartment_id))%>%arrange(desc(Total))
    datatable(NumberOfApartmentsPerOwner,
              colnames = c('Name_of_owner'=2,'Number_of_Apartment'=3),
              caption = 'Table 1: This table shows the number of owner per apartment.')
})
#=======================Question3===============================
#==============Plotting of anaylysis results on renting price===
#=per arrondissements (city quarters) =========================
#========= Visualization options include=======================
#====== Average price per city Quarter; prices per apartment ==
#=== and price statistics for all arrondisements ==============
#==============================================================
output$rentplot <- renderPlot({
    target <-c("Paris-17E-Arrondissement",
               "Paris-7E-Arrondissement",
               "Paris-19E-Arrondissement ",
               "Paris-20E-Arrondissement",
               "Paris-9E-Arrondissement",
               "Paris 9e Arrondissement",
               "Paris-10E-Arrondissement",
               "Paris-11E-Arrondissement",
               "10th arrondissement, Paris",
               "Paris-13E-Arrondissement",
               "Paris-14E-Arrondissement",
               "Paris-15E-Arrondissement",
               " Paris-12E-Arrondissement",
               "Paris-1ER-Arrondissement",
               "Paris-2E-Arrondissement",
               "Paris-6E-Arrondissement",
               "Paris 6e arrondissement",
               "Paris-8E-Arrondissement",
               "Paris-3E-Arrondissement",
               "Paris-16E-Arrondissement",
               "Paris 11e arrondissement",
               "Paris-18E-Arrondissement",
               "Paris-4E-Arrondissement",
               "Paris 4e arrondissement",
               "Paris, 4th arrondissement")
    alqb <- switch(input$Arrod,
                   qp = 1,
                   Al= 2,
                   op=3)
    if (alqb == 1){
        allarr = input$inSelect
        arrondissement1 <-AirBnB%>%select(city,price)%>%
            filter(city%in%allarr,price<200)%>%group_by(city)  
        ggplot(arrondissement1)+geom_boxplot(aes(x=price,y=city),
                                             fill = "coral",colour = "#3366FF")+
            labs( x="Prices ($)" ,
                  y="Arrondisements",
                  title = "Statistical Analysis of prices per apartment",
                  subtitle = "Select the Arrondisement to view")+
            theme(
                plot.title = element_text(size = 16,color = 'blue',face = 'bold'),    
                plot.subtitle = element_text(size = 14,color = 'red','italic'))
    }else if(alqb==2){
        arrondissement <-AirBnB%>%select(city,price)%>%
            filter(city%in%target,price<200)%>%group_by(city)
        TabArr <- arrondissement %>%summarise(mean=mean(price),
                                              median=median(price),
                                              max=max(price),
                                              min=min(price))
        A <- matrix(data=0,nrow=23,ncol = 4)
        A[,1] = TabArr$min
        A[,2] = TabArr$max
        A[,3] = TabArr$mean
        A[,4] = TabArr$median
        boxplot(A, 
                col=c("red","green","blue","orange"),
                names = c("Minimum", "Maximum", "Mean", "Median"),
                main ="Price Statistics for all Arrondissement",
                ylab = "Prices ($)")
    }else{
        arrondissement <-AirBnB%>%select(city,price)%>%filter(city%in%target,price<200)%>%group_by(city)%>%arrange(desc(price))
        TabArr <- arrondissement %>% group_by(city)%>%summarise(mean=mean(price),
                                                                median=median(price),
                                                                max=max(price),
                                                                min=min(price))
        avg_price <- input$range
        TabArr1 <- TabArr%>%select(city,mean)%>%filter(mean <= avg_price)
        ggplot(TabArr1,aes(mean,city,fill=factor(city)))+geom_bar(stat = "identity")+
            theme(legend.position = "none")+
            labs(
                x="Prices ($)" ,
                y="Arrondisements",
                title = "Average prices per Arrodisement",
                subtitle = "Slide the Average prices to view")+
            theme(
                plot.title = element_text(size = 16,color = 'blue',face = 'bold'),    
                plot.subtitle = element_text(size = 14,color = 'red','italic'))
        
    }
})

#=================Question4=============================================
#============= Visualization of the visit frequency for the quarters===
#== The display shows two figure plotted based on number of visits ====
#==== per year or month with option to select year and month range ====
#======================================================================
output$barplot <- renderPlot({
    target <-c("Paris-17E-Arrondissement",
               "Paris-7E-Arrondissement",
               "Paris-19E-Arrondissement ",
               "Paris-20E-Arrondissement",
               "Paris-9E-Arrondissement",
               "Paris 9e Arrondissement",
               "Paris-10E-Arrondissement",
               "Paris-11E-Arrondissement",
               "10th arrondissement, Paris",
               "Paris-13E-Arrondissement",
               "Paris-14E-Arrondissement",
               "Paris-15E-Arrondissement",
               " Paris-12E-Arrondissement",
               "Paris-1ER-Arrondissement",
               "Paris-2E-Arrondissement",
               "Paris-6E-Arrondissement",
               "Paris 6e arrondissement",
               "Paris-8E-Arrondissement",
               "Paris-3E-Arrondissement",
               "Paris-16E-Arrondissement",
               "Paris 11e arrondissement",
               "Paris-18E-Arrondissement",
               "Paris-4E-Arrondissement",
               "Paris 4e arrondissement",
               "Paris, 4th arrondissement")
    mtyr <- switch(input$mtyr,
                   yr = 1,
                   mt = 2)
    if (mtyr == 1){
        selecty <- input$yrr
        yearList <- c(2012,2013,2014,2015,2016)
        Indxc = which(yearList == selecty)
        frequency_years_visit <-AirBnB%>%select(city,Year)%>%filter(city%in%target,Year%in%yearList[1:Indxc])
        ggplot(data = frequency_years_visit) + 
            geom_bar(aes(y = factor(city),fill=factor(Year)))+guides(fill = guide_legend(reverse=TRUE))+
            labs(
                x="Frequency" ,
                y="Arrondissements" ,
                fill="Year",
                title="Visit frequency per year from 2012 -- 2016")+
            theme(
                plot.title = element_text(size = 16,color = 'blue',face = 'bold'))
    }
    else{
        selectmt <- input$mtr
        monthList <-c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        Indxmm = which(monthList == selectmt)
        Frequency_month_visit <-AirBnB%>%select(MonthAbb,city)%>%
            filter(city%in% target,MonthAbb%in%monthList[1:Indxmm])
        ggplot(data = Frequency_month_visit) + 
            geom_bar(aes(y = factor(city),fill=factor(MonthAbb)))+ 
            scale_fill_discrete(name = "Month", 
                                labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
            labs(
                x="Frequency" ,
                y="Arrondissements" ,
                fill="month",
                title="Visit frequency per month from 2012 -- 2016")+
            theme(
                plot.title = element_text(size = 16,color = 'blue',face = 'bold'))
    }
})
#server==================================================================
}


# Run the application 
shinyApp(ui = ui, server = server)
