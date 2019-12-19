#Loading libraries
library(shinythemes)
library(devtools)
library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(gganimate)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)
library(dichromat)
library(viridis)
library(Hmisc)
library(reshape2)
library(dashboardthemes)
library(shinyWidgets)

#Loading data
load("gender_ineq.RData")


#---------------------------------------------------------------------------------------------

#Shiny

ui <- dashboardPage( skin="purple",      # FULL PAGE
                     
  #### Header zone
  dashboardHeader(title="Gender Inequality Dashboard"),   
  
  #### Sidebar zone
  dashboardSidebar(
    
    ### start css section
    tags$head(tags$style(HTML('
        body {
          font-family: "Avenir";
          font-weight: 300;
        
        }
        .sidebar {
          color: #cccaca;
          background-color: #2c3031;
        
        }
         .nav-tabs-custom>.nav-tabs>li.header {
        font-size: 18px;
        font-weight: 500;
        color: #444;
        }
        .h2 , h2{
        color: white;
        font-family: "Avenir";
        font-weight: 300;
        font-size: 21px;
        }
        .h1, .h3, .h4, .h5, h1, h3, h4, h5 {
        color: white;
        font-family: "Avenir";
        font-weight: 300;
        font-size: 16px;
        }
    '))),
    setSliderColor(c("red"), c(1)),
    ### end css section
     
    ### slider inputs section 
    column(12, h2("Inputs")),                                # Element 1: a simple title
    sliderInput("year", h4("Year"),                          # Element 2: year selection
                min = 2000, max = 2018, 
                value = c(2000, 2018), step = 2, sep = ""),
    
    checkboxGroupInput("gender", h4("Gender"),               # Element 3: gender selection
                       choices = list("Women" = "Women", 
                                      "Men" = "Men"),
                       selected = c("Women", "Men")),
    numericInput("max_country_gov", h4("Max number of countries:      Governance Data"),  
                 min = 2, max = 200, step = 1, value = 30),
    numericInput("max_country_dev", h4("Development Data"),      
                 min = 2, max = 1000, step = 1, value = 200),
    numericInput("max_country_emp", h4("Employment Data"), 
                 min = 2, max = 200, step = 1, value = 30)
    
  ),
  
  ##---------------------------------------------------------------------------------------------
  
  dashboardBody(
      # shinyDashboardThemes(
      #  theme = "onenote"
      # ),
    
    ### start css section
    tags$head(tags$style(HTML('
        .main-header .logo {
          font-family: "Avenir", Times, "Times New Roman", serif;
          font-weight: 300;
          font-size: 22px;
        }
        
    '))),
    ### end css section
    
    ### Body content section
    
    #boxes
    fluidRow(
      valueBoxOutput("box_gendwg_max", width =4),
      infoBoxOutput("box_par_max", width = 5),
      valueBoxOutput("box_vio_max", width = 3)
    ),   
    #tabs                                                                                                 
    fluidRow(   
      tabsetPanel(type = "tabs",
      tabBox(
        title = "Results: Graphs & Tables", width = 12,  # Width in bootstrap, height in px
       
        
        tabPanel("Inequality in Governance", 
                 fluidRow(
                   column(12,plotlyOutput("plot_gov_min", height = 300))),
                 fluidRow(
                   column(12,plotlyOutput("plot_gov_par", height = 300))),
                 fluidRow(
                   column(5,plotlyOutput("plot_gov_par_share", height = 300)),
                   column(7,plotlyOutput("plot_gov_cen", height = 300))),
                 fluidRow(
                   column(12,DT::dataTableOutput("pt_gov", height = 300)))
                  
        ),
        tabPanel("Inequality in Development",
                 fluidRow(
                   column(12,plotlyOutput("plot_dev_viol", height = 300))),
                 fluidRow(
                   column(12,plotlyOutput("plot_dev_prev", height = 300))),
                 fluidRow(
                   column(12,plotlyOutput("plot_dev_prev_vlin", height = 500))),
                 fluidRow(
                   column(12,plotOutput("plot_dev_law_viol", height = 500))),
                 fluidRow(
                   column(12,DT::dataTableOutput("pt_dev", height = 300)))
                      
                 ),
        
        tabPanel("Inequality in Employment",
                 fluidRow(
                   column(12,plotlyOutput("plot_empl_unemp", height = 500)),
                   column(12,DT::dataTableOutput("pt_emp", height = 300)))
        )
        )
      )
    
)))


#---------------------------------------------------------------------------------------------

server <- function(input,output){  
    #### Reactive Section starts
    data_gov <- reactive({                # Creates the dynamic data
      governance <- governance %>%      
        filter(Year >= input$year[1], Year <= input$year[2])
      return(governance)
    })
    data_wg <- reactive({                # Creates the dynamic data
      wage_gap <- wage_gap %>%      
        filter(Time >= input$year[1], Time <= input$year[2])
      return(wage_gap)
    })
    data_dev <- reactive({                # Creates the dynamic data
      development <- development %>%      
        filter(Time >= input$year[1], Time <= input$year[2])
      return(development)
    })
    data_emp <- reactive({                # Creates the dynamic data
      employment <- employment %>%      
        filter(Year>= input$year[1], Year <= input$year[2],
               Sex %in% input$gender)
      return(employment)
    })
    
    #### Reactive Section ends
    
    #---------------------------------------------------------------------------------------------
    
    #### Output Section starts
  
    ### value box 
    output$box_gendwg_max <- renderValueBox({
      valueBox(
        value = "34.6%",
        subtitle =  "Highest gender wage gap in 2017 in South Korea.",
        icon = icon("balance-scale", lib = "font-awesome"),
        color = "red"
      )
    })
    
    output$box_par_max <- renderInfoBox({
      infoBox(
        "More than", "53.1%", "of women suffer domestic violence in Sub-Saharan Africa." ,icon = icon("home"),
        color = "black", fill=TRUE
      )
    })
    
    output$box_vio_max <- renderValueBox({
      valueBox(
        value = "43.7%",
        subtitle =  "of women in Swedish parliament in 2015.",
        icon = icon("landmark", lib = "font-awesome"),
        color = "teal"
      )
    })
    
    ###---------------------------------------------------------------------------------------------
    
    ### Plots
    ## gov_plots
    #  1. plot_gov_min: Share of women ministers
    output$plot_gov_min <- renderPlotly({ 
      gov_min <- data_gov() %>%
          filter(IND=="EMPW_MIN")  %>%
         head(input$max_country_gov*2)
      gov_min %>%  ggplot(aes(x=Country, y=Value, fill=factor(Year))) + 
          geom_bar(stat="identity", position = "dodge2")+
          labs(title = "Share of women ministers",
             x = "",
             y = "Percentage %") +
         theme(text = element_text(size = 11),
              axis.text.x = element_text(angle = 70, 
                                         size = 7,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 11,
                                         hjust = 1)
         ) + guides(fill=guide_legend(title="Year"))
       })
      # 2. plot_gov_par: Share of women parliamentarians
      output$plot_gov_par <- renderPlotly({ 
       gov_par <- data_gov() %>%
       filter(Indicator=="Share of women parliamentarians")  %>%
       head(input$max_country_gov*2)
       gov_par %>%  ggplot(aes(x=Country, y=Value, fill=factor(Year))) + 
          geom_bar(stat="identity", position = "dodge2")+
           labs(title = "Share of women parliamentarians",
               x = "",
               y = "Percentage %") +
            theme(text = element_text(size = 11),
            axis.text.x = element_text(angle = 70, 
                                       size = 7,
                                       hjust = 1),
            axis.text.y = element_text(angle = 90, 
                                       size = 11,
                                       hjust = 1)
            )+ guides(fill=guide_legend(title="Year"))
        })
      # 3. plot_gov_par_share: Average share of women parliamentarians by country
      output$plot_gov_par_share <- renderPlotly({ 
      g_piv_share <- data_gov() %>%
        filter(Indicator=="Share of women parliamentarians") %>%
        head(input$max_country_gov)
      g_piv_share %>%
        group_by(Country) %>%             
        summarise(avg_share = mean(Value)) %>%  
        ggplot(aes(x=Country, y=avg_share, fill=Country)) + 
        geom_bar(stat="identity") + 
        labs(title = "Average share of women parliamentarians",
             x = "",
             y = "Percentage %") +
        theme(text = element_text(size = 9),
              axis.text.x = element_text(angle = 70, 
                                         size = 7,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 11,
                                         hjust = 1)
        )
      })
      # 4. output$plot_gov_cen: Share of central government employment filled by women
      output$plot_gov_cen <- renderPlotly({ 
      gov_cen <-  data_gov() %>%
        filter(Indicator=="Share of central government employment filled by women", Year==2015)  %>% 
        head(input$max_country_gov)
      gov_cen %>%
        ggplot(aes(x=Country, y=Value, fill=Country)) + 
        geom_bar(stat = "identity")+
        labs(title = "Share of women central government employment",
             x = "",
             y = "Percentage %") +
        theme(text = element_text(size = 9),
              axis.text.x = element_text(angle = 70, 
                                         size = 7,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 11,
                                         hjust = 1)
        )
        })
      
      ##---------------------------------------------------------------------------------------------
      
      ## dev_plots
      #  1. Attitudes towards violence: plot_dev_viol
      output$plot_dev_viol <- renderPlotly({ 
        dev_viol <- data_dev() %>%
          filter(Variables=="Attitudes towards violence") %>% 
          head(input$max_country_dev)
        dev_viol <- dev_viol %>% 
          ggplot(aes(x =Region, y=Value*100, color=Region, label=Country)) + 
          geom_jitter(alpha=0.8, size = 3)+
          labs(title="Attitudes towards violence",
               x = "",
               y = "Percentage %") +
          theme(text = element_text(size = 10),
                axis.text.x = element_text(angle = 30, 
                                           size = 7,
                                           hjust = 1),
                axis.text.y = element_text(angle = 90, 
                                           size = 8,
                                           hjust = 1)
          ) 
        p <- ggplotly(dev_viol)%>%
          layout(title = list(text = paste0('Attitudes towards violence',
                                            '<br>',
                                            '<sup>',
                                            '% of women who agree that a husband is justified in beating his wife',
                                            '<sup>')))
      }) 
      
      # 2. Prevalence of violence in the lifetime: plot_dev_prev
      output$plot_dev_prev <- renderPlotly({ 
      dev_prev <- data_dev() %>%
        filter(Variables=="Prevalence of violence in the lifetime") %>% 
        head(input$max_country_dev)
      dev_prev <- dev_prev %>% 
        ggplot(aes(x =Region, y=Value*100, color=Region, label=Country)) + 
        geom_jitter(alpha=0.8, size = 3)+
        labs(title = "Prevalence of violence in the lifetime",
             x = "",
             y = "Percentage %") +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle = 30, 
                                         size = 7,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 8,
                                         hjust = 1)
        )  +scale_colour_viridis_d()  
      p <- ggplotly(dev_prev) %>%
        layout(title = list(text = paste0('Prevalence of violence in the lifetime',
                                          '<br>',
                                          '<sup>',
                                          '% of women who have experienced physical or sexual violence from an intimate partner in their lives',
                                          '<sup>')))
      
      })
      
      # 3. Prevalence of violence in the lifetime-violin: plot_dev_prev_vlin
      output$plot_dev_prev_vlin <- renderPlotly({ 
      dev_prev_vlin <- data_dev() %>%
          filter(Variables=="Prevalence of violence in the lifetime", Region != "North America") %>% 
          head(input$max_country_dev)
      dev_prev_vlin <- dev_prev_vlin %>% 
        ggplot(aes(x =Region, y=Value*100, fill=Region),trim=FALSE) + 
        geom_violin(scale="area", lwd=0.2)+ #binaxis = "y", stackdir = "center"
        labs(title = "Prevalence of violence in the lifetime",
             x = "",
             y = "Percentage %") +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle = 40, 
                                         size = 8,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 8,
                                         hjust = 1)
        )  + stat_summary(fun.data="mean_sdl", mult=1, 
                          geom="pointrange", size=0.5, color= "red",alpha=0.6,aes(lwd=0.05))
      p <- ggplotly(dev_prev_vlin) %>%
        layout(title = list(text = paste0('Prevalence of violence in the lifetime',
                                          '<br>',
                                          '<sup>',
                                          '% of women who have experienced physical or sexual violence from an intimate partner in their lives',
                                          '<sup>')))
      })
      
      
      #  4. Laws on domestic violence: plot_dev_law_viol
      output$plot_dev_law_viol <- renderPlot({ 
      dev_law_viol <- data_dev() %>%
        filter(Variables=="Laws on domestic violence") %>% 
        head(input$max_country_dev)
      dev_law_viol %>%
        ggplot(aes(x =Region, y=factor(Value), color=factor(Value), label=Country)) + 
        geom_jitter(size=4,alpha=0.5)+    #binaxis = "y", stackdir = "center" 
        labs(title = "Laws on domestic violence",
             subtitle = "Whether the legal framework offers women legal protection from domestic violence.",
             color = "Degree of law framework",
             x = "",
             y = "Degree") +
        
        scale_color_discrete(
          name = "Degree of law framework", 
          labels = c("0: specific legislation in place to address domestic violence", 
                     "0.25: specific legislation in place to address domestic violence, but some problems of implementation were reported.", 
                     "0.5: specific legislation in place to address domestic violence, but the law is inadequate.", 
                     "0.75: no specific legislation in place to address domestic violence, but evidence of legislation being planned or drafted.", 
                     "1:  no legislation in place to address domestic violence."))+
        
        theme(text = element_text(size = 12),
              title = element_text(size = 12, face="bold"),
              legend.text= element_text(size=12),
              legend.position = "bottom",
              legend.direction = "vertical",
              axis.text.x = element_text(angle = 30, 
                                         size = 10,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 10,
                                         hjust = 1)
              
        )
    
       })
      
      #---------------------------------------------------------------------------------------------
      
      ## emp_plots
      #  1. Unemployment rate, by sex and age group: plot_empl_unemp
      output$plot_empl_unemp <- renderPlotly({ 
      empl_unemp <- data_emp () %>%
        filter(Indicator=="Unemployment rate, by sex and age group", Year ==2015) %>% 
        head(input$max_country_emp*2)
      empl_unemp <- empl_unemp %>%
        ggplot(aes(x=Country, y=Value, fill=Sex)) + 
        geom_bar(stat="identity",position = "stack")+
        labs(title = "Unemployment rate in 2015",
             x = "",
             y = "Percentage %") +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle = 70, 
                                         size = 7,
                                         hjust = 1),
              axis.text.y = element_text(angle = 90, 
                                         size = 8,
                                         hjust = 1)
        ) 
  
      })
      
      #---------------------------------------------------------------------------------------------
      
      ### Tables
      ## Pivot Table - Governance
      output$pt_gov <- DT::renderDataTable({
      piv_gov <- data_gov() %>% 
        group_by(Indicator, Country) %>%             
        summarise("Average Value" = mean(Value)) %>%
        head(input$max_country_gov)
      
      })
      
      ## Pivot Table - Development
      output$pt_dev <- DT::renderDataTable({
      piv_dev <- data_dev() %>%
        group_by(Variables, Region) %>%             
        summarise("Average Value" = round(mean(Value),2))
      })
      
      output$pt_emp <- DT::renderDataTable({
        data_emp() 
    
       })


}


#---------------------------------------------------------------------------------------------

#Run the app
shinyApp(ui = ui, server = server)  # Aggregates the app.
