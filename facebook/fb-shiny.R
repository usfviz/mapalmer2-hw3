require(shiny)
require(ggplot2)
require(plotly)
require(plyr)
require(dplyr)
require(reshape2)
require(GGally)

facebook <- read.csv("dataset_Facebook.csv", sep = ';')
#facebook$Post.Weekday <- as.factor(facebook$Post.Weekday)
facebook$Post.Month <- as.numeric(facebook$Post.Month)

facebook$day <- ifelse(facebook$Post.Weekday == 1, 'Mon', facebook$Post.Weekday)
facebook$day <- ifelse(facebook$Post.Weekday == 2, 'Tues', facebook$day)
facebook$day <- ifelse(facebook$Post.Weekday == 3, 'Wed', facebook$day)
facebook$day <- ifelse(facebook$Post.Weekday == 4, 'Thurs', facebook$day)
facebook$day <- ifelse(facebook$Post.Weekday == 5, 'Fri', facebook$day)
facebook$day <- ifelse(facebook$Post.Weekday == 6, 'Sat', facebook$day)
facebook$day <- ifelse(facebook$Post.Weekday == 7, 'Sun', facebook$day)
facebook$day <- as.factor(facebook$day)

for(i in seq(1,500)){
  ifelse(facebook$like[i] > 3000, facebook$like[i] <- NA, facebook$like[i])
}

for(i in seq(1,500)){
  ifelse(facebook$Lifetime.Post.Consumptions[i] > 7000, facebook$like[i] <- NA, facebook$like[i])
}


facebook$weekend <- ifelse(facebook$Post.Weekday == 1, 'Weekday', facebook$Post.Weekday)
facebook$weekend <- ifelse(facebook$Post.Weekday == 2, 'Weekday', facebook$weekend)
facebook$weekend <- ifelse(facebook$Post.Weekday == 3, 'Weekday', facebook$weekend)
facebook$weekend <- ifelse(facebook$Post.Weekday == 4, 'Weekday', facebook$weekend)
facebook$weekend <- ifelse(facebook$Post.Weekday == 5, 'Weekday', facebook$weekend)
facebook$weekend <- ifelse(facebook$Post.Weekday == 6, 'Weekend', facebook$weekend)
facebook$weekend <- ifelse(facebook$Post.Weekday == 7, 'Weekend', facebook$weekend)

facebook$month <- ifelse(facebook$Post.Month == 1, 'Jan', facebook$Post.Month)
facebook$month <- ifelse(facebook$Post.Month == 2, 'Feb', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 3, 'Mar', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 4, 'Apr', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 5, 'May', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 6, 'June', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 7, 'July', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 8, 'Aug', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 9, 'Sep', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 10, 'Oct', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 11, 'Nov', facebook$month)
facebook$month <- ifelse(facebook$Post.Month == 12, 'Dec', facebook$month)

facebook <- na.omit(facebook)

ui <- fluidPage(
  headerPanel("Facebook Data"),
  fluidRow(column(12,
                  navlistPanel("Graph Options",
                               tabPanel("Total Interactions by Month and Day", plotlyOutput("heat")),
                               tabPanel("Likes and Comments per Month", plotlyOutput("plot1")),
                               tabPanel("Total Consumers per Month", plotlyOutput("box")),
                               tabPanel("Interaction Totals by Post Type", plotlyOutput("parallel")),
                               tabPanel("Lifetime Engagement by Page Likers and Non-Likers", 
                                        plotOutput("pairs", click = "plot_click")),
                               selectizeInput("month", "Month:",
                                              choices = c("All",
                                                          "January",
                                                          "February",
                                                          "March",
                                                          "April",
                                                          "May",
                                                          "June",
                                                          "July",
                                                          "August",
                                                          "September",
                                                          "October",
                                                          "November",
                                                          "December"))),
                  verbatimTextOutput("info")
                  
  )
  )
)



server <- function(input, output) {
  moData <- reactive({
    switch(input$month,
            "All" = facebook,
            "January" = facebook[grep("Jan", facebook$month), ],
            "February" = facebook[grep("Feb", facebook$month), ],
            "March" = facebook[grep("Mar", facebook$month), ],
            "April" = facebook[grep("Apr", facebook$month), ],
            "May" = facebook[grep("May", facebook$month), ],
            "June" = facebook[grep("June", facebook$month), ],
            "July" = facebook[grep("July", facebook$month), ],
            "August" = facebook[grep("Aug", facebook$month), ],
            "September" = facebook[grep("Sep", facebook$month), ],
            "October" = facebook[grep("Oct", facebook$month), ],
            "November" = facebook[grep("Nov", facebook$month), ],
            "December" = facebook[grep("Dec", facebook$month), ])
  })
  
  reactData <- reactive({
    moData() %>% filter(c(day == input$day, Type == input$Type, Category == input$Category,
                        Post.Hour == input$Post.Hour, Lifetime.Post.Total.Reach == input$Lifetime.Post.Total.Reach,
                        Lifetime.Post.Total.Impressions == input$Lifetime.Post.Total.Impressions, comment == input$comment,
                        like == input$like, share == input$share, weekend == input$weekend, 
                        Lifetime.Post.Impressions.by.people.who.have.liked.your.Page == input$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page,
                        Lifetime.Post.reach.by.people.who.like.your.Page == input$Lifetime.Post.reach.by.people.who.like.your.Page,
                        Total.Interactions == input$Total.Interactions))
  })
  
  colors <- c("lightskyblue1", "mediumblue", 
              "royalblue2", "royalblue4", 
              "dodgerblue", 
              "deepskyblue", "deepskyblue4")
  colors.month <- c("lightskyblue1", "lightskyblue2", "mediumblue", 
              "royalblue2", "royalblue4", 
              "dodgerblue", "blue", "royalblue", "deepskyblue2",
              "deepskyblue", "deepskyblue4", "lightskyblue4")
  
  
  output$heat <- renderPlotly({
    data <- moData()
    t <- list(size = 10)
    h <- plot_ly(data, x= ~day, y = ~month, z = ~Total.Interactions, type = "heatmap",
                 colors = colorRamp(c("lightskyblue1", "royalblue4")), 
                 text = ~paste(#'Month:', month, 
                               'Day:', day, 
                               '<br>Hour:', Post.Hour,
                               '<br>Lifetime Total Impressions:', 
                               Lifetime.Post.Total.Impressions,
                               '<br>Total Interactions:', Total.Interactions)
                ) %>%
      layout(title = ' ', font = t) %>%
             layout(xaxis = list(title = '',
                          gridcolor = 'rgb(255, 255, 255)',
                          #range = c(10, 90),
                          #type = 'log',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             yaxis = list(title = '',
                          gridcolor = 'rgb(255, 255, 255)',
                          #range = c(0, 9),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
             legend = list(orientation = 'h'))
    h
    
  })
  
  
  output$plot1 <- renderPlotly({
    data <- moData()
    options(warn = -1) 
    p <- ggplot(data, aes(x = like, y = comment, color = day)) +
      geom_point(aes(color = day,
                     size = Type), alpha = 0.7) + 
      #geom_point(colour="grey20", aes(size = Type), shape = 21) +
      facet_wrap(~ month) + 
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(colour = 'grey'),
            legend.position = "none"
            ) + 
      xlab("Likes") + ylab("Comments") + 
      scale_x_continuous(breaks = seq(0,3000,1000)) + scale_y_continuous(breaks = seq(0,250,50)) +
      scale_fill_manual(name="Day of Week",
                        values=c(Mon = "lightskyblue1",Tues = "mediumblue", 
                                 Wed = "royalblue2", Thurs = "royalblue4", 
                                 Fri = "dodgerblue", 
                                 Sat = "deepskyblue", Sun = "deepskyblue4")) +
      scale_color_manual(name="Day of Week",
                         values=c(Mon = "lightskyblue1",Tues = "mediumblue", 
                                  Wednesday = "royalblue2", Thurs = "royalblue4", 
                                  Fri = "dodgerblue", 
                                  Sat = "deepskyblue", Sun = "deepskyblue4"))
    options(warn = -1)
    ggplotly(p) 
  })
  
  
  output$parallel <- renderPlotly({
    data <- moData()
    pc <- ggparcoord(data, columns = c(16:19), groupColumn = 2,
                     scale = "globalminmax", #centerObsID = 1, scaleSummary = "median",
                     missing = "exclude", showPoints = FALSE,
                     splineFactor = FALSE, alphaLines = 1, boxplot = F,
                     shadeBox = NULL, mapping = NULL) +
      scale_fill_manual(name="Type of Post",
                        values=c(Photo = "lightskyblue1", Status = "mediumblue", 
                                 Video = "royalblue4",
                                 Link = "deepskyblue2")) +
      scale_color_manual(name="Type of Post",
                         values=c(Photo = "lightskyblue1", Status = "mediumblue", 
                                  Video = "royalblue4",
                                  Link = "deepskyblue2")) +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(colour = 'grey')) +
      xlab("Interactions") + ylab("Scaled Values")
    
    ggplotly(pc, textfont = list(color = '#000000', size = 12))
  })
  
  
  output$box <- renderPlotly({
    data <- moData()
    t <- list(size = 10)
    p <- plot_ly(data, x = ~month, y = ~Lifetime.Post.Consumers, 
                 color = ~ day, colors = colors, type = "box") %>%
      layout(title = ' ', font = t) %>%
             layout(xaxis = list(title = 'Month'),
             yaxis = list(title = 'Total Post Consumers'))
                          
    p
  })

  
  output$pairs <- renderPlot({
    data <- moData()
    
    plot <- function(data, mapping){
      ggplot2::ggplot(data = data, mapping = mapping) + 
        geom_point() +
        scale_fill_manual(name="",
                          values=c(Weekday = "royalblue",Weekend = "mediumblue")) +
        scale_color_manual(name="",
                           values=c(Weekday = "royalblue",Weekend = "mediumblue"))
    }
    
    plot2 <- function(data, mapping){
      ggplot2::ggplot(data = data, mapping = mapping) + 
        geom_freqpoly(binwidth = 10000) + 
        scale_fill_manual(name="",
                          values=c(Weekday = "royalblue",Weekend = "mediumblue")) +
        scale_color_manual(name="",
                           values=c(Weekday = "royalblue",Weekend = "mediumblue")) 
    }
  
    
    pm <- ggpairs(data, columns = c(8, 9, 13, 14), 
                  mapping = aes(color = weekend),
                  columnLabels = c("Total Reach", "Total Impressions", "Reach (Likes)", "Impressions (Likes)"),
                  lower = list(continuous = plot), 
                  diag = list(continuous = plot2),          
                  upper = list(continuous = plot)) +
      theme_light() + theme(axis.text = element_text(size = 1))
    pm
    
  })
  
  #output$info <- renderText({
  #  #paste0("X = ", input$plot_click$x, "\nY = ", input$plot_click$y)
  #  paste("Summary: ", input$plot_click$summary)
  #})
  
  
}

shinyApp(ui = ui, server = server)
