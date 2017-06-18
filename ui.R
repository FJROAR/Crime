library(leaflet)

shinyUI(fluidPage(
    
    img(src = "TD.jpg"),   img(src = "Big.jpg"),
    titlePanel ("Crime Prediction by City"),
    mainPanel(
      
      h4("How to use the map:"),
      br(),
      p("-The map represent Violent Crimes in NY City by crime category. Only Rape crime is excluded"),
      br(),
      p("-When in the Type of Crime menu, a crime category is chosen, the last 3 months crimes are shown"),
      p("-A pop-pup information is associated to each crime indicating day and hour. This pop-pup is shown clicking over the point"),
      br(),
      h4("How to calculate the Probability of Crime:"),
      br(),
      p("-Hours go from 0h to 23h"),
      p("-Week-Days go from 1 = Monday to 7 = Sunday"),
      p("-Probabilities are shared among week-days and hour by day in a typical week according to one-year past data"),
      p("-Users make selections by week-day and Hour"),
      br(),
      h4("How to compute the expected number of crimes:"), img(src = "AR.jpg"),
      br(),
      p("-If Predicted Week Crimes is deleted, a monthly arima model can be applied"),
      p("-A default Arima model configuration is done, but this one can be changed"),
      p("-The Arima model has a 3-month horizont and some fitting statistics are shown by each Arima model configuration")
      
      ),   
    
  p(),
  
  leafletOutput("mymap"),
  
  h3("Choose which Crime Category want to be shown: "),
  p(),
  fluidRow(
    
    column(3,
           radioButtons("radio", label = h4("Type of Crime"),
                 choices = list("Murder"= "purple", #"Rape"= 3,
                                "Vehicle Theft"= "black",
                                "Assault"= "orange",
                                "Larceny Theft"= "grey",
                                "Robbery"= "cyan",
                                "Burglary"= "brown"),selected = "purple")),
    p(),
    
    mainPanel(
      
      h4("Crime Prediciton:")
    ),
    
    fluidRow(
      #Number
      column(4, 
             numericInput("n1", 
                          label = h6("Number of Weekly Crimes: "), 
                          value = 6500)),
      
      # Hour
      column(2, 
             numericInput("h1", 
                          label = h6("Hour of Day: "), 
                          value = 0)),
      
      # Day
      column(2,
             numericInput("d1", 
                          label = h6("Week-Days: "), 
                          value = 1)),
      column(1,
             numericInput("ar", 
                          label = h6("AR Parameter: "), 
                          value = 1)), 
      column(1,
             numericInput("ma", 
                          label = h6("MA Parameter: "), 
                          value = 0)), 
      column(1,
             numericInput("sar", 
                          label = h6("SAR Parameter: "), 
                          value = 1)),     
      column(1,
             numericInput("sma", 
                          label = h6("SAM Parameter: "), 
                          value = 0)),     
      column(8,
             sidebarPanel(
               selectInput("Seg", "Apply Category in computations:", 
                           choices=c("No", "Yes")))),
      column(3,
             radioButtons("radio2", label = h4("Crime aggregation by:"),
                          choices = list("None"= 1, 
                                         "Day"= 2,
                                         "Hour" = 3
                                         ),selected = 1))
      
    ),
    
    
    fluidRow(
      
      column(8, h2(textOutput("text1"))),
      column(8, h2(textOutput("text2"))),
      column(8, h4(textOutput("text3"))),
      column(8, h4(textOutput("text4")))
      
    )
  
  
)))


