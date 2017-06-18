shinyServer(function(input, output, session) {
  

  RUTA = "DATA/"
  #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
  
  Datos <- read.csv(paste0(RUTA, "DatosMap.csv"), sep = ",")
  
  points <- reactive({
    
    
      Datos[which(Datos$Color == input$radio),]
      

  })
    output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 40.6643, lng = -73.9385, zoom = 12) %>%
      addProviderTiles("Stamen.Terrain",
                       options = providerTileOptions(noWrap = TRUE)
      )
      
  })
    
  observe({
    
    
    leafletProxy("mymap", data = points()) %>% 
      clearShapes() %>%
      clearMarkers()%>%
      addCircleMarkers( #color = input$radio, 
                        color = ~paste(Color),
                        popup = ~paste(Label), radius = 2)
  })
  
  output$text1 <- renderText({
    
    Hour = input$h1
    Day = input$d1
    Crimes = input$n1
    
    if(is.nan(Hour) == 1 || is.infinite(Hour) == 1 || is.na(Hour) == 1) {Hour = 0}
    if(Hour > 23) {Hour = 23}
    if(Hour < 0) {Hour = 0}
    if(is.nan(Day) == 1 || is.infinite(Day) == 1 || is.na(Day) == 1) {Day = 1}
    if(Day > 7) {Day = 7}
    if(Day < 1) {Day = 1}
    
    Cat = input$radio
    
    Agg = input$radio2
    
    
    if (input$Seg == 'No'){
      
      cathourFunction <- function (Hour, Day, Cat, Agg){
        RUTA ="DATA/"
        #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
        
        ProbMatrix <- read.csv(paste0(RUTA,"DayHour_Matrix.csv"), sep = ",")
        CrimeProb <- ProbMatrix[which(ProbMatrix$Hora == Hour), Day + 1]
        
        if (Agg == 2)
        {
          CrimeProb <- colSums(ProbMatrix)[c(2:8)]
          CrimeProb <- CrimeProb[Day]
          
        }
        
        if (Agg == 3)
        {
          CrimeProb <- rowSums(ProbMatrix[,c(2:8)])
          CrimeProb <- CrimeProb[(Hour + 1)]
          
        }
        
        
        return(CrimeProb)  
    
      }
    }
    
    if (input$Seg == 'Yes'){
      
      cathourFunction <- function (Hour, Day, Cat, Agg){
        
        RUTA ="DATA/"
        #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
        
        ProbMatrix <- read.csv(paste0(RUTA,"DayHour_Matrix_", Cat,".csv"), sep = ",")
        CrimeProb <- ProbMatrix[which(ProbMatrix$Hora == Hour), Day + 1]

        if (Agg == 2)
        {
          CrimeProb <- colSums(ProbMatrix)[c(2:8)]
          CrimeProb <- CrimeProb[Day]
          
        }
        
        if (Agg == 3)
        {
          CrimeProb <- rowSums(ProbMatrix[,c(2:8)])
          CrimeProb <- CrimeProb[(Hour + 1)]
          
        }
        

        return(CrimeProb)  
        
      }
      
      
    }
    
    paste("Crime Probability = ", paste0(round((100 * cathourFunction (Hour, Day, Cat, Agg)),4),"%"))
    
    
  })
  
  
  output$text2 <- renderText({
    
    Hour = input$h1
    Day = input$d1
    Crimes = input$n1
    Cat = input$radio
    AR = input$ar
    MA = input$ma
    SAR = input$sar
    SMA = input$sma
    
    if(is.nan(Hour) == 1 || is.infinite(Hour) == 1 || is.na(Hour) == 1) {Hour = 0}
    if(Hour > 23) {Hour = 23}
    if(Hour < 0) {Hour = 0}
    if(is.nan(Day) == 1 || is.infinite(Day) == 1 || is.na(Day) == 1) {Day = 1}
    if(Day > 7) {Day = 7}
    if(Day < 1) {Day = 1}
    if(is.nan(AR) == 1 || is.infinite(AR) == 1 || is.na(AR) == 1) {AR = 7}
    if(is.nan(MA) == 1 || is.infinite(MA) == 1 || is.na(MA) == 1) {MA = 7}
    if(is.nan(SAR) == 1 || is.infinite(SAR) == 1 || is.na(SAR) == 1) {SAR = 7}
    if(is.nan(SMA) == 1 || is.infinite(SMA) == 1 || is.na(SMA) == 1) {SMA = 7}
      
    
    if(is.nan(Crimes) == 1 || is.infinite(Crimes) == 1 || is.na(Crimes) == 1) {Crimes = 6500}
    
    Agg = input$radio2
    
      RUTA ="DATA/"
      #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
      
      Time_Series <- read.csv(paste0(RUTA,"SERIE_CRIMEN.csv"), sep = ",")
      
      
      Modelo_con_Backstesting <- function (SERIE_CRIMEN, p, d, q, P, D, Q, periodo, mesback)
      {
        
        #mesback = 6
        #p = 1 
        #d = 1
        #q = 0
        #P = 1
        #D = 0
        #Q = 1
        
        SERIE_back <- SERIE_CRIMEN [1:(nrow(SERIE_CRIMEN) - mesback + 1), ]
        
        M_back <- arima(SERIE_back$Crimes, order = c(p, d, q),
                        seasonal = list(order = c(P, D, Q), period = periodo),
                        include.mean = FALSE)
        
        Back <- as.vector(predict(M_back, n.ahead = mesback)$pred)
        Real <- SERIE_CRIMEN[(nrow(SERIE_CRIMEN) - mesback + 1) :(nrow(SERIE_CRIMEN)),3]
        
        Fiabilidad_Agregada = abs(sum(Back - Real)/sum(Real))
        Fiabilidad_EAMR = mean(abs(Back - t(Real))) / mean(t(Real))
        Fiabilidad_ECMR = (mean(abs(Back - t(Real))**2))**0.5 / mean(t(Real))
        
        Modelo <- arima(SERIE_CRIMEN$Crimes, order = c(p, d, q),
                        seasonal = list(order = c(P, D, Q), period = periodo),
                        include.mean = FALSE)
        
        Prediccion <- as.vector(predict(Modelo, n.ahead = mesback)$pred)
        
        Lista_final <- list(Prediccion, Fiabilidad_Agregada, Fiabilidad_EAMR, Fiabilidad_ECMR)
        
        return(Lista_final)
      }
      
      
    
    
      if (input$Seg == 'No'){
        
        cathourFunction <- function (Hour, Day, Cat, Agg){
          RUTA ="DATA/"
          #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
          
          ProbMatrix <- read.csv(paste0(RUTA,"DayHour_Matrix.csv"), sep = ",")
          CrimeProb <- ProbMatrix[which(ProbMatrix$Hora == Hour), Day + 1]
          
          if (Agg == 2)
          {
            CrimeProb <- colSums(ProbMatrix)[c(2:8)]
            CrimeProb <- CrimeProb[Day]
            
          }
          
          if (Agg == 3)
          {
            CrimeProb <- rowSums(ProbMatrix[,c(2:8)])
            CrimeProb <- CrimeProb[(Hour + 1)]
            
          }
          
          
          return(CrimeProb)  
          
        }
      }
      
      if (input$Seg == 'Yes'){
        
        cathourFunction <- function (Hour, Day, Cat, Agg){
          RUTA ="DATA/"
          #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
          
          ProbMatrix <- read.csv(paste0(RUTA,"DayHour_Matrix_", Cat,".csv"), sep = ",")
          
          CrimeProb <- ProbMatrix[which(ProbMatrix$Hora == Hour), Day + 1]
          
          if (Agg == 2)
          {
            CrimeProb <- colSums(ProbMatrix)[c(2:8)]
            CrimeProb <- CrimeProb[Day]
            
          }
          
          if (Agg == 3)
          {
            CrimeProb <- rowSums(ProbMatrix[,c(2:8)])
            CrimeProb <- CrimeProb[(Hour + 1)]
            
          }
          
          
          return(CrimeProb)  
          
        }
      }
      
      
    if(is.nan(Crimes) != TRUE)
    {
        
      Crimes = input$n1
        
    }
      
    if (is.na(Crimes) == TRUE)
    {
      
      Prediccion_Crimen <- Modelo_con_Backstesting(Time_Series, AR, 1, MA, SAR, 1, SMA, 12, 3)
      
      Crimes <- 7 * sum(Prediccion_Crimen[[1]]) / 90
      
    }
  
    paste("Expected Criminality = ", round(cathourFunction (Hour, Day, Cat, Agg) * Crimes,2))
    
  })
  
  output$text3 <- renderText({
    
    Hour = input$h1
    Day = input$d1
    Crimes = input$n1
    Cat = input$radio
    AR = input$ar
    MA = input$ma
    SAR = input$sar
    SMA = input$sma
    
    if(is.nan(Hour) == 1 || is.infinite(Hour) == 1 || is.na(Hour) == 1) {Hour = 0}
    if(Hour > 23) {Hour = 23}
    if(Hour < 0) {Hour = 0}
    if(is.nan(Day) == 1 || is.infinite(Day) == 1 || is.na(Day) == 1) {Day = 1}
    if(Day > 7) {Day = 7}
    if(Day < 1) {Day = 1}
    if(is.nan(AR) == 1 || is.infinite(AR) == 1 || is.na(AR) == 1) {AR = 7}
    if(is.nan(MA) == 1 || is.infinite(MA) == 1 || is.na(MA) == 1) {MA = 7}
    if(is.nan(SAR) == 1 || is.infinite(SAR) == 1 || is.na(SAR) == 1) {SAR = 7}
    if(is.nan(SMA) == 1 || is.infinite(SMA) == 1 || is.na(SMA) == 1) {SMA = 7}
    
    
    if(is.nan(Crimes) == 1 || is.infinite(Crimes) == 1 || is.na(Crimes) == 1) {Crimes = 6500}
    
   
    RUTA = "DATA/"
    #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
    
    Time_Series <- read.csv(paste0(RUTA,"SERIE_CRIMEN.csv"), sep = ",")
    
    
    Modelo_con_Backstesting <- function (SERIE_CRIMEN, p, d, q, P, D, Q, periodo, mesback)
    {
      
      #mesback = 6
      #p = 1 
      #d = 1
      #q = 0
      #P = 1
      #D = 0
      #Q = 1
      
      SERIE_back <- SERIE_CRIMEN [1:(nrow(SERIE_CRIMEN) - mesback + 1), ]
      
      M_back <- arima(SERIE_back$Crimes, order = c(p, d, q),
                      seasonal = list(order = c(P, D, Q), period = periodo),
                      include.mean = FALSE)
      
      Back <- as.vector(predict(M_back, n.ahead = mesback)$pred)
      Real <- SERIE_CRIMEN[(nrow(SERIE_CRIMEN) - mesback + 1) :(nrow(SERIE_CRIMEN)),3]
      
      Fiabilidad_Agregada = abs(sum(Back - Real)/sum(Real))
      Fiabilidad_EAMR = mean(abs(Back - t(Real))) / mean(t(Real))
      Fiabilidad_ECMR = (mean(abs(Back - t(Real))**2))**0.5 / mean(t(Real))
      
      Modelo <- arima(SERIE_CRIMEN$Crimes, order = c(p, d, q),
                      seasonal = list(order = c(P, D, Q), period = periodo),
                      include.mean = FALSE)
      
      Prediccion <- as.vector(predict(Modelo, n.ahead = mesback)$pred)
      
      Lista_final <- list(Prediccion, Fiabilidad_Agregada, Fiabilidad_EAMR, Fiabilidad_ECMR)
      
      return(Lista_final)
    }
        
    paste("EAM ARIMA Model = ", round(Modelo_con_Backstesting(Time_Series, AR, 1, MA, SAR, 1, SMA, 12, 3)[[3]],4))

    })
  output$text4 <- renderText({
    
    Hour = input$h1
    Day = input$d1
    Crimes = input$n1
    Cat = input$radio
    AR = input$ar
    MA = input$ma
    SAR = input$sar
    SMA = input$sma
    
    if(is.nan(Hour) == 1 || is.infinite(Hour) == 1 || is.na(Hour) == 1) {Hour = 0}
    if(Hour > 23) {Hour = 23}
    if(Hour < 0) {Hour = 0}
    if(is.nan(Day) == 1 || is.infinite(Day) == 1 || is.na(Day) == 1) {Day = 1}
    if(Day > 7) {Day = 7}
    if(Day < 1) {Day = 1}
    if(is.nan(AR) == 1 || is.infinite(AR) == 1 || is.na(AR) == 1) {AR = 7}
    if(is.nan(MA) == 1 || is.infinite(MA) == 1 || is.na(MA) == 1) {MA = 7}
    if(is.nan(SAR) == 1 || is.infinite(SAR) == 1 || is.na(SAR) == 1) {SAR = 7}
    if(is.nan(SMA) == 1 || is.infinite(SMA) == 1 || is.na(SMA) == 1) {SMA = 7}
    
    
    if(is.nan(Crimes) == 1 || is.infinite(Crimes) == 1 || is.na(Crimes) == 1) {Crimes = 6500}
    
    
    RUTA = "DATA/"
    #RUTA = "C:/FJRA/2017/PET_PROJECTS/CRIME_01/MAPAS4/DATA/"
    
    Time_Series <- read.csv(paste0(RUTA,"SERIE_CRIMEN.csv"), sep = ",")
    
    
    Modelo_con_Backstesting <- function (SERIE_CRIMEN, p, d, q, P, D, Q, periodo, mesback)
    {
      
      #mesback = 6
      #p = 1 
      #d = 1
      #q = 0
      #P = 1
      #D = 0
      #Q = 1
      
      SERIE_back <- SERIE_CRIMEN [1:(nrow(SERIE_CRIMEN) - mesback + 1), ]
      
      M_back <- arima(SERIE_back$Crimes, order = c(p, d, q),
                      seasonal = list(order = c(P, D, Q), period = periodo),
                      include.mean = FALSE)
      
      Back <- as.vector(predict(M_back, n.ahead = mesback)$pred)
      Real <- SERIE_CRIMEN[(nrow(SERIE_CRIMEN) - mesback + 1) :(nrow(SERIE_CRIMEN)),3]
      
      Fiabilidad_Agregada = abs(sum(Back - Real)/sum(Real))
      Fiabilidad_EAMR = mean(abs(Back - t(Real))) / mean(t(Real))
      Fiabilidad_ECMR = (mean(abs(Back - t(Real))**2))**0.5 / mean(t(Real))
      
      Modelo <- arima(SERIE_CRIMEN$Crimes, order = c(p, d, q),
                      seasonal = list(order = c(P, D, Q), period = periodo),
                      include.mean = FALSE)
      
      Prediccion <- as.vector(predict(Modelo, n.ahead = mesback)$pred)
      
      Lista_final <- list(Prediccion, Fiabilidad_Agregada, Fiabilidad_EAMR, Fiabilidad_ECMR)
      
      return(Lista_final)
    }
    
    paste("ECM ARIMA Model = ", round(Modelo_con_Backstesting(Time_Series, AR, 1, MA, SAR, 1, SMA, 12, 3)[[4]],4))
    
  })
})
  



