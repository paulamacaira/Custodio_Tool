if(!require(dplyr)) {install.packages("dplyr"); library(dplyr) }
if(!require(lubridate)) {install.packages("lubridate"); library(lubridate) }
if(!require(tibble)) {install.packages("tibble"); library(tibble) }
if(!require(feather)) {install.packages("feather"); library(feather) }
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse) }
if(!require(Metrics)) {install.packages("Metrics"); library(Metrics) }
if(!require(reshape2)) {install.packages("reshape2"); library(reshape2) }
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2) }
if(!require(BBmisc)) {install.packages("BBmisc"); library(BBmisc) }
if(!require(readxl)) {install.packages("readxl"); library(readxl) }
if(!require(hash)) {install.packages("hash"); library(hash) }
if(!require(gtools)) {install.packages("gtools"); library(gtools) }
if(!require(plotly)) {install.packages("plotly"); library(plotly) } 
if(!require(raster)) {install.packages("raster"); library(raster) }
if(!require(rgdal)) {install.packages("rgdal"); library(rgdal) }
if(!require(ncdf4)) {install.packages("ncdf4"); library(ncdf4) }  #Problema
if(!require(httr)) {install.packages("httr"); library(httr) }
if(!require(parallel)) {install.packages("parallel"); library(parallel) }
if(!require(forecast)) {install.packages("forecast"); library(forecast) }
if(!require(tseries)) {install.packages("tseries"); library(tseries) }
if(!require(fitdistrplus)) {install.packages("fitdistrplus"); library(fitdistrplus) }
if(!require(zoo)) {install.packages("zoo"); library(zoo) }
if(!require(cowplot)) {install.packages("cowplot"); library(cowplot) }
if(!require(numbers)) {install.packages("numbers"); library(numbers) }
if(!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx) }
if(!require(tsibble)) {install.packages("tsibble"); library(tsibble) }
if(!require(fpp3)) {install.packages("fpp3"); library(fpp3) }

library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(shinyWidgets)
library(tibbletime)

VizinhoProximo<-function(long_p, lat_p, lonlat){
  
  Melhor_lat = 0
  Melhor_lon = 0
  Menor_d = Inf
  
  for (i in 1:length(lonlat$long)) {
    a = lonlat$lat[i]
    b = lonlat$long[i]
    #Artigo [4]:
    rad <- pi/180
    d <- 6378.388*acos(sin(rad*lat_p) * sin(rad*a) + cos(rad*lat_p) * cos(rad*a) * cos(rad*b-rad*long_p))
    if(d <= Menor_d ){
      Melhor_lat = a
      Melhor_lon = b
      Menor_d = d
    }
  }
  
  return(c(Melhor_lon,Melhor_lat))
}

PontosMapa<-function(apertar,long_parque, lat_parque, lonlat){
  if(apertar == 0){
    p <- cbind(long_parque, lat_parque)
  }else{
    saida <- VizinhoProximo(long_parque, lat_parque, lonlat)
    p <- rbind(c(long_parque, lat_parque), saida)
  }
  return(p)
}

GerarSerieVel<-function(Melhor_latitute, Melhor_longitude, EscalaTempo, altura, data_inicio, data_fim, caminho_dadosMerra2){
  Endereco = paste0(caminho_dadosMerra2,Melhor_longitude,"_",Melhor_latitute,".feather")
  base_hora <- read_feather(Endereco)
  Endereco2 = paste0(caminho_dadosMerra2,"MerraDate.feather")
  MarcosTempo <- read_feather(Endereco2)
  base = cbind(MarcosTempo,base_hora)
  
  base$veloc50 <- sqrt(((base$U50M)^2)+((base$V50M)^2))
  base$veloc10 <- sqrt(((base$U10M)^2)+((base$V10M)^2))
  
  #alpha friction coefficient
  alpha <- (log(base$veloc50)-log(base$veloc10))/(log(50)-log(10+base$DISPH))
  #wind speed with power law
  base$velocEXT <- base$veloc50*((altura/50)^alpha)
  
  inicio_Vel = as.POSIXct(  paste0(data_inicio,"00:00:00") ,tz="UTC")
  fim_vel    = as.POSIXct(  paste0(data_fim,"23:00:00") ,tz="UTC")
  #dado = base[which(base$MerraDate == inicio_Vel):which(base$MerraDate == fim_vel),c(1,9)]
  dado = base[which(base$MerraDate == inicio_Vel):which(base$MerraDate == fim_vel),]
  
  SerieTeste <- as_tsibble(base[which(base$MerraDate == inicio_Vel):which(base$MerraDate == fim_vel),c(1,9)])
  SerieTeste <- as_tbl_time(SerieTeste,index = MerraDate)
  
  SaidaDados = list(Dados=dado, Name_X=EscalaTempo, Serie=NA)

  if(EscalaTempo == "Hora"){
    SerieTeste <- SerieTeste %>%
      mutate(mean_velocEXT = velocEXT)
    SaidaDados$Serie = SerieTeste
  }

  if(EscalaTempo == "Dia"){
    # b = dado %>%
    #   group_by(dia, ano, mes) %>%
    #   summarise(
    #     vel_Dia = mean(velocEXT)
    #   )
    SerieTesteDia <- SerieTeste %>%
      collapse_by("day") %>%
      group_by(MerraDate) %>%
      summarise(mean_velocEXT = mean(velocEXT))
    
    SaidaDados$Serie = SerieTesteDia
  }

  if(EscalaTempo == "Mes"){
    SerieTesteMes <- SerieTeste %>%
      collapse_by("month") %>%
      group_by(MerraDate) %>%
      summarise(mean_velocEXT = mean(velocEXT))
    
    SaidaDados$Serie = SerieTesteMes
  }

  if(EscalaTempo == "Ano"){
    SerieTesteAno <- SerieTeste %>%
      collapse_by("year") %>%
      group_by(MerraDate) %>%
      summarise(mean_velocEXT = mean(velocEXT))
    
    SaidaDados$Serie = SerieTesteAno
  }
  
  return(SaidaDados)
  #return(dado)
}

LonLat <- read_feather("lonlat.feather")
lonlat <- as.data.frame(LonLat)

caminho_dadosMerra2="C:/Users/usuario/Documents/Versao2App/Download_MERRA/"

#Melhor_latitute =-6
#Melhor_longitude = -33.75
#altura=137
#data_inicio = "2017-02-01"
#data_fim    = "2022-02-28"

# Exemplo do serie - Inicio
#data <- data.frame(day = as.Date("2017-06-14") - 0:364,
#                   value = runif(365) + seq(-140, 224)^2 / 10000)
# Exemplo do serie - Fim

#Inicio fluidPage
ui <- fluidPage(
  
  #Inicio tabsetPanel
  tabsetPanel(
    #Inicio tabPanel(Primeira Aba)
    tabPanel("Velocidade do Vento",
             
             # Titulo da Primeira Aba do Aplicativo
             titlePanel("Serie Temporal de Velocidade do Vento com base no MERRA-2"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 "Localizacao Geografica do seu Parque Eolico/Turbina:",
                 
                 numericInput("latitudeParque", "Latitude:", -15.25, min = -34.25, max = 7.65, step = 0.01),
                 #[De 7,65 (Norte) a -34,25 (Sul)]
                 
                 numericInput("longitudeParque", "Longitude:", -55.25, min = -75.95, max = -33.40, step = 0.01),
                 #[De -75,95 (Oeste) a -33,40 (Leste)]
                 
                 actionButton("interpolacao", "Interpolar")
               ),
               
               mainPanel(
                 leafletOutput("mymap")
               )
             ),
             
             
             dateRangeInput("TamanhoSerie", 
                            "Periodo - Inicio/Fim:", start = "2020-01-01", end = "2021-12-31",
                            min    = "2017-01-01", max    = "2022-02-28", format = "dd-mm-yyyy",language = "pt-BR"), 
             
             # Inputs limitados
             selectInput("tempo", 
                         "Escala Temporal:",
                         c("Hora","Dia","Mes","Ano")),
             
             numericInput("Altura", "Altura do Rotor da Turbina Eolica:", 100, min = 0, max = 1500, step = 1),
             
             checkboxInput("UsarCorrecao", "Fazer Correcao de Vies com dados do INMET"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 radioButtons("TipoCorrecao", 
                              "Tipo de Corracao por Media com dados do INMET:",
                              c("Periodo Unico", "Mensal", "Horario", "Mesal e Horario"), selected ="Horario" ),
                 
                 
               ),
               
               mainPanel(
                 #graficoserie
                 shinycssloaders::withSpinner(
                   plotly::plotlyOutput("serieVeloc"),
                   type = 4,
                   color = "orange",
                   size = 2
                 )
                 
                 
               )
             ),
             
             
             fluidRow(
               column(6, 
                      actionButton("contruir", "Gerar Serie!") 
               ),
               column(6,
                      actionButton("baixar", "Baixar Serie!")
               )
             ),
             
             br(),
             fluidRow(
               column(12, 
                      "Base usada na criacao da Serie de Velocidade do Vento:",
                      br(),
                      tableOutput("TabelaBase") 
               )
             ),
             
             
             
    ),
    #Fim tabPanel(Primeira Aba)
    
    #Inicio tabPanel(Segunda Aba)
    tabPanel("Geracao Eolica",
             
             # Titulo da Segunda Aba do Aplicativo
             titlePanel("Serie Temporal de Geracao Eolica"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 fileInput("file", "Dados Historicos de Potencia Eolica:", buttonLabel = "Upload..."),
                 dateRangeInput("TamanhoSeriePotencia", 
                                "Historico - Inicio/Fim:", start = "2017-01-01", end = "2018-12-31",
                                min    = "1980-01-01", max    = "2022-12-31") #, format = "dd-mm-yyyy",language = "pt-BR"
                 
               ),
               
               mainPanel(
                 #Carregar Imagem
                 "O Arquivo a se fazer o Upload deve ter formato de Planilha do Microsoft Excel (.xlsx) e confeccionado conforme exemplo abaixo.",
                 img(src="TebelaDadosPotencia.png")
               )
             ),
             
             radioButtons("DisponivelVelocidade", "Apresenta Dados Historicos de Velocidade do Vento:", c("Sim", "Não")),
             
             sidebarLayout(
               sidebarPanel(
                 
                 fileInput("file2", "Dados Historicos de Velocidade do Vento:", buttonLabel = "Upload..."),
                 dateRangeInput("TamanhoSerieVelocidade", 
                                "Historico - Inicio/Fim:", start = "2001-01-01", end = "2010-12-31",
                                min    = "1980-01-01", max    = "2022-12-31"),
                 numericInput("AlturaMedicao", "Altura de Medicao da Velocidade do Vento:", 100, min = 0, max = 1500, step = 1)
                 
               ),
               
               mainPanel(
                 #Carregar Imagem
                 "O Arquivo a se fazer o Upload deve ter formato de Planilha do Microsoft Excel (.xlsx) e confeccionado conforme exemplo abaixo.",
                 img(src="TebelaDadosVelocidade.png")
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 
                 "Localizacao Geografica do seu Parque Eolico/Turbina:",
                 
                 numericInput("latitudeParque2", "Latitude:", -15.25, min = -34.25, max = 7.65, step = 0.01),
                 #[De 7,65 (Norte) a -34,25 (Sul)]
                 
                 numericInput("longitudeParque2", "Longitude:", -55.25, min = -75.95, max = -33.40, step = 0.01),
                 #[De -75,95 (Oeste) a -33,40 (Leste)]
                 
                 checkboxInput("UsarCorrecao2", "Fazer Correcao de Vies com dados do INMET"),
                 
                 radioButtons("TipoCorrecao2", 
                              "Tipo de Corracao por Media com dados do INMET:",
                              c("Periodo Unico", "Mensal", "Horario", "Mesal e Horario"), selected ="Horario" )
                     
               ),
               
               mainPanel(
                 #Desenhar mapa
                 img(src="mapa.png")
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 
                 numericInput("Altura2", "Altura do Rotor da Turbina Eolica:", 100, min = 0, max = 1500, step = 1),
                 
                 
                 radioButtons("TipoMetodologia", 
                              "Metodologia a Adotar:",
                              c("Periodo Unico", "Mensal", "Horario", "Mesal e Horario"), selected ="Mesal e Horario" ),
                 
                 actionButton("AplicarMetodo", "Aplicar Metodo!")
               ),
               
               mainPanel(
                 #Carregar Imagem
                 img(src="AplicacaoMetodo.png", height = 400, width = 800)
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 
                 "Serie Temporal de Potencia:",
                 dateRangeInput("TamanhoSerieCriar", 
                                "Periodo - Inicio/Fim:", start = "2001-01-01", end = "2010-12-31",
                                min    = "1980-01-01", max    = "2022-12-31"), #, format = "dd-mm-yyyy",language = "pt-BR"
                 
                 numericInput("NumCenarios", "Quantidade de Cenarios:", 100, min = 0, max = 1500, step = 1),
                 
                 actionButton("CriarSerie", "Gerar!")
               ),
               
               mainPanel(
                 #Carregar Imagem
                 img(src="CenarioEseriemedia.png", height = 400, width = 800)
               )
             ),
             
             checkboxGroupInput("SaidasCenarioSerie", 
                     "Selecione os dados que deseja baixar:",
                     c("Cenarios", "Serie Media"), selected = "Serie Media"),
             
             actionButton("BaixarSerieCenario", "Baixar!")
             
    )
    #Fim tabPanel(Segunda Aba)
  )
  #Fim tabsetPanel
  
)
#Fim fluidPage

server <- function(input, output, session){
  
  Pontos <- eventReactive(input$interpolacao, {
    PontosMapa(1,input$longitudeParque,input$latitudeParque,lonlat)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-55.25, -15.25, zoom = 4) %>%
      #setView(input$longitudeParque, input$latitudeParque, zoom = 4) %>%
      #addMarkers(input$longitudeParque, input$latitudeParque) %>%
      addMarkers(data = Pontos())
    
  })
  
  # observeEvent(input$contruir, {
  #   
  #   output$serieVeloc <- plotly::renderPlotly({
  #     p <- ggplot(data, aes(x=day, y=value)) +
  #       geom_line() + 
  #       xlab("")+
  #       theme_minimal()
  #     
  #     plotly::ggplotly(p)
  #   })
  #   
  # })
  
  observeEvent(input$contruir, {
    Melhor_latitute=as.double(Pontos()[2,2])
    Melhor_longitude=as.double(Pontos()[2,1])
    serie_V<-GerarSerieVel(Melhor_latitute, Melhor_longitude, input$tempo, input$Altura, input$TamanhoSerie[1], input$TamanhoSerie[2], caminho_dadosMerra2)
    
    output$serieVeloc <- plotly::renderPlotly({
      p <- ggplot(serie_V$Serie, aes(x=MerraDate, y=mean_velocEXT)) +
        geom_line() + 
        xlab(serie_V$Name_X)+
        ylab("Velocidade do Vento (m/s)")+
        theme_minimal()
      
      plotly::ggplotly(p)
    })
    
    output$TabelaBase <- renderTable({
      serie_V$Dados
    })
    
  })
  
  observeEvent(input$baixar, {
    Melhor_latitute=as.double(Pontos()[2,2])
    Melhor_longitude=as.double(Pontos()[2,1])
    serie_V<-GerarSerieVel(Melhor_latitute, Melhor_longitude, input$tempo, input$Altura, input$TamanhoSerie[1], input$TamanhoSerie[2], caminho_dadosMerra2)
    
    nome_arquivo = paste0("SerieVelocidadeVento",input$tempo,"_", input$Altura,"M_Long",input$longitudeParque,"_Lat",input$latitudeParque,".csv")
    
    write.csv2(serie_V$Serie,nome_arquivo)
  })
  
  
}

shinyApp(ui, server)
