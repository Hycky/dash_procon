#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# https://shiny.posit.co/

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)

setAccountInfo(name = 'hhenriquecorp' , token = 'E91B94E1A4ACE9F63CB62DEDB8711D7E', secret = 'NTzoniFmxCtPX7FtCOQtDSPLGo1H0M8R4FXho96+')

dados <- fread('dados_limpos.csv')
media_chamados_ano <- dados %>%
                        group_by(anocalendario) %>%
                        summarise(qtd_chamados = n() ) %>%
                        summarise(medias_chamados_ano = mean(qtd_chamados))

cabecalho <- dashboardHeader(title = 'Dashboard PROCONs')

barra_lateral <- dashboardSidebar(width = '250px' ,
                                  sidebarMenu(
                                    menuItem('Dashboard',
                                             tabName = 'dashboard',
                                             icon = icon('dashboard')),
                                    menuItem('Informações',
                                             tabName = 'Infos',
                                             icon = icon('info-circle'))
                                  ))
painel_principal <- dashboardBody(
  tags$head(tags$style(HTML(".info-box, .info-box-icon, .small-box{height: 100px}"))),
  tags$style(".info-box-number{font-size: 38px; color: #001f3f}"),
  tabItems(
    tabItem(tabName = 'Infos',
            h1("Informações"),
            infoBox(title = 'Contato', icon = icon('envelope-square'),
                    subtitle = 'Para mais informações e/ou feedback
                    entre em contato: hhenrique.corp@gmail.com')),
    tabItem(tabName = 'dashboard',
            fluidRow(
              valueBox(subtitle = 'Registros', value = nrow(dados),color = 'navy', 
                       icon = tags$i(class = "fa fa-database", style = "color: white;")),
              infoBox(title = "", subtitle = "Reclamações Por Ano", value = media_chamados_ano,color = 'navy',
                      icon = icon("list")),
              valueBoxOutput(outputId = "qtdUf")
            ),
            fluidRow(
              column(width = 12, 
                     box(title = 'FILTROS, desmarque a opção TODOS para selecionar a UF desejada', width = '100%',
                         column(width = 12,
                                box(width = '100%',
                                    awesomeCheckboxGroup(inline = TRUE,
                                                         inputId = 'select_UF',
                                                         label = 'Estados:',
                                                         choices = c('TODOS', unique(dados$UF)),
                                                         selected = 'TODOS')
                                )
                         ),
                         column(width = 6,
                                box(width = '100%',
                                    dateRangeInput(inputId = 'data_abertura',
                                                   label = 'Data Abertura',
                                                   format = 'dd-mm-yyyy',
                                                   start = min(as.Date(dados$DataAbertura)),
                                                   end = max(as.Date(dados$DataAbertura))
                                    )
                                )
                         ),
                         column(width = 6,
                                box(width = '100%',
                                    selectizeInput(inputId = 'assunto',
                                                   label = 'Descrição Assunto: ',
                                                   choices = c('TODOS', unique(dados$DescricaoAssunto)),
                                                   multiple = T,
                                                   options = list(maxItens = 5),
                                                   selected = 'TODOS')
                                )
                         )
                     )##Final box principal
              )
            ),##final linha
            fluidRow(
              column(width = 12,
                     box(width = '100%',
                         plotlyOutput(outputId = "data", width = '100%'),
                         verbatimTextOutput(outputId = 'descData')
                     )
              )
              
            ),##final linha
            fluidRow(
              column(width = 6,
                     box(width = '100%',
                         plotlyOutput(outputId = "atendida")
                     )
                     
              ),
              column(width = 6,
                     box(width = '100%',
                         plotlyOutput(outputId = "atendidaAno")
                     )
              )
            ),##final linha
            fluidRow(
              column(width = 12,
                     box(width = '100%',
                         plotlyOutput(outputId = "uf")
                        )
                     )
                    )
            )
  )## fim tab itens
)
# Define UI for application that draws a histogram
ui <- dashboardPage(header = cabecalho,
                    sidebar = barra_lateral,
                    body = painel_principal
                    )

# Define server logic required to draw a histogram
server <- function(input, output) {

  dados_selecionados <- reactive({
    if(!'TODOS' %in% input$select_UF){
      dados <- dados %>%
        filter(UF %in% input$select_UF) 
    }
    if(! 'TODOS' %in% input$assunto){
      dados <- dados %>%
        filter(DescricaoAssunto %in% input$assunto)
    }
    dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] &
                                as.Date(DataAbertura) <= input$data_abertura[2])
    dados
  })
    output$listaUF <- renderPrint({
      unique(dados_selecionados()$UF)
    })
    output$data <- renderPlotly({
      ggplotly(
        data.frame(table(as.Date(dados_selecionados()$DataArquivamento))) %>%
        rename(Data = Var1, Qtd = Freq) %>%
        ggplot(aes(as.Date(Data), Qtd)) +
        geom_line(group = 1, color = 'navy') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
        ) +
        scale_x_date(date_labels = '%b-%y', breaks = '6 month') +
        ggtitle('Quantidade de Reclamações por Ano-Mês') +
        xlab("") + ylab("")
      ) %>%
        layout(hoverlabel = list(bgcolor = "navy", font = list(size = 12)))
    })
    output$uf <- renderPlotly({
      ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>%
        rename(UF = Var1, Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF,Qtd), y = Qtd, 
                   text = paste('UF:', UF, "<br>", "QTD:", Qtd))) +
        geom_bar(fill = 'navy', stat = 'identity') +
        coord_flip() +
        xlab('UF') + 
        theme_bw() +
        xlab("") + ylab("") +
        theme(panel.grid = element_blank()) +
        ggtitle('Quantidade de Reclamações por UF'),
        tooltip = 'text'
      )
    })
    output$atendida <- renderPlotly({
      ggplotly(
        ggplot(dados_selecionados()) +
          geom_bar(aes(Atendida), fill = c('lightblue','navy'), 
                   stat = 'count') +
          theme_bw() +
          ggtitle('Quantidade de Chamados Atendidos') + ylab("")
      )
    })
    output$atendidaAno <- renderPlotly({
      ggplotly(
        data.frame(table(dados_selecionados()$anocalendario, dados_selecionados()$Atendida)) %>%
          rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
          ggplot() +
          geom_bar(aes(x = Ano, y = Qtd, fill = Atendida), stat = 'identity', 
                   position = position_dodge2()) +
          scale_fill_manual(values = c("lightblue", "navy")) +
          theme_bw() +
          ggtitle('Quantidade de Reclamações Atendidas(não) por Ano') + ylab("") + xlab("")
      )
    })
    output$descData <- renderText({
      paste("Gráfico com a quantidade de reclamações feitas entre: ",
            format(min(dados_selecionados()$DataAbertura), "%d/%m/%Y"), '-',
            format(max(dados_selecionados()$DataAbertura), "%d/%m/%Y")
      )
    })
    output$qtdUf <- renderValueBox({
      valueBox(value = length(unique(dados_selecionados()$UF)),
               subtitle = "Ufs", icon = tags$i(class = "fa fa-map-marker", style = "color: white;"),color = 'navy')
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
