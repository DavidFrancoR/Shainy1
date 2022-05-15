################################################################################
#################################### AULA 1 ####################################
################################################################################

#################################### PACOTE ####################################
library(shiny)
library(shinydashboard)
library(corrgram)
library(ggplot2)
library(ggpubr)

################################################################################


# VISUALIZAÇÃO INTERATIVA - Projeto de Visualização de Dados: Vulnerabilidade Social em Idosos

library(readr)
Dados_Lab01 <- read.csv("C:/Users/SUPORTE/documents/dataset.csv", sep="")

ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(title = "Vulnerabilidade Social em Idosos",
                                    titleWidth = 700),
                    
                    dashboardSidebar(
                      width = 200,
                      sidebarMenu(
                        menuItem("Introdução", tabName = "item1", icon = icon("atom")),
#                        menuItem("Power", tabName = "subitem3", icon = icon("plug"))),
                        menuItem("Metodologia", tabName = "item2", icon = icon("bahai")),
                        menuItem("Resultados", tabName = "item3", icon = icon("info")),
                        menuItem("Mapa", tabName = "item4", icon = icon("info")),
                        menuItem("Referência", tabName = "item5", icon = icon("info"))
                      )),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: Georgia;
                      font-weight: bold;
                      font-size: 32px;
                      }
                    '))),
                      tabItems(
                        tabItem(tabName = "item1",
                                h2("Introdução"),
                                h4("Na perspectiva de explorar, visualizar e entender as das dimensionalidades da vulnerabilidade social para os municípios do Estado da Bahia, no contexto da população de idosos, considerando as variáveis do CENSO-2010 através de uma análise de cluster (uma análise de agrupamentos)."),
                                h4("As vulnerabilidades sociais vinculam-se a situações de empobrecimento da classe trabalhadora, relacionadas a dificuldades materiais para a manutenção da sobrevivência, mas, também, a dificuldades relacionais e culturais, pois estas interferem na forma de viver dos trabalhadores e de suas famílias (DE MORAES ALVES e SEMZEZEM, 2013)."),
                                h4("Embora a vulnerabilidade social seja fator importante para todas as fases da vida, na velhice há evidências crescentes que ligam circunstâncias sociais com a idade. Idosos frágeis em contexto de vulnerabilidade social trazem consigo demandas para as políticas públicas, podendo estar altamente relacionada à saúde e às necessidades de auxílios da assistência social (ANDREW e KEEF, 2014; ANDREW, 2015; JESUS et al., 2017)."),
                                h4("Segundo o CORECON BA – Conselho Regional de Economia da Bahia, a vulnerabilidade social no estado era considerada muito alta em 2000 e alta em 2010 (CORECON-BA, 2015).")),
                        tabItem(tabName = "item2",
                                h2("Metodologia"),
                                h4("As vulnerabilidades sociais são um fator determinante na sociedade, entendê-las pode ajudar na melhora de políticas públicas e na conscientização da população, neste caso o enfoque é a população vulnerável na terceira idade, particularmente no Estado da Bahia. Os dados podem ser analisados a um nível de município, já que os dados proporcionados são do CENSO-2010."),
                                h4("MUN Município do Estado da Bahia "),
                                h4("V1 	População total"),
                                h4("V2 	% da população que vive em domicílios com banheiro e água encanada "),
                                h4("V3 	IDHM - Índice de Desenvolvimento Humano Municipal "),
                                h4("V4 	IDHM Renda "),
                                h4("V5 	IDHM Longevidade "),
                                h4("V6 	IDHM Educação "),
                                h4("V7 	População masculina com 80 anos e mais de idade "),
                                h4("V8 	Taxa de envelhecimento "),
                                h4("V9 	População feminina com 80 anos e mais de idade "),
                                h4("V10 População em domicílios vulneráveis e com idoso "),
                                h4("Uma análise de agrupamentos pode ajudar a entender graficamente onde estão concentrados os municípios similares na classificação, de acordo com as variáveis do censo.")),
                        tabItem(tabName = "item5",
                                mainPanel(
                                  box(status = "warning", 
                                      width = 100,
                                      height = 500,
                                      solidHeader = T,
                                      title = "Referência",
                                      h4("Are there still sex differences in the functioning of the elderly."),
                                      h4("Idosos no contexto da pandemia da COVID-19 no Brasil: efeitos nas condições de saúde, renda e trabalho."),
                                      h4("Perfis de integração social entre idosos institucionalizados não frágeis no município de Natal, Rio Grande do Norte, Brasil."),
                                      h4("Factors associated with depressive symptoms in older adults in context of social vulnerability."),
                                      h4("Fragilidade e qualidade de vida de idosos em contexto de vulnerabilidade social."),
                                      h4("Fragilidade de idosos em vulnerabilidade social.")),

                                  )),
                        #  tabItems(
                        tabItem(tabName = "subitem3",
                                mainPanel(
                                  box(status = "warning", 
                                      width = 5,
                                      height = 300,
                                      solidHeader = T,
                                      title = "Menu de Informações",
                                      h1("Escreva aqui <h1>"),
                                      h2("Escreva aqui <h2>"),
                                      h3("Escreva aqui <h3>"),
                                      h4("Escreva aqui <h4>"),
                                      h4(strong("Escreva <strong>"), "aqui"))
                                )),
                        tabItem(tabName = "item3",
                                fluidRow(
                                  box(
                                    solidHeader = F, status = "warning",
                                    plotOutput("ex_a")
                                  ),
                                  box(
                                    solidHeader = F, status = "warning",
                                    plotOutput("ex_b")
                                  ),
                                  box(
                                    solidHeader = F, status = "warning",
                                    plotOutput("ex_c")
                                  ),
                                  box(
                                    solidHeader = F, status = "warning",
                                    plotOutput("ex_d")
                                  ),
                                  box(
                                    solidHeader = F, status = "warning",
                                    plotOutput("ex_e")
                                  )
                                ))
                        
                      )
                    ))


server <- function(input, output) { 
  
  output$ex_a <- renderPlot({
    mi_archivo <- read.csv2("dataset.csv")
    summary(mi_archivo)
    boxplot(mi_archivo$V1, main="População total",
            ylab="População", col=("blue"))
#    boxplot(mi_archivo$V2, main="% da população que vive em domicílios com banheiro e água encanada ",
#            ylab="População", col=("blue"))
#    boxplot(mi_archivo$V3, mi_archivo$V4, mi_archivo$V5, mi_archivo$V6, main="IDHM - Índice de Desenvolvimento Humano Municipal",
#            names=c("Municipal","Renda","Longevidade", "Educação"),
#            xlab="IDHM", ylab="Índice", col=("blue"))
    
#    boxplot(mi_archivo$V10, mi_archivo$V7, mi_archivo$V9, main="População em domicílios vulneráveis e com idoso",
#            names=c("Total","Masculina","Feminina"),
#            xlab="População", ylab="Índice", col=("blue"))
    
#    boxplot(mi_archivo$V8, main="Taxa de envelhecimento",
#            ylab="Taxa", col=("blue"))
    
    

#    bp1 <- ggplot(Dados_Lab01, aes(y = escore_socioeconomico)) +
#      geom_boxplot() +
#      labs(y="Escore Socioeconômico")+
#      theme_minimal()
    
#    bp2 <- ggplot(Dados_Lab01, aes(y = mortalidade)) +
#      geom_boxplot() +
#      labs(y="Taxa de Mortalidade")+
#      theme_minimal()
    
#    gg1 <- ggarrange(bp2, bp1, labels = c("A", "B"), ncol = 2, nrow = 1)
#    annotate_figure(gg1, top = text_grob("Boxplot", color = "black", face = "bold", size = 20))
    
  })
  
  output$ex_b <- renderPlot({
    mi_archivo <- read.csv2("dataset.csv")
    summary(mi_archivo)
 #   boxplot(mi_archivo$V1, main="População total",
#            ylab="População", col=("blue"))
    boxplot(mi_archivo$V2, main="% da população que vive em domicílios com banheiro e água encanada ",
            ylab="População", col=("blue"))
#    boxplot(mi_archivo$V3, mi_archivo$V4, mi_archivo$V5, mi_archivo$V6, main="IDHM - Índice de Desenvolvimento Humano Municipal",
#            names=c("Municipal","Renda","Longevidade", "Educação"),
#            xlab="IDHM", ylab="Índice", col=("blue"))
    
#    boxplot(mi_archivo$V10, mi_archivo$V7, mi_archivo$V9, main="População em domicílios vulneráveis e com idoso",
#            names=c("Total","Masculina","Feminina"),
#            xlab="População", ylab="Índice", col=("blue"))
    
#    boxplot(mi_archivo$V8, main="Taxa de envelhecimento",
#            ylab="Taxa", col=("blue"))

#    dd <- ggplot(Dados_Lab01, aes(x = escore_socioeconomico)) +
#      geom_histogram(bins=30,col = "black", fill = "white") +
#      labs(x="Escore Socioeconômico", y="Frequência")+
#      theme_minimal()
    
#    ddd <- ggplot(Dados_Lab01, aes(x = mortalidade)) +
#      geom_histogram(bins=30,col = "black", fill = "white") +
#      labs(x="Taxa de Mortalidade", y="Frequência")+
#      theme_minimal()
    
 #   gg <- ggarrange(ddd, dd, labels = c("A", "B"), ncol = 2, nrow = 1)
#    annotate_figure(gg, top = text_grob("Histograma", color = "black", face = "bold", size = 20))
  })
  output$ex_c <- renderPlot({
    mi_archivo <- read.csv2("dataset.csv")
    summary(mi_archivo)
        boxplot(mi_archivo$V3, mi_archivo$V4, mi_archivo$V5, mi_archivo$V6, main="IDHM - Índice de Desenvolvimento Humano Municipal",
                names=c("Municipal","Renda","Longevidade", "Educação"),
                xlab="IDHM", ylab="Índice", col=("blue"))
    
    #    boxplot(mi_archivo$V10, mi_archivo$V7, mi_archivo$V9, main="População em domicílios vulneráveis e com idoso",
    #            names=c("Total","Masculina","Feminina"),
    #            xlab="População", ylab="Índice", col=("blue"))
    
    #    boxplot(mi_archivo$V8, main="Taxa de envelhecimento",
    #            ylab="Taxa", col=("blue"))
    
  })
  output$ex_d <- renderPlot({
    mi_archivo <- read.csv2("dataset.csv")
        boxplot(mi_archivo$V10, mi_archivo$V7, mi_archivo$V9, main="População em domicílios vulneráveis e com idoso",
                names=c("Total","Masculina","Feminina"),
                xlab="População", ylab="Índice", col=("blue"))
    
    #    boxplot(mi_archivo$V8, main="Taxa de envelhecimento",
    #            ylab="Taxa", col=("blue"))
    
  })
  output$ex_e <- renderPlot({
    mi_archivo <- read.csv2("dataset.csv")

        boxplot(mi_archivo$V8, main="Taxa de envelhecimento",
                ylab="Taxa", col=("blue"))
    
  })
  
    
    
}

shinyApp(ui, server)

