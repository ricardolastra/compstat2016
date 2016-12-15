#TAREA 1 y 2
library(shiny)
library(plotly)
library(dplyr)
library(markdown)
library(ggplot2)
library(shinydashboard)

# Creamos el display que ajusta al tamaño de la pantalla
dashboardPage(
  
  # Titulo de la APP
  dashboardHeader(title = "EstComp"),
  
  
  # TITULOS E INICIO DEL SHINY (DASHBOARD)
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Método - Función Inversa"),
      menuItem("Integración numérica - Monte Carlo"),
      menuItem("Tarea 3. The_MCMC_revolution"),
      menuItem("Tarea 4-6. Metropolis-Hastings"),
      menuItem("Créditos")
    )
  ),
  dashboardBody(
    img(src="itam.png"),
    h3("Ricardo Lastra Cuevas  - 000160167"),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Tarea 1. Método - Función Inversa",
                          sidebarLayout(
                             sidebarPanel(
                               sliderInput("simula",
                                           "Selecciona # de simulaciones:",
                                            min = 10,
                                            max = 10000,
                                            value = 1000),
                            numericInput("lambda",
                                          "Parámetro lambda:",
                                          value = 0.5)
                            ),
                            mainPanel(
                              h3("Visualizamos la simulación:"),
                              plotlyOutput("trendPlot"),
                              h2("Prueba Smirnov-Kolmogorov:"),
                              textOutput("text1")
                            )
                          )
                  ),

                  tabPanel("Tarea 2. Integración numérica - Monte Carlo",
                           sidebarLayout(
                             sidebarPanel(
                               textInput("funcion",
                                         "Función a Integrar:",
                                         value = "cospi(x)"),
                               numericInput("simul",
                                            "Número de simulaciones:",
                                            value = 10000,
                                            min = 10, 
                                            max = 100000),
                              numericInput("a",
                                            "Límite inferior:",
                                            value = -2),
                              numericInput("b",
                                            "Límite superior:",
                                            value = 2),
                              numericInput("alphas",
                                            "Significancia (alpha):",
                                            value = 0.05)
                               ),
                           
                            mainPanel(
                               h3("Función de Integración"),
                              plotOutput("graf_fun"),
                              h3("Resultado de la Integración por Monte Carlo"),
                              textOutput("text2"),
                               h3("Intervalos de confianza"),
                               plotOutput("graf_conf")
                             )
                           )
                  ),
                  
                  tabPanel("Tarea 3. The_MCMC_revolution",
                           mainPanel(
                             h3("Resumén"),
                             tags$iframe(src="The_MCMC_revolution.pdf", width="1100", height="500")
                             )
                           ),
                  
                  
                  tabPanel("Tarea 4-6. Metropolis-Hastings",
                           mainPanel(
                             h3("SE ANEXA CODIGO EN GIT"))),
                             
                             
                  tabPanel("Créditos",
                           mainPanel(h4("Agradecimientos:       
                                        Mauricio G. Tec, 
                                        A todos los compañeros de la maestria de DS, 
                                        A Guille por donar su depa de laboratorio, 
                                        A mis compañeros de trabajo, 
                                        A el ITAM,
                                        A a. Gelman, G. O. Roberts and W.R. Gilks y 
                                        A la Universidad de Cambridge.")))
                  
                  
                  )
      )
    )
)

  



