#TAREA 1 y 2
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(markdown)


#FUNCION PARA ECUCACIONES Y SU SIMETRIA
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}


shinyServer(function(input, output){
  
  
#TAREA 1
  
  Finv <- function(u, lambda) {return(-log(1-u)/lambda)}
  set.seed(20161123)
  
  ElInput <- reactive({
    nsim <- input$simula
    lambda <- input$lambda
    U <- runif(nsim)
    X <- Finv(U, lambda)
  })
  
  Comp <- reactive({
    Y <- rexp(input$simula,rate=input$lambda)
  })
  
  output$trendPlot <- renderPlotly({
    
    X <- ElInput()
    
    X2 <- seq(0,max(X),(max(X)-0)/input$simula)
    funcion <- function(x) input$lambda * exp(- (x * input$lambda)) * input$simula/(input$lambda*10)
    aplicada <- sapply(X2, funcion)
    
    plot_ly(x=X,type="histogram", opacity=0.6, name = "Func. Inversa", 
            marker = list (color="rgb(0,102, 51),"), opacity = .5) %>%
      add_trace(x=X2, y=aplicada, type="bar", opacity=1, name = "Func. Exp",
                marker = list (color="rgb(160, 160, 160),"), opacity = .5)
   #SE MEJORA GRAFICO   
    
  })
  
  output$text1 <- renderPrint({
    
    X <- ElInput()
    Y <- Comp()
    KS <- ks.test(X, Y)
    
    KS
  })
  
  #TAREA 2
  
  al_cero <- function(x){dist(c(x,0))}
  
  integral_MC <- function(f,a,b,N){
    
    simulacion <- runif(N,a,b)
    
    funcion_aplicada <- f(simulacion)
    minimo <- min(funcion_aplicada)
    maximo <- max(funcion_aplicada)
    comparada <- runif(N,min(minimo,0),max(maximo,0))
    
    dist_fun <- sapply(funcion_aplicada,al_cero)
    dist_com <- sapply(comparada,al_cero)
    
    bajo_la_curva <- sum(dist_fun >= dist_com & (sign(comparada)==1 & sign(funcion_aplicada)==1))
    sobre_la_curva <- sum(dist_fun >= dist_com & (sign(comparada)==-1 & sign(funcion_aplicada)==-1))
    (bajo_la_curva - sobre_la_curva)/N * abs(max(maximo,0)-min(minimo,0))*abs(b-a)
  }
  
  fun <- reactive({
    texto <- paste("aux <- function(x) ",input$funcion)
    eval(parse(text = texto))
    aux
  })
  
  ElInput2 <- reactive({
    a     <- input$a
    b     <- input$b
    N     <- input$simul
    f     <- fun()
    alphas <- input$alphas
    return(list(a,b,N,f,alphas))
  })
  
  LaIntegral <- reactive({
    c(a,b,N,f,alphas) := ElInput2()
    int <- integral_MC(f,a,b,N)
    int
  })
  
  confianza <- function(Numero){
    c(a,b,N,f,alphas) := ElInput2()
    uniforme <- runif(Numero, min = a, max = b)
    aplicada <- (b-a)*f(uniforme)
    media <- mean(aplicada)
    desvia <- qnorm(alphas/2, lower.tail = F) * sd(aplicada)/sqrt(Numero)
    minimo <- media - desvia
    maximo <- media + desvia
    lista <- list(media,minimo,maximo,Numero)
    df <- data.frame(lista)
    names(df) <- paste(c("media","minimo","maximo","Numero"))
    df
  }
  
  Intervalos <- reactive({
    c(a,b,N,f,alphas) := ElInput2()
    repeticiones <- seq(10,N,30)
    sapply(repeticiones,confianza, simplify = FALSE) %>%
      bind_rows()
  })
  
  output$text2 <- renderText({
    
    integral <- round(LaIntegral(), 5)
    
    
    as.character(integral)
  })
  
  output$graf_fun <- renderPlot({
    c(a,b,N,f,alphas) := ElInput2()
    x <- seq(a,b,(b-a)/N)
    y <- f(x)
    df <- data.frame(x,y)
    
    ggplot(df,aes(x,y))+
      geom_line(colour="#990000",size=1.5)+
      geom_area(fill="#06b9C7",alpha=0.3)+
      ylab('f(x)')
  })
  
  output$graf_conf <- renderPlot({
    c(a,b,N,f,alphas) := ElInput2()
    ggplot(Intervalos(), aes(x = Numero, y = media)) + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), 
                  alpha =0.4, fill = '#06b9C7') + 
      geom_line(color = 'black', size = 0.6) + 
      ylab('Valor de la integral MC') + 
      xlab("Número de simulaciones")+ 
      ggtitle("Intervalos de Confianza")
  })
  
})