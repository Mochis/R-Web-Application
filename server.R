library(shiny)

shinyServer(function(input, output){
  output$main <- renderPlot({
    curve(dnorm(x,1), from = -3, to = 5)
  })
  
# ======================= BINOMIAL NEGATIVA =========================================  
  valoresBinom <- reactive({
    p = input$pBinom
    r = input$r
    Y = c() #vector de valores
    N = input$nBinom
    
    for(i in 1:N){ 
      yi = 0
      xi = 0
      while(xi < r){ # hasta obtener el r-esimo exito
        randNum <- runif(1) #generamos un aleatorio de distribucion uniforme 
        if(randNum > p){ 
          yi <- yi+1
          xi <- xi+0
        }else{ # exito: randNum <= p
          xi <- xi+1
        }
      }
      Y <- c(Y, yi) #guardamos en array el valor i-esimo
    }
    return(Y)
  })
  
  dataBinomial <- reactive({
    v <- valoresBinom()
    dataBinomial = data.frame(v, pnbinom(v, input$r, input$pBinom), dnbinom(v, input$r, input$pBinom))
    names(dataBinomial) <- c("Valor", "Probabilidad", "Densidad")
    
    return(dataBinomial)
  })
  
  
  # renderizado grafica para binomial
  output$plotBinom <- renderPlot({
    if(input$probRealBinom){
      range <- input$probBinom
      plot(1:max(valoresBinom()), dnbinom(1:max(valoresBinom()), input$r, input$pBinom), col = "blue", type = "l", xlab = "Xi", ylab = "fi(xi)",  main = "Comparacion de dist. de frec. relativas con la dist. de probabilidad")
      hist(valoresBinom(), freq = F, add = T, col = "darkgrey", border = "white")
      points(range[1], dnbinom(range[1], input$r, input$pBinom), pch = 19)
      points(range[2], dnbinom(range[2], input$r, input$pBinom), pch = 19)
    }else{
      hist(valoresBinom(), add = F, breaks = "FD", xlab = "x", ylab = "f(x)", col = "darkgrey", border = "white", main = paste("Frecuencias absolutas con r = ", input$r, ", p = ", input$pBinom))
    }
   
  })

  output$plotBinom2 <- renderPlot({
    plot(1:max(valoresBinom()), pnbinom(1:max(valoresBinom()), input$r, input$pBinom), type = "s",xlab = "Numero de fracasos", ylab = "P(x)",
         main = paste("Funcion de Distr. Acum. con r = ", input$r, ", p = ", input$pBinom))
  })

  output$plotBinom3 <- renderPlot({
    par(mfrow = c(1, 2))
    v <- valoresBinom()
    x <- c(0:max(v))
    fx <- dnbinom(x, input$r, input$pBinom)
    res <- data.frame(x, fx)
    plot(res, type = "l", xlab = "x", ylab = "F(x)", col = "blue", main = "Distribucion de probabilidad real")
    plot(density(v), xlab = "x", ylab = "F(x)", col = "red", main = "Distribucion de probabilidad simulada")
  })

  output$plotBinom4 <- renderPlot({
    boxplot(valoresBinom(),
         main = paste("Grafico de caja con r = ", input$r, ", p = ", input$pBinom))
  })
  
  output$summaryBinom <- renderPrint({
    summary(valoresBinom())
  })

  output$summaryDataBinom <- renderPrint({
    str(dataBinomial())
  })

  output$tableMeanBinom <- renderTable({
    v <- valoresBinom()
    data.frame(Parametro = c("Media", "Varianza"), Teorica = c(input$r*(1-input$pBinom)/input$pBinom, input$r*(1-input$pBinom)/(input$pBinom)^2), Simulada = c(mean(v), var(v)))
  })

  output$tableProbBinom <- renderTable({
    v <- valoresBinom()
    range <- input$probBinom
    if(range[1] == range[2])
      data.frame(Probabilidad = paste("P(X = ", range[1], ")"), Real = as.character(dnbinom(range[1], input$r, input$pBinom)), Estimada = as.character(length(v[v == range[1]])/input$nBinom))
    else
      data.frame(Probabilidad = paste("P(", range[1], " <= X <= ", range[2], ")"), Real = as.character((pnbinom(range[2], input$r, input$pBinom)-pnbinom(range[1]-1, input$r, input$pBinom))), Estimada = as.character(length(v[v >= range[1] & v <= range[2]])/input$nBinom))
  })
  
  tableDataB <- reactive({
    table <- table(valoresBinom())
    ni <- as.vector(table)    
    fi <- (ni)/margin.table(table)
    Fi <- cumsum(ni)/margin.table(table)
    Ni <- cumsum(ni)
    
    return(data.frame(xi = names(table), ni, fi, Fi, Ni, Pi = Fi*100))
    
  })

  output$tableBinom <- renderDataTable({
    tableDataB()
  })
  
  output$dataTableBinom <- renderDataTable(
    dataBinomial(),
    options = list(iDisplayLength = 10)
  )
 
# ======================= EXPONENCIAL =========================================  
valoresExp <- reactive({
  lambda = input$lambda
  N = input$nExp
  Y = c() #vector de valores
  
  for(i in 1:N){
    randNum <- runif(1)
    xi <- -log(1-randNum)/lambda
    Y <- c(Y, xi)
  }
  
  return(Y)
})

dataExponencial <- reactive({
  v <- valoresExp()
  dataExponencial = data.frame(v, pexp(v, input$lambda), dexp(v, input$lambda))
  names(dataExponencial) <- c("Valor", "Probabilidad", "Densidad")
  
  return(dataExponencial)
})


# renderizado grafica para exponencial
output$plotExp <- renderPlot({
  if(input$densityExp){
    range <- input$probExp
    hist(valoresExp(), freq = F, add = F, breaks = "FD", xlab = "Valores simulados", ylab = "F(valores)", col = "darkgrey", border = "white", main = paste("Histograma de valores de la exponencial con lambda = ", input$lambda))
    curve(dexp(x, input$lambda), add = T, col = "blue")
    points(range[1], dexp(range[1], input$lambda), pch = 19)
    points(range[2], dexp(range[2], input$lambda), pch = 19)
  }else{
    hist(valoresExp(), add = F, breaks = "FD", xlab = "Valores simulados", ylab = "F(valores)", col = "darkgrey", border = "white", main = paste("Histograma de valores de la exponencial con lambda = ", input$lambda))
  }
  
})

output$plotExp2 <- renderPlot({
  v <- valoresExp()
  curve(pexp(x, input$lambda), from = min(v), to = max(v), xlab = "x", ylab = "F(x)", 
        main = paste("Funcion de dist. acumulada con lambda = ", input$lambda))
})

output$plotExp3 <- renderPlot({
  v <- valoresExp()
  plot(density(v), col = "red", lwd=2, main = paste("Funcion de densidad con lambda = ", input$lambda)) 
  curve(dexp(x, input$lambda), add = T, lwd = 2, col= "blue", main = paste("Funcion de densidad con lambda = ", input$lambda))
  legend("topright", legend = c("Teorica", "Simulada"), fill = c("blue", "red"))
  
})

output$plotExp4 <- renderPlot({
  boxplot(valoresExp(),
          main = paste("Grafico de caja con lambda = ", input$lambda))
})

output$summaryExp <- renderPrint({
  summary(valoresExp())
})

output$summaryDataExp <- renderPrint({
  str(dataExponencial())
})

output$tableMeanExp <- renderTable({
  v <- valoresExp()
  data.frame(Parametro = c("Media", "Varianza"), Teorica = c(1/input$lambda, 1/(input$lambda)^2), Simulada = c(mean(v), var(v)))
})

output$tableProbExp <- renderTable({
  v <- valoresExp()
  integrand <- function(x){input$lambda*exp(-input$lambda*x)}
  range <- input$probExp
  res <- integrate(integrand, range[1], range[2])
  if(range[1] == range[2])
    data.frame(Probabilidad = paste("P(X = ", range[1], ")"), Real = as.character(dexp(range[1], input$lambda)), Estimada = as.character(length(v[v == range[1]])/input$nExp))
  else
    data.frame(Probabilidad = paste("P(", range[1], " <= X <= ", range[2], ")"), Real = paste(as.character(res[1]), "con error absoluto <", as.character(res[2])), Estimada = as.character(length(v[v >= range[1] & v <= range[2]])/input$nExp))
})

tableDataE <- reactive({
  table <- table(valoresExp())
  ni <- as.vector(table)    
  fi <- (ni)/margin.table(table)
  Fi <- cumsum(ni)/margin.table(table)
  Ni <- cumsum(ni)
  
  return(data.frame(xi = names(table), ni, fi, Fi, Ni, Pi = Fi*100))
  
})

output$tableExp <- renderDataTable({
  tableDataE()
})

output$dataTableExp <- renderDataTable(
  dataExponencial(),
  options = list(iDisplayLength = 10)
)
##============================= GEOMETRICA =======================================
valoresGeom <- reactive({
  p = input$pGeom
  Y = c() #vector de valores
  N = input$nGeom
  
  for(i in 1:N){
    yi = 0
    xi = 0
    while(xi != 1){
      randNum <- runif(1) #generamos un aleatorio de distribucion uniforme 
      if(randNum > p){ 
        yi <- yi+1
        xi <- 0
      }else{ # exito: randNum <= p
        xi <- 1
      }
    }
    Y <- c(Y, yi) #guardamos en array el valor i-esimo
  }
  return(Y)
})

dataGeometrica <- reactive({
  v <- valoresGeom()
  dataGeometrica = data.frame(v, pgeom(v, input$pGeom), dgeom(v, input$pGeom))
  names(dataGeometrica) <- c("Valor", "Probabilidad", "Densidad")
  
  return(dataGeometrica)
})


# renderizado grafica para geometica
output$plotGeom <- renderPlot({
  if(input$probRealGeom){
    range <- input$probGeom
    plot(1:max(valoresGeom()), dgeom(1:max(valoresGeom()), input$pGeom), type = "l", col = "blue", xlab = "Xi", ylab = "fi(xi)", main = "Comparacion de dist. de frec. relativas con la dist. de probabilidad")
    hist(valoresGeom(),freq = F, add = T, col = "darkgrey", border = "white")
    points(range[1], dgeom(range[1], input$pGeom), pch = 19)
    points(range[2], dgeom(range[2], input$pGeom), pch = 19)
  }else{
    hist(valoresGeom(), add = F, breaks = "FD", xlab = "x", ylab = "f(x)", col = "darkgrey", border = "white", main = paste("Frecuencias absolutas con p = ", input$pGeom))
  }
  
})

output$plotGeom2 <- renderPlot({
  plot(1:max(valoresGeom()), pgeom(1:max(valoresGeom()), input$pGeom), type = "s", xlab = "Numero aciertos", ylab = "P(x)",
       main = paste("Funcion de Distr. Acum. con p = ", input$pGeom))
})

output$plotGeom3 <- renderPlot({
  par(mfrow = c(1, 2))
  v <- valoresGeom()
  x <- c(0:max(v))
  fx <- dgeom(x, input$pGeom)
  res <- data.frame(x, fx)
  plot(res, type = "l", xlab = "x", ylab = "F(x)", col = "blue", main = "Distribucion de probabilidad real")
  plot(density(v), xlab = "x", ylab = "F(x)", col = "red", main = "Distribucion de probabilidad simulada")
})

output$plotGeom4 <- renderPlot({
  boxplot(valoresGeom(),
          main = paste("Grafico de caja con p = ", input$pGeom))
})

output$summaryGeom <- renderPrint({
  summary(valoresGeom())
})

output$summaryDataGeom <- renderPrint({
  str(dataGeometrica())
})

output$tableMeanGeom <- renderTable({
  v <- valoresGeom()
  data.frame(Parametro = c("Media", "Varianza"), Teorica = c((1-input$pGeom)/input$pGeom, (1-input$pGeom)/(input$pGeom)^2), Simulada = c(mean(v), var(v)))
})

output$tableProbGeom <- renderTable({
  v <- valoresGeom()
  range <- input$probGeom
  if(range[1] == range[2])
    data.frame(Probabilidad = paste("P(X = ", range[1], ")"), Real = as.character(dgeom(range[1], input$pGeom)), Estimada = as.character(length(v[v == range[1]])/input$nGeom))
  else
    data.frame(Probabilidad = paste("P(", range[1], " <= X <= ", range[2], ")"), Real = as.character((pgeom(range[2], input$pGeom)-pgeom(range[1]-1, input$pGeom))), Estimada = as.character(length(v[v >= range[1] & v <= range[2]])/input$nGeom))
})

tableDataG <- reactive({
  table <- table(valoresGeom())
  ni <- as.vector(table)    
  fi <- (ni)/margin.table(table)
  Fi <- cumsum(ni)/margin.table(table)
  Ni <- cumsum(ni)
  
  return(data.frame(xi = names(table), ni, fi, Fi, Ni, Pi = Fi*100))
  
})

output$tableGeom <- renderDataTable({
  tableDataB()
})

output$dataTableGeom <- renderDataTable(
  dataGeometrica(),
  options = list(iDisplayLength = 10)
)

##============================= WEIBULL =======================================
valoresWeib <- reactive({
  a = input$a # alpha
  #beta = 1 # b
  N = input$nWeib
  Y = c() #vector de valores
  
  for(i in 1:N){
    randNum <- runif(1)
    xi <- (-log(randNum))^(1/a) #al ser xi distribuido uniformemente podemos sustituir 
    Y <- c(Y, xi)                        # (1-randNum) por randNum  
  }
  
  return(Y)
})

dataWeibull <- reactive({
  v <- valoresWeib()
  dataWeibull = data.frame(v, pweibull(v, input$a), dweibull(v, input$a))
  names(dataWeibull) <- c("Valor", "Probabilidad", "Densidad")
  
  return(dataWeibull)
})


# renderizado grafica para Weibull
output$plotWeib <- renderPlot({
  if(input$densityWeib){
    range <- input$probWeib
    hist(valoresWeib(), freq = F, add = F, breaks = "FD", xlab = "Valores simulados", ylab = "F(valores)", col = "darkgrey", border = "white", main = paste("Histograma de valores de la exponencial con alpha = ", input$a))
    curve(dweibull(x, input$a), add = T, col = "blue")
    points(range[1], dweibull(range[1], input$a), pch = 19)
    points(range[2], dweibull(range[2], input$a), pch = 19)
  }else{
    hist(valoresWeib(), add = F, breaks = "FD", xlab = "Valores simulados", ylab = "F(valores)", col = "darkgrey", border = "white", main = paste("Histograma de valores de la exponencial con alpha = ", input$a))
  }
  
})

output$plotWeib2 <- renderPlot({
  v <- valoresWeib()
  curve(pweibull(x, input$a), from = min(v), to = max(v), xlab = "x", ylab = "F(x)", 
        main = paste("Funcion de dist. acumulada con alpha = ", input$a))
})

output$plotWeib3 <- renderPlot({
  v <- valoresWeib()
  plot(density(v), col = "red", lwd=2, main = paste("Funcion de densidad con alpha = ", input$a)) 
  curve(dweibull(x, input$a), add=T, col = "blue", lwd=2, main = paste("Funcion de densidad con alpha = ", input$a))
  legend("topright", legend = c("Teorica", "Simulada"), fill = c("blue", "red"))
})

output$plotWeib4 <- renderPlot({
  boxplot(valoresWeib(),
          main = paste("Grafico de caja con alpha = ", input$a))
})

output$summaryWeib <- renderPrint({
  summary(valoresWeib())
})

output$summaryDataWeib <- renderPrint({
  str(dataWeibull())
})

output$tableMeanWeib <- renderTable({
  v <- valoresWeib()
  data.frame(Parametro = c("Media", "Varianza"), Teorica = c(gamma(1+1/input$a), (gamma(1+2/input$a)-gamma(1+1/input$a)^2)), Simulada = c(mean(v), var(v)))
})

output$tableProbWeib <- renderTable({
  v <- valoresWeib()
  integrand <- function(x){input$a*((x)^(input$a-1))*exp(-((x)^input$a))}
  range <- input$probWeib
  res <- integrate(integrand, range[1], range[2])
  if(range[1] == range[2])
    data.frame(Probabilidad = paste("P(X = ", range[1], ")"), Real = as.character(dweibull(range[1], input$a)), Estimada = as.character(length(v[v == range[1]])/input$nWeib))
  else
    data.frame(Probabilidad = paste("P(", range[1], " <= X <= ", range[2], ")"), Real = paste(as.character(res[1]), "con error absoluto <", as.character(res[2])), Estimada = as.character(length(v[v >= range[1] & v <= range[2]])/input$nWeib))
})

tableDataW <- reactive({
  table <- table(valoresWeib())
  ni <- as.vector(table)    
  fi <- (ni)/margin.table(table)
  Fi <- cumsum(ni)/margin.table(table)
  Ni <- cumsum(ni)
  
  return(data.frame(xi = names(table), ni, fi, Fi, Ni, Pi = Fi*100))
  
})

output$tableWeib <- renderDataTable({
  tableDataW()
})

output$dataTableWeib <- renderDataTable(
  dataWeibull(),
  options = list(iDisplayLength = 10)
)

})