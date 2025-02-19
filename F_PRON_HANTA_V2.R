library(shiny)
library(rmarkdown)

# Definición de los pesos normalizados para cada factor pronóstico
weights <- c(AgeOver40 = 2.17, FemaleGender = 0.58, HematocritOver42 = 2.03,
             IncreasedCreatinine = 1.32, ChestInfiltrates = 5.09, Vomiting = -1.00,
             PlateletsBelow100 = 0.59, ASTIncrease = 1.08, Headache = -6.25, Leukocytosis = 0.47)

# Puntaje máximo teórico (sólo se suman los pesos positivos) y umbral de bajo riesgo
max_score <- sum(weights[weights > 0]) * 10  # Multiplicado por 10 para ajustar la escala
threshold_low_risk <- max_score * 0.16

# UI
ui <- fluidPage(
  titlePanel("Calculadora de Riesgo de Mortalidad por Hantavirus"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("ageOver40", "Mayor de 40 años", value = FALSE),
      checkboxInput("femaleGender", "Género femenino", value = FALSE),
      checkboxInput("hematocritOver42", "Hematocrito mayor de 42%", value = FALSE),
      checkboxInput("increasedCreatinine", "Creatinina aumentada (>1.4 mg/dl)", value = FALSE),
      checkboxInput("chestInfiltrates", "Infiltrados en radiografía de tórax", value = FALSE),
      checkboxInput("vomiting", "Presencia de vómitos", value = FALSE),
      checkboxInput("plateletsBelow100", "Recuento de Plaquetas < 100,000/mm³", value = FALSE),
      checkboxInput("astIncrease", "Aumento de AST x 3", value = FALSE),
      checkboxInput("headache", "Presencia de Cefalea", value = FALSE),
      checkboxInput("leukocytosis", "Leucocitosis > 12,000/mm³", value = FALSE),
      actionButton("calculate", "Calcular Puntaje de Riesgo"),
      downloadButton("download_report", "Exportar a HTML")
    ),
    mainPanel(
      textOutput("result"),
      plotOutput("risk_plot"),
      uiOutput("references")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Realiza el cálculo de riesgo cuando se presiona el botón
  risk_calculation <- eventReactive(input$calculate, {
    # Selecciona los factores activos
    factores <- names(weights)[as.logical(c(input$ageOver40, input$femaleGender, 
                                            input$hematocritOver42, input$increasedCreatinine, 
                                            input$chestInfiltrates, input$vomiting, 
                                            input$plateletsBelow100, input$astIncrease, 
                                            input$headache, input$leukocytosis))]
    score <- sum(weights[factores]) * 10
    
    # Determina el nivel de riesgo
    if (length(factores) <= 2 || input$chestInfiltrates) {
      risk_level <- ifelse(score <= threshold_low_risk, "Bajo Riesgo", "Alto Riesgo")
    } else {
      risk_level <- "Alto Riesgo"
    }
    
    # Calcula el porcentaje de riesgo (escala 0 a 100)
    progress <- score / max_score
    risk_percent <- round(progress * 100, 1)
    
    list(score = score, risk_level = risk_level, progress = progress, risk_percent = risk_percent)
  })
  
  # Muestra el resultado del cálculo
  output$result <- renderText({
    req(risk_calculation())
    rc <- risk_calculation()
    sprintf("Tu puntaje de riesgo calculado es: %.1f\nNivel de Riesgo: %s", rc$score, rc$risk_level)
  })
  
  # Genera el gráfico de barras que muestra el porcentaje de riesgo
  output$risk_plot <- renderPlot({
    req(risk_calculation())
    rc <- risk_calculation()
    progress <- rc$progress
    risk_percent <- rc$risk_percent
    risk_level <- rc$risk_level
    bar_color <- ifelse(risk_level == "Bajo Riesgo", "green", "red")
    
    # Se crea el gráfico horizontal con la escala de 0 a 1
    bp <- barplot(c(progress, 1 - progress), names.arg = c("Riesgo", ""), 
                  col = c(bar_color, "lightgray"), horiz = TRUE, xlim = c(0, 1), 
                  border = NA, space = 0)
    
    # Se agrega el porcentaje de riesgo en el centro de la barra
    if (progress >= 0.1) {
      text(x = progress / 2, y = bp[1], labels = paste0(risk_percent, "%"), 
           col = "white", cex = 1.5)
    } else {
      text(x = progress + 0.05, y = bp[1], labels = paste0(risk_percent, "%"), 
           col = "black", cex = 1.5, pos = 4)
    }
    
    mtext("0%                      100%", side = 1, line = 2)
  })
  
  # Texto de fundamentación, bibliografía y disclaimer
  bibliography_text <- "<h4>Fundamento del Modelo y Bibliografía</h4>
<p><strong>Identificación de factores pronósticos para el desarrollo de un modelo de evaluación de riesgo (MER)</strong><br>
Identificamos con la revisión sistemática, varios factores críticos asociados con un aumento en el riesgo de mortalidad en individuos con infección por hantavirus. La evidencia de certeza moderada sugirió que la edad superior a 40 años, el género femenino, niveles elevados de creatinina (>1.4 mg/dL), hematocrito aumentado (>42%) y la presencia de infiltrados en radiografías de tórax.</p>
<p><strong>Selección de factores pronósticos</strong><br>
Basado en nuestra revisión sistemática, los factores pronósticos más confiables asociados con la mortalidad por infección de hantavirus, que presentan una certeza de evidencia de moderada a alta, incluyen:
<ul>
  <li>Edad mayor de 40 años - OR 1.95 (1.32, 2.88)</li>
  <li>Género femenino - OR 1.35 (1.23, 1.48)</li>
  <li>Hematocrito mayor de 42 - OR 1.8 (1.32, 2.46)</li>
  <li>Creatinina aumentada (>1.4 g/dl) - OR 1.58 (1.36, 1.84)</li>
  <li>Infiltrados en radiografía de tórax - OR 2.72 (1.36, 5.46)</li>
</ul>
</p>
<p><strong>Modelo de Regresión Logística</strong><br>
El modelo se construyó utilizando regresión logística. Los coeficientes (log OR) se calcularon de la siguiente manera:<br>
- Edad mayor de 40 años: β = log(1.95) ≈ 0.667<br>
- Género femenino: β = log(1.35) ≈ 0.300<br>
- Hematocrito > 42: β = log(1.8) ≈ 0.588<br>
- Creatinina aumentada (>1.4 g/dl): β = log(1.58) ≈ 0.457<br>
- Infiltrados en radiografía de tórax: β = log(2.72) ≈ 1.000<br>
<br>
El modelo final se representa como:<br>
LP(Y) = β0 + β1*X1 + β2*X2 + β3*X3 + β4*X4 + β5*X5<br>
donde P(Y=1) es la probabilidad de mortalidad.</p>
<p><strong>Resultados y Contribución de Factores</strong><br>
Se presenta la siguiente tabla ilustrativa:<br>
<table border='1' style='border-collapse:collapse;'>
  <tr>
    <th>Factores de Riesgo</th>
    <th>OR (95% CI)</th>
    <th>β (log OR)</th>
    <th>SE</th>
    <th>Contribución al Riesgo Total (%)</th>
  </tr>
  <tr>
    <td>Edad > 40 años</td>
    <td>1.95 (1.32, 2.88)</td>
    <td>0.667</td>
    <td>0.10</td>
    <td>20.0</td>
  </tr>
  <tr>
    <td>Género femenino</td>
    <td>1.35 (1.23, 1.48)</td>
    <td>0.300</td>
    <td>0.09</td>
    <td>9.0</td>
  </tr>
  <tr>
    <td>Hematocrito > 42</td>
    <td>1.8 (1.32, 2.46)</td>
    <td>0.588</td>
    <td>0.12</td>
    <td>17.6</td>
  </tr>
  <tr>
    <td>Creatinina aumentada (>1.4 g/dl)</td>
    <td>1.58 (1.36, 1.84)</td>
    <td>0.457</td>
    <td>0.11</td>
    <td>13.7</td>
  </tr>
  <tr>
    <td>Infiltrados en radiografía de tórax</td>
    <td>2.72 (1.36, 5.46)</td>
    <td>1.000</td>
    <td>0.15</td>
    <td>30.0</td>
  </tr>
</table>
</p>
<p><strong>Escenario Clínico</strong><br>
Se presenta un caso hipotético de un paciente masculino de 42 años, trabajador rural, con factores de riesgo específicos, donde el modelo estima una probabilidad de mortalidad de aproximadamente 43.91%.</p>
<p><strong>Disclaimer</strong><br>
<strong>Este modelo es provisorio y requiere un proceso de validación interna y externa.</strong> La información presentada se basa en datos simulados y revisiones sistemáticas previas, y no debe utilizarse para la toma de decisiones clínicas sin la validación adecuada.</p>
<p>Prototipo del Modelo RAM: <a href='https://fernando-tortosa.shinyapps.io/ARBO' target='_blank'>https://fernando-tortosa.shinyapps.io/ARBO</a></p>"
  
  # Muestra la bibliografía y disclaimer en la interfaz
  output$references <- renderUI({
    HTML(bibliography_text)
  })
  
  # Configuración para la exportación del reporte en HTML usando R Markdown
  output$download_report <- downloadHandler(
    filename = function() { "hantavirus_risk_report.html" },
    content = function(file) {
      # Se pasan los parámetros calculados al documento Rmd
      params <- risk_calculation()
      rmarkdown::render("hantavirus_risk_report.Rmd", 
                        output_file = file,
                        params = list(score = params$score,
                                      risk_level = params$risk_level,
                                      risk_percent = params$risk_percent))
    }
  )
}

shinyApp(ui = ui, server = server)
