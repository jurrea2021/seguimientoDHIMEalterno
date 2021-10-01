#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


source("./global.R")





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #######################################################################################################################
  # Areas Operativas ---> Seguimiento
  #######################################################################################################################
  
  meses = c("Enero" = 1,"Febrero" = 2,"Marzo" = 3,"Abril" = 4,
            "Mayo" = 5,"Junio" = 6,"Julio" = 7,"Agosto" = 8,
            "Septiembre" = 9,"Octubre" = 10,"Noviembre" = 11,"Diciembre" = 12)
  
  output$text <- renderText({
    paste("Cantidad de estaciones por rangos de días de digitación en el ",input$anio," para la etiqueta ", input$variable)
  })
  
  output$text2 <- renderText({
    paste("Cantidad de estaciones por rangos de días de digitación en ",names(meses)[meses == input$mes]," de ",input$anio," para la etiqueta", input$variable)
  })
  
  output$text3 <- renderText({
    paste("Estaciones del ",input$AO," con cantidad de datos de la etiqueta ",input$variable,"en el ",input$anio)
  })
  
  output$text4 <- renderText({
    paste("Estaciones del ",input$AO," con cantidad de datos de la etiqueta ",input$variable," en ",names(meses)[meses == input$mes]," de ",input$anio)
  })
  
  output$text5 <- renderText({
    paste("Cantidad de datos de la etiqueta ",input$variable," y rangos por estación para el ",input$AO," en el ",input$anio)
  })
  
  output$text6 <- renderText({
    paste("Cantidad de datos de la etiqueta ",input$variable," y rangos por estación para el ",input$AO," en ",names(meses)[meses == input$mes]," de ",input$anio)
  })
  
  output$text7 <- renderText({
    paste("Porcentaje de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," para el ",input$anio," en el DHIME.")
  })
  output$text8 <- renderText({
    paste("Porcentaje de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," en cada mes de ",input$anio," en el DHIME, en el ",input$AO)
  })
  output$text9 <- renderText({
    paste("Cantidad de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," para el ",input$anio," en el DHIME.")
  })
  output$text10 <- renderText({
    paste("Cantidad de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," en cada mes de ",input$anio," en el DHIME, en el ",input$AO)
  })
  output$text11 <- renderText({
    paste("Porcentaje de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," por año en el DHIME para el ",input$AO)
  })
  
  
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "Las gráficas de barras apiladas indican por año (gráfica izquierda) y por mes (gráfica derecha) la cantidad de estaciones en cada rango de periodo de digitación de la etiqueta seleccionada, agrupados por Área Operativa.\n Están acondicionadas por los filtros de Año, Mes y Etiqueta DHIME.",type = "info")
  })
  
  observeEvent(input$ayuda2, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año, Mes y Etiqueta DHIME, junto con el filtro de Área Operativa, se asocian los mapas y tablas de cantidades y porcentajes correspondientes a las estaciones que hacen parte de los rangos de digitación.\n Estos productos se encuentran a escala Anual (columna izquierda) y Mensual (columna derecha) organizados en pestañas.",type = "info")
  })
  
  ## Se filtra la tabla con la cantidad de estaciones por rangos y por años del 2010 al 2020
  RESUMEN_ORG_1 = reactive({
    RESUMEN_ORG_A = RESUMEN_ORG %>% filter(anio == input$anio & VARIABLE == input$variable)
    RESUMEN_ORG_A$AREA_OPERATIVA = paste0("AO ",substr(RESUMEN_ORG_A$AREA_OPERATIVA,16,17))
    return(RESUMEN_ORG_A)
  })
  
  RESUMEN_ORG_2 = reactive({
    TAB_GRAF_B = TAB_GRAF_2 %>% filter(anio == input$anio & VARIABLE == input$variable & mes == input$mes)
    TAB_GRAF_B$AREA_OPERATIVA = paste0("AO ",substr(TAB_GRAF_B$AREA_OPERATIVA,16,17))
    return(TAB_GRAF_B)
  })
  
  # Se genera la grafica del resumen de cantidad de estaciones con valores digitados de las variables por Areas Operativas
  
  output$grafico = renderPlotly({
    fig <- plot_ly(RESUMEN_ORG_1(), x = ~AREA_OPERATIVA, y = ~COMPLETO, type = 'bar', name = 'COMPLETO',text = ~paste0("Porcentaje: ",round((COMPLETO/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig <- fig %>% add_trace(y = ~periodoA, name = '10 meses - 1 año',text = ~paste0("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig <- fig %>% add_trace(y = ~periodoB, name = '8 meses - 10 meses',text = ~paste0("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig <- fig %>% add_trace(y = ~periodoC, name = '6 meses - 8 meses',text = ~paste0("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig <- fig %>% add_trace(y = ~periodoD, name = '4 meses - 6 meses',text = ~paste0("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig <- fig %>% add_trace(y = ~periodoE, name = '< 4 meses',text = ~paste0("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
  })
  
  output$grafico2 = renderPlotly({
    fig2 <- plot_ly(RESUMEN_ORG_2(), x = ~AREA_OPERATIVA, y = ~periodoA, type = 'bar', name = 'Mes Completo',text = ~paste("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoB, name = '24 días - Mes Completo',text = ~paste("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoC, name = '15 días - 24 días',text = ~paste("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoD, name = '10 días - 15 días',text = ~paste("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoE, name = '5 días - 10 días',text = ~paste("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoF, name = '< 5 días',text = ~paste("Porcentaje: ",round((periodoF/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig2 <- fig2 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
    fig2
  })
  
  TABLA_Z_PRE = reactive({
    TABLA_Z_PRE = conteoConsolidado %>% filter(VARIABLE == input$variable & anio == input$anio)
    TABLA_Z_PRE = merge(TABLA_Z_PRE,CNE_IDEAM[,c("CODIGO","latitud","longitud","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA")],by.x = "CodigoEstacion",by.y = "CODIGO",all.x = T)
    TABLA_Z_PRE$AREA_OPERATIVA = substr(TABLA_Z_PRE$AREA_OPERATIVA,1,17)
    TABLA_Z_PRE = TABLA_Z_PRE %>% filter(AREA_OPERATIVA == input$AO)
    return(TABLA_Z_PRE)
  })
  
  TABLA_Z = reactive({  
    TABLA_Z = TABLA_Z_PRE()
    TABLA_Z$VARIABLE = NULL
    TABLA_Z$AREA_OPERATIVA = NULL
    TABLA_Z$anio = NULL
    TABLA_Z$latitud = NULL
    TABLA_Z$longitud = NULL
    TABLA_Z$CodigoEstacion = NULL
    TABLA_Z = TABLA_Z[,c("nombre","MUNICIPIO","DEPARTAMENTO","RANGO","CONTEO")]
    TABLA_Z
  })
  
  output$mapAnual = renderLeaflet({
    dataAnualAAW = TABLA_Z_PRE() 
    
    # AreasOpPoly1 = AreasOpPoly %>% filter(SEDE == names(AreasOpAlt)[AreasOpAlt == input$AO])
    AreasOpPoly1 = AreasOpPoly[AreasOpPoly$SEDE %in% names(AreasOpAlt)[AreasOpAlt == input$AO],]
    
    
    pal = colorFactor(palette = c("#00B14B", "#6924D3", "#0077BE", "#EBBD10","#756000","#C50B09"),levels = c("01 - COMPLETO","02 - 10 meses - 1 anio","03 - 8 meses - 10 meses","04 - 6 meses - 8 meses","05 - 4 meses - 6 meses","06 - < 4 meses"))
    
    leaflet(dataAnualAAW) %>%
      # addProviderTiles("HERE.normalNightMobile") %>%
      addTiles() %>%
      addPolygons(data = AreasOpPoly1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "#C8C9D7", fillOpacity = 0.8,group = "AreaOperativa",label = ~ SEDE) %>%
      addCircleMarkers(lng=dataAnualAAW$longitud,lat=dataAnualAAW$latitud,group = "estacion",color = ~pal(RANGO),radius = 6,label =paste("rango:", dataAnualAAW$RANGO,"<br>","nombre:", dataAnualAAW$nombre, "<br>","conteo datos:", dataAnualAAW$CONTEO) %>%
                         lapply(htmltools::HTML)
      ) %>% 
      leaflet::addLegend(pal = pal, values = dataAnualAAW$RANGO, position = "bottomleft") %>% 
      addLayersControl(
        overlayGroups = c("AreaOperativa", "estacion"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  output$mapMensual = renderLeaflet({
    y = TABLA_C_PRE()
    AreasOpPoly1 = AreasOpPoly[AreasOpPoly$SEDE %in% names(AreasOpAlt)[AreasOpAlt == input$AO],]
    pal = colorFactor(palette = c("#00B14B", "#6924D3", "#0077BE", "#EBBD10","#756000","#C50B09"),levels = c("mesCompleto","24Dias-mesCompleto","15Dias-24Dias","10Dias-15Dias","5Dias-10Dias","<5Dias"))
    # colorRampPalette(c('red', 'green'))(length(t$RANGO))
    leaflet() %>%
      # addProviderTiles("HERE.normalNightMobile") %>%
      addTiles() %>%
      addPolygons(data = AreasOpPoly1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "#C8C9D7", fillOpacity = 0.8,group = "AreaOperativa",label = ~ SEDE) %>%
      addCircleMarkers(data = y,lng=y$longitud,lat=y$latitud,group = "estacion",color = ~pal(RANGO),radius = 6,label =paste("rango:", y$RANGO, "<br>",
                                                                                                                            "nombre:", y$nombre, "<br>",
                                                                                                                            "conteo datos:", y$CONTEO) %>% lapply(htmltools::HTML)
                       
      ) %>%
      leaflet::addLegend(pal = pal, values = y$RANGO, position = "bottomleft") %>%
      addLayersControl(
        overlayGroups = c("AreaOperativa", "estacion"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  
  output$tablaAnual = renderDataTable({datatable(TABLA_Z(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaAnual <- downloadButtonTable(TABLA_Z())
  
  TABLA_R = reactive({
    TABLA_R = RESUMEN_ORG_PORC_MES %>% filter(anio == input$anio & VARIABLE == input$variable & AREA_OPERATIVA == input$AO)
    TABLA_R$VARIABLE = NULL
    TABLA_R$anio = NULL
    TABLA_R$AREA_OPERATIVA = NULL
    colnames(TABLA_R) = c("mes","MesCompleto","24Dias-Mes","15Dias-24Dias","10Dias-15Dias","5Dias-10Dias","<5Dias")
    TABLA_R
  })
  output$RESUMEN_ORG_PORC_MES = renderDataTable({datatable(TABLA_R(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadRESUMEN_ORG_PORC_MES <- downloadButtonTable(TABLA_R())
  
  TABLA_K = reactive({
    TABLA_K = ConteoMes %>% filter(anio == input$anio & VARIABLE == input$variable & AREA_OPERATIVA == input$AO)
    TABLA_K$VARIABLE = NULL
    TABLA_K$anio = NULL
    TABLA_K$AREA_OPERATIVA = NULL
    colnames(TABLA_K) = c("mes","MesCompleto","24Dias-Mes","15Dias-24Dias","10Dias-15Dias","5Dias-10Dias","<5Dias")
    TABLA_K[is.na(TABLA_K)] = 0
    TABLA_K
  })
  output$ConteoMes = renderDataTable({datatable(TABLA_K(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadConteoMes <- downloadButtonTable(TABLA_K())
  
  TABLA_Q = reactive({
    TABLA_Q = RESUMEN_ORG_PORC %>% filter(anio == input$anio & VARIABLE == input$variable)
    TABLA_Q$VARIABLE = NULL
    TABLA_Q$anio = NULL
    TABLA_Q$AREA_OPERATIVA = paste0("AO ",substr(TABLA_Q$AREA_OPERATIVA,16,17))
    colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_Q
  })
  output$tablaQ = renderDataTable({datatable(TABLA_Q(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaQ <- downloadButtonTable(TABLA_Q())
  
  TABLA_T = reactive({
    TABLA_T = RESUMEN_ORG %>% filter(anio == input$anio & VARIABLE == input$variable)
    TABLA_T$VARIABLE = NULL
    TABLA_T$anio = NULL
    TABLA_T$AREA_OPERATIVA = paste0("AO ",substr(TABLA_T$AREA_OPERATIVA,16,17))
    colnames(TABLA_T) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses","TOTAL")
    TABLA_T
  })
  output$tablaT = renderDataTable({datatable(TABLA_T(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaT = downloadButtonTable(TABLA_T())
  
  TABLA_S = reactive({
    TABLA_S = RESUMEN_ORG_PORC %>% filter(AREA_OPERATIVA == input$AO & VARIABLE == input$variable)
    TABLA_S$VARIABLE = NULL
    TABLA_S$AREA_OPERATIVA = NULL
    colnames(TABLA_S) = c("Anio","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_S
  })
  output$tablaS = renderDataTable({datatable(TABLA_S(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaS <- downloadButtonTable(TABLA_S())
  
  TABLA_C_PRE = reactive({
    TABLA_C_PRE = TABLA_3 %>% filter(VARIABLE == input$variable & anio == input$anio & mes == input$mes)
    TABLA_C_PRE = merge(TABLA_C_PRE,CNE_IDEAM[,c("CODIGO","latitud","longitud","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA")],by.x = "CodigoEstacion",by.y = "CODIGO",all.x = T)
    TABLA_C_PRE$AREA_OPERATIVA = substr(TABLA_C_PRE$AREA_OPERATIVA,1,17)
    TABLA_C_PRE = TABLA_C_PRE %>% filter(AREA_OPERATIVA == input$AO)
    TABLA_C_PRE = TABLA_C_PRE[,c("CodigoEstacion","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","latitud","longitud","anio","mes","VARIABLE","RANGO","CONTEO")]
    return(TABLA_C_PRE)
  })
  
  
  TABLA_C = reactive({
    TABLA_C = TABLA_C_PRE()
    TABLA_C$VARIABLE = NULL
    TABLA_C$AREA_OPERATIVA = NULL
    TABLA_C$anio = NULL
    TABLA_C$mes = NULL
    TABLA_C$latitud = NULL
    TABLA_C$longitud = NULL
    names(TABLA_C)[1] = "COD"
    TABLA_C
  })
  output$tabla3 = renderDataTable({datatable(TABLA_C(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla3 <- downloadButtonTable(TABLA_C())
  
#############################################################################################################################  
  

  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel4")
  })
  observeEvent(input$jumpToP5, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel5")
  })
  observeEvent(input$jumpToP6, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel6")
  })
  observeEvent(input$jumpToP7, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel7")
  })
  observeEvent(input$principal, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "Para acceder a cada uno de los módulos descritos, está la opción de dar clic en el botón que se encuentra encima de la descripción de cada módulo o también en las pestañas de la barra superior de la herramienta.",type = "info")
  })

  observeEvent(input$ayuda3, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, se ajusta la gráfica correspondiente a los porcentajes y datos digitados para la etiqueta seleccionada, por cada mes del año seleccionado y por Área Operativa. Toma la cantidad de estaciones con registros de la etiqueta en el mes del Área Operativa.",type = "info")
  })
  
  observeEvent(input$ayuda4, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, las tablas 1 y 2 describen las metas anuales por Área Operativa obtenidas con base al listado de las estaciones que las Áreas Operativas usan para enviar sus reportes por Intranet y el listado de las estaciones que reportaron datos de la etiqueta seleccionada en el Año seleccionado.",type = "info")
  })
  
  observeEvent(input$ayuda5, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, las tablas 1 y 2 describen el total de meses procesados del listado de estaciones que las Áreas Operativas usan para enviar sus reportes por Intranet y el listado de estaciones que reportan datos de la etiqueta seleccionada en el Año seleccionado respectivamente.",type = "info")
  })
  
  observeEvent(input$ayuda6, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, la gráfica (izquierda) y la tabla (derecha) resumen por Área Operativa los avances de digitación en DHIME, representado como la diferencia de los porcentajes de digitación por mes de dos descargas realizadas a la etiqueta seleccionada y para el año seleccionado.",type = "info")
  })
  
  observeEvent(input$ayuda7, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año, Mes y etiqueta DHIME, la gráfica representa la cantidad de registros de la etiqueta seleccionadas que fueron analizados en el proceso del cálculo de los avances.\n Los registros de las descargas usadas y se encuentran agrupados por Área Operativa.",type = "info")
  })
  
  observeEvent(input$ayuda8, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir del filtro de Estación, se selecciona el código de la estación de interés y se ajusta el mapa de la izquierda con la ubicación de la estación y la Tabla Anual de la derecha con la cantidad de registros por año para cada etiqueta de DHIME disponible en la estacion.\n La tabla mensual que se encuentra en la segunda pestaña a la derecha, muestra la misma dinámica que la primera tabla pero por mes y se corresponde con el filtro de Mes.",type = "info")
  })
  
  observeEvent(input$ayuda20, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "PRUEBA !!!",type = "info")
  })
  
  observeEvent(input$ayuda23, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","Con los filtros de departamento y de etiqueta DHIME, se filtra de la pestaña de ´Dashboard´, el mapa de ubicación de las estaciones asi como la gráfica de distribución de las estaciones agrupadas por subregiones del departamento y/o por municipios del departamento. Temporalmente la gráfica de series de las estaciones dle conjunto se encuentra estática. Finalmente, debajo de los filtros se encuentra una barra para filtrar las estaciones por porcentaje de completitud de sus series.",type = "info")
  })
  
  observeEvent(input$ayuda24, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","La tabla contiene información de las estaciones filtradas por etiqueta DHIME, Año, Mes y Área Operativa con contenido relacionado con la cantidad y porcentaje de datos por descarga para cada estación, asi como un campo con el cálculo del Avance que es el cálculo del porcentaje diferencial de ambos porcentajes de datos ya mencionados.",type = "info")
  })
  
  observeEvent(input$ayuda28, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","Con el filtro de 'Área Operativa' se puede depurar la tabla con las estaciones con probables coordenadas erróneas del Área Operativa de interés. A la derecha se encuentra un contador de estaciones activas, suspendidas y en mantenimiento",type = "info")
  })
  
  observeEvent(input$ayuda29, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","El filtro de 'Código' se actualiza de acuerdo al filtro 'Área Operativa' que se encuentra arriba.",type = "info")
  })
  
  observeEvent(input$ayuda30, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","La tabla contiene información de la cantidad de datos por mes para cada etiqueta de DHIME y para cada año desde el año 2010. Con los filtros de 'Área Operativa', 'Etiqueta DHIME', y 'Año' se ajusta el contenido de la tabla al momento de su visualización y de su descarga con el botón 'Download'.",type = "info")
  })
  

  
  RESUMEN_ORG_PREC_AUT = reactive({
    RESUMEN_ORG_H = RESUMEN_ORG_PT_AUT10 %>% filter(anio == input$anio3)
    RESUMEN_ORG_H$AREA_OPERATIVA = paste0("AO ",substr(RESUMEN_ORG_H$AREA_OPERATIVA,16,17))
    return(RESUMEN_ORG_H)
  })
  
  RESUMEN_ORG_NV_AUT = reactive({
    RESUMEN_ORG_D = RESUMEN_ORG_NV_AUT_60 %>% filter(anio == input$anioNV)
    RESUMEN_ORG_D$AREA_OPERATIVA = paste0("AO ",substr(RESUMEN_ORG_D$AREA_OPERATIVA,16,17))
    return(RESUMEN_ORG_D)
  })
  
  
  output$tabla = renderDataTable({
    TABLA_A = TABLA_1 %>% filter(anio == input$anio & VARIABLE == input$variable)
    TABLA_A$VARIABLE = NULL
    TABLA_A$anio = NULL
    TABLA_A
  })

  dptos = c("Choco" = "Chocó","Antioquia" = "Antioquia","Cordoba" = "Córdoba","Atlantico" = "Atlantico","Sucre" = "Sucre","Bolivar" = "Bolívar","Magdalena" = "Magdalena",
            "LaGuajira" = "La Guajira","Cesar" = "Cesar","NorteDeSantander" = "Norte de Santander","SanAndres" = "Archipiélago de San Andres, Providencia y Santa Catalina",
            "Huila" = "Huila","Cauca" = "Cauca","Tolima" = "Tolima","Cundinamarca" = "Cundinamarca","Bogota" = "Bogotá","Caldas" = "Caldas","Boyaca" = "Boyacá","Santander" = "Santander",
            "ValledelCauca" = "Valle del Cauca","Quindio" = "Quindío","RIsaralda" = "RIsaralda","Guaviare" = "Guaviare","Guainia" = "Guainía","Meta" = "Meta","Vichada" = "Vichada","Casanare" = "Casanare",
            "Arauca" = "Arauca","Vaupes" = "Vaupes","Putumayo" = "Putumayo","Caqueta" = "Caquetá","Amazonas" = "Amazonas","Narino" = "Nariño")
  
  
  mesesB = c("Enero" = "01","Febrero" = "02","Marzo" = "03","Abril" = "04",
            "Mayo" = "05","Junio" = "06","Julio" = "07","Agosto" = "08",
            "Septiembre" = "09","Octubre" = "10","Noviembre" = "11","Diciembre" = "12")
  
  AreasOpAlt = c("01 - Medellin" = "Area Operativa 01","02 - Barranquilla" = "Area Operativa 02","03 - Villavicencio" = "Area Operativa 03","04 - Neiva" = "Area Operativa 04",
                 "05 - Santa Marta" = "Area Operativa 05","06 - Duitama" = "Area Operativa 06","07 - Pasto" = "Area Operativa 07","08 - Bucaramanga" = "Area Operativa 08",
                 "09 - Cali" = "Area Operativa 09","10 - Ibague" = "Area Operativa 10","11 - Bogota" = "Area Operativa 11")
  

  output$text12 <- renderText({
    paste("Porcentaje de digitación de datos de ",input$variablePorc," por Área Operativa en el ",input$anioPorc)
  })
  output$text13 <- renderText({
    paste("Metas anuales por área Operativa con las estaciones que tuvieron registros de ",input$variablePorc,"  en el año ",input$anioPorc)
  })
  output$text14 <- renderText({
    paste("Metas anuales por Área Operativa con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10) para la etiqueta ",input$variablePorc,"  en el año ",input$anioPorc)
  })
  output$text15 <- renderText({
    paste("Meses procesados por Área Operativa a escala mensual con las estaciones que tuvieron registros de ",input$variablePorc," en el año ",input$anioPorc)
  })
  output$text16 <- renderText({
    paste("Meses procesados por Área Operativa a escala mensual con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10) para la etiqueta ",input$variablePorc," en el año ",input$anioPorc)
  })
  output$textNN <- renderText({
    paste("Cantidad de registros en el mes de ",names(mesesB)[mesesB == input$mesB]," del año ",input$anio2," para las últimas dos descargas de la etiqueta ",input$variableX)
  })
  
  output$tabla2 = renderDataTable({
    
    TABLA_B = TABLA_2 %>% filter(anio == input$anio & VARIABLE == input$variable)
    TABLA_B$VARIABLE = NULL
    TABLA_B$anio = NULL
    TABLA_B$AO = paste0("AO ",substr(TABLA_B$AO,16,17))
    TABLA_B
  })
  
  
  TABLA_P = reactive({
    TABLA_I = TABLA_META %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_I$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_I
  }) 
  output$tablaMeta = renderDataTable({datatable(TABLA_P(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMeta <- downloadButtonTable(TABLA_P())
  
  TABLA_OK = reactive({
    TABLA_H = TABLA_META_MES %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_H$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_H
  })
  output$tablaMetaMes = renderDataTable({datatable(TABLA_OK(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMetaMes <- downloadButtonTable(TABLA_OK())
  
  TABLA_W = reactive({
    TABLA_R = TABLA_META_2 %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_R$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_R
  })
  output$tablaMetaBeta = renderDataTable({datatable(TABLA_W(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMetaBeta <- downloadButtonTable(TABLA_W())
  
  TABLA_GG = reactive({
    TABLA_U = TABLA_META_MES_2 %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_U$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_U
  })
  output$tablaMetaBetaMes = renderDataTable({datatable(TABLA_GG(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMetaBetaMes <- downloadButtonTable(TABLA_GG())

  
  # Se genera la grafica del resumen de cantidad de estaciones de la etiqueta PT_AUT_10 por Areas Operativas
  output$graficoPrecAut = renderPlotly({
    fig <- plot_ly(RESUMEN_ORG_PREC_AUT(), x = ~AREA_OPERATIVA, y = ~COMPLETO, type = 'bar', name = 'COMPLETO',text = ~paste0("Porcentaje: ",round((COMPLETO/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig <- fig %>% add_trace(y = ~periodoA, name = '10 meses - 1 anio',text = ~paste0("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig <- fig %>% add_trace(y = ~periodoB, name = '8 meses - 10 meses',text = ~paste0("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig <- fig %>% add_trace(y = ~periodoC, name = '6 meses - 8 meses',text = ~paste0("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig <- fig %>% add_trace(y = ~periodoD, name = '4 meses - 6 meses',text = ~paste0("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig <- fig %>% add_trace(y = ~periodoE, name = '< 4 meses',text = ~paste0("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
  })
  
  # Se genera la grafica del resumen de cantidad de estaciones de la etiqueta NV_AUT_60 por Areas Operativas
  output$graficoNVAut = renderPlotly({
    fig <- plot_ly(RESUMEN_ORG_NV_AUT(), x = ~AREA_OPERATIVA, y = ~COMPLETO, type = 'bar', name = 'COMPLETO',text = ~paste0("Porcentaje: ",round((COMPLETO/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig <- fig %>% add_trace(y = ~periodoA, name = '10 meses - 1 anio',text = ~paste0("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig <- fig %>% add_trace(y = ~periodoB, name = '8 meses - 10 meses',text = ~paste0("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig <- fig %>% add_trace(y = ~periodoC, name = '6 meses - 8 meses',text = ~paste0("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig <- fig %>% add_trace(y = ~periodoD, name = '4 meses - 6 meses',text = ~paste0("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig <- fig %>% add_trace(y = ~periodoE, name = '< 4 meses',text = ~paste0("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
  })

  
  TABLA_E = reactive({
    TABLA_E = TABLA_5 %>% filter(anio == input$anio2 & variable == input$variableX)
    TABLA_E$variable = NULL
    TABLA_E$anio = NULL
    TABLA_E$AO = substr(TABLA_E$AO,16,17)
    TABLA_E
  })
  output$tabla5 = renderDataTable({datatable(TABLA_E(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla5 <- downloadButtonTable(TABLA_E())

  VARIACIONES_1 = reactive({
    MM = GRAF_1_AVANCE
    unionTemp = MM %>% filter(variable == input$variableX & anio == input$anio2)
    unionTemp = unionTemp[order(unionTemp$mes),]
    return(unionTemp)
  })
  
  output$grafico3 = renderPlotly({
    graph = plot_ly(VARIACIONES_1(),x = ~mes,y = ~AO01_dif,name = 'AO <b>01</b>',text = ~paste0("Conteo datos penúltima descarga:",AO01_conteoDatosMesAnt,"\n Conteo datos última descarga:",AO01_conteoDatosMesAct),type = 'scatter',mode = 'lines+markers') %>%
      add_trace(y = ~AO02_dif,name = 'AO <b>02</b>',text = ~paste0("Conteo datos descarga anterior:",AO02_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO02_conteoDatosMesAct)) %>%
      add_trace(y = ~AO03_dif,name = 'AO <b>03</b>',text = ~paste0("Conteo datos descarga anterior:",AO03_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO03_conteoDatosMesAct)) %>%
      add_trace(y = ~AO04_dif,name = 'AO <b>04</b>',text = ~paste0("Conteo datos descarga anterior:",AO04_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO04_conteoDatosMesAct)) %>%
      add_trace(y = ~AO05_dif,name = 'AO <b>05</b>',text = ~paste0("Conteo datos descarga anterior:",AO05_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO05_conteoDatosMesAct)) %>%
      add_trace(y = ~AO06_dif,name = 'AO <b>06</b>',text = ~paste0("Conteo datos descarga anterior:",AO06_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO06_conteoDatosMesAct)) %>%
      add_trace(y = ~AO07_dif,name = 'AO <b>07</b>',text = ~paste0("Conteo datos descarga anterior:",AO07_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO07_conteoDatosMesAct)) %>%
      add_trace(y = ~AO08_dif,name = 'AO <b>08</b>',text = ~paste0("Conteo datos descarga anterior:",AO08_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO08_conteoDatosMesAct)) %>%
      add_trace(y = ~AO09_dif,name = 'AO <b>09</b>',text = ~paste0("Conteo datos descarga anterior:",AO09_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO09_conteoDatosMesAct)) %>%
      add_trace(y = ~AO10_dif,name = 'AO <b>10</b>',text = ~paste0("Conteo datos descarga anterior:",AO10_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO10_conteoDatosMesAct)) %>%
      add_trace(y = ~AO11_dif,name = 'AO <b>11</b>',text = ~paste0("Conteo datos descarga anterior:",AO11_conteoDatosMesAnt,"\n Conteo aÃÂ±o anterior:",AO11_conteoDatosMesACt)) %>%
      layout(yaxis = list(title = 'Diferencia de porcentajes (%)'))
  })
  
  PORCENTAJES = reactive({
    RR = TABLA_2S
    RR = RR %>% filter(VARIABLE == input$variablePorc & anio == input$anioPorc)
    return(RR)
  })
  
  output$graficoPorc = renderPlotly({
    graph = plot_ly(PORCENTAJES(),x = ~AO,y = ~ENE,name = 'ENE',text = ~paste0("Datos digitados:",ENEconteo),type = 'scatter',mode = 'lines+markers') %>%
      add_trace(y = ~FEB,name = 'FEB',text = ~paste0("Datos digitados:",FEBconteo)) %>%
      add_trace(y = ~MAR,name = 'MAR',text = ~paste0("Datos digitados:",MARconteo)) %>%
      add_trace(y = ~ABR,name = 'ABR',text = ~paste0("Datos digitados:",ABRconteo)) %>%
      add_trace(y = ~MAY,name = 'MAY',text = ~paste0("Datos digitados:",MAYconteo)) %>%
      add_trace(y = ~JUN,name = 'JUN',text = ~paste0("Datos digitados:",JUNconteo)) %>%
      add_trace(y = ~JUL,name = 'JUL',text = ~paste0("Datos digitados:",JULconteo)) %>%
      add_trace(y = ~AGO,name = 'AGO',text = ~paste0("Datos digitados:",AGOconteo)) %>%
      add_trace(y = ~SEP,name = 'SEP',text = ~paste0("Datos digitados:",SEPconteo)) %>%
      add_trace(y = ~OCT,name = 'OCT',text = ~paste0("Datos digitados:",OCTconteo)) %>%
      add_trace(y = ~NOV,name = 'NOV',text = ~paste0("Datos digitados:",NOVconteo)) %>%
      add_trace(y = ~DIC,name = 'DIC',text = ~paste0("Datos digitados:",DICconteo)) %>%
      layout(yaxis = list(title = 'Porcentajes de digitación (%)'))
    
  })
  
  AVANCES_EST = reactive({
    AVANC = AVANCE_ESTACION %>% filter(etiqueta == input$variableX & anio == input$anio2 & mes == names(mesesB)[mesesB == input$mesB])
    AVANC = merge(AVANC,CNE_IDEAM[,c("CODIGO","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","ESTADO")],by.x = "Codigo",by.y = "CODIGO",all.x = T)
    AVANC$AREA_OPERATIVA = substr(AVANC$AREA_OPERATIVA,1,17)
    AVANC = AVANC %>% filter(AREA_OPERATIVA == input$AO_ModAvances)
    AVANC = AVANC[,c("Codigo","anio","mes","etiqueta","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","ESTADO","conteoDatosAnt","conteoDatosAct","PorcAnt(%)","PorcAct(%)","Avance(%)")]
    return(AVANC)
  })
  
  output$tablaAvancesEst = renderDataTable({datatable(AVANCES_EST(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaAvancesEst <- downloadButtonTable(AVANCES_EST())
  
  VARIACIONES_2 = reactive({
    NN = GRAF_2_AVANCE
    
    consolidadoConteoTemp = NN %>% filter(variable == input$variableX & anio == input$anio2 & mes == input$mesB)
    consolidadoConteoTemp$AO = paste0("AO_",substr(consolidadoConteoTemp$AO,16,17))
    
    return(consolidadoConteoTemp)
    
  })
  
  output$grafico4 = renderPlotly({
    graph2 =  plot_ly(VARIACIONES_2(),x = ~AO,y = ~conteoDatosMesAnt, type = 'bar', textposition = 'auto',name = "Cantidad datos descarga anterior", text = ~paste0("% Diferencia: ",avanceV2," %")) %>%
      add_trace(y = ~conteoDatosMesAct, textposition = 'auto',name = "Cantidad datos descarga actual", text = ~paste0("% Diferencia: ",avanceV2," %")) %>%
      layout(yaxis = list(title = 'Cantidad de datos'),legend = list(orientation = 'h',y = 1.13))
  })
  
  output$data = renderText({
    print(paste0("Número de Area Operativa: ",input$selAO ))
  })
  
  # Se incluye la vista de leaflet con la capa de las estaciones
  output$map = renderLeaflet({
    tre<-subset(CNE_IDEAM,CNE_IDEAM$CODIGO==input$selEstacion)
    # Graficamos el mapa resultante
    leaflet() %>%
      # addProviderTiles("Stamen.Toner") %>%
      addTiles() %>%
      # addPolygons() %>%
      addCircleMarkers(data=tre,lng=tre$longitud,lat=tre$latitud,label = tre$nombre,color="#0C00FF",weight = 3,radius=10,layerId = tre$OBJECTID) 
  })


  
  
  output$mapAvances = renderLeaflet({
    
    AVANCS = AVANCES_EST()
    AVANCS = merge(AVANCS,CNE_IDEAM[,c("CODIGO","latitud","longitud")],by.x = "Codigo",by.y = "CODIGO",all.x = T)
    colnames(AVANCS) = c("Codigo","anio","mes","etiqueta","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","ESTADO","conteoDatosAnt",
                         "conteoDatosAct","PorcAnt","PorcAct","Avance","latitud","longitud")
    AVANCS$RANGO = NA
    AVANCS$RANGO[which(AVANCS$Avance == 0)] = "0%"
    AVANCS$RANGO[which(AVANCS$Avance > 0 & AVANCS$Avance <= 5)] = "0% a 5%"
    AVANCS$RANGO[which(AVANCS$Avance < 0 & AVANCS$Avance >= -5)] = "-5% a 0%"
    AVANCS$RANGO[which(AVANCS$Avance > 5 & AVANCS$Avance <= 10)] = "5% a 10%"
    AVANCS$RANGO[which(AVANCS$Avance < -5 & AVANCS$Avance >= -10)] = "-10% a -5%"
    AVANCS$RANGO[which(AVANCS$Avance > 10)] = "> 10%"
    AVANCS$RANGO[which(AVANCS$Avance < -10)] = "< -10%"
    
    pal = colorFactor(palette = c("#ff5e04","#fbac05","#ffe38b","#000000","#c8f4ff","#0479ff","#0505ff"),
                      levels = c("< -10%","-10% a -5%","-5% a 0%","0%","0% a 5%","5% a 10%","> 10%"))
    
    leaflet(AVANCS) %>%
      addTiles() %>%
      addCircleMarkers(lng=AVANCS$longitud,lat=AVANCS$latitud,color = ~pal(RANGO),opacity = 0.8, radius = 6,label =paste("codigo:", AVANCS$Codigo, "<br>",
                                                                                                          "nombre:", AVANCS$nombre, "<br>",
                                                                                                          "municipio:", AVANCS$MUNICIPIO, "<br>",
                                                                                                          "departamento:", AVANCS$DEPARTAMENTO, "<br>",
                                                                                                          "estado", AVANCS$ESTADO , "<br>",
                                                                                                          "cantidad_datos_anterior_descarga:", AVANCS$conteoDatosAnt , " datos <br>",
                                                                                                          "porcentaje_datos_anterior_descarga:", AVANCS$PorcAnt  , " % <br>",
                                                                                                          "cantidad_datos_ultima_descarga:", AVANCS$conteoDatosAct, " datos <br>",
                                                                                                          "porcentaje_datos_ultima_descarga:", AVANCS$PorcAct  , " % <br>",
                                                                                                          "Avance:", AVANCS$Avance, " % <br>") %>%
                         lapply(htmltools::HTML)
      ) %>% 
      
      leaflet::addLegend(pal = pal, values = AVANCS$RANGO, position = "topleft")

  })

  seriesHistA = reactive({
    seriesHist = analisisSeries %>% filter(DEPARTA == input$seleccionarDpto & etiquet == input$etiq)
    seriesHist$lngSerie = NA
    seriesHist$lngSerie[which(seriesHist$lngtdSr < 10)] = "< 10 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 10 & seriesHist$lngtdSr < 20)] = "10 - 20 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 20 & seriesHist$lngtdSr < 30)] = "20 - 30 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 30 & seriesHist$lngtdSr < 40)] = "30 - 40 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 40)] = "> 40 anios"
    seriesHist$AREA_OP = substr(seriesHist$AREA_OP,1,17)
    seriesHist = subset(seriesHist,seriesHist$porcTtl >= input$porcComplet[1] & seriesHist$porcTtl <= input$porcComplet[2])
    
    seriesHist = merge(seriesHist,CNE_CRUCE_DIVIPOLA,by.x = "MUNICIP",by.y = "MUNICIPIO",all.x = T)
    seriesHist = merge(seriesHist,MunicipioSubregion[,c("MPIO_CCNCT","COD_SUBREG","NOM_SUBREG")],by.x = "COD_CNMO",by.y = "MPIO_CCNCT",all.x = T)
    return(seriesHist)
  })
  
  seriesHistB = reactive({
    seriesHistBA = seriesHistA()
    seriesHistBA = seriesHistBA[,c("codigo","nombre","MUNICIP","AREA_OP","NOM_SUBREG","ESTADO","periodo","lngtdSr","porcTtl","cntddDt","FECHA_INSTALACION","FECHA_SUSPENSION")]
    colnames(seriesHistBA) = c("codigo","nombre","municipio","AREA_OPERATIVA","SUBREGION","ESTADO","periodo","longitudSerie","porcCompletitud","cantidadDatos","FECHA_INSTALACION","FECHA_SUSPENSION")
    return(seriesHistBA)
  })
  
  
  output$mapPrueba = renderLeaflet({

    seriesHist1 = seriesHistA() 
    SubRegs1 = SubRegs[SubRegs$COD_SUBREG %in% stringr::str_pad(unique(seriesHist1$COD_SUBREG),4,pad = "0"),]
    
    pal = colorFactor(palette = c("#73FFDF","#0070FF","#FFAA00","#4CE600","#FFFF00"),levels = c("< 10 anios","10 - 20 anios","20 - 30 anios","30 - 40 anios","> 40 anios"))
    
		m = leaflet() %>%
		  addProviderTiles("Esri.WorldImagery") %>% addProviderTiles(providers$Stamen.TonerLines) %>% addProviderTiles(providers$Stamen.TonerLabels) %>% 
		  addPolygons(data = SubRegs1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "#6CAB96", fillOpacity = 0.8,group = "Subregiones",label = ~ NOM_SUBREG) %>%
		  addCircleMarkers(data = seriesHist1,lng= ~ longitud,lat= ~ latitud,color = ~pal(lngSerie),group = "Estaciones", opacity = 0.8, radius = 5, fillOpacity = 0.8,label =paste("codigo:", seriesHist1$codigo, "<br>",
		                                                                                                                                                      "nombre:", seriesHist1$nombre, "<br>",
		                                                                                                                                                      "municipio:", seriesHist1$MUNICIP, "<br>",
		                                                                                                                                                      "estado", seriesHist1$ESTADO, "<br>",
		                                                                                                                                                      "periodo:", seriesHist1$periodo, "<br>",
		                                                                                                                                                      "longitud_serie:", seriesHist1$lngtdSr, " anios <br>",
		                                                                                                                                                      "cantidad_datos:", seriesHist1$cntddDt, "<br>",
		                                                                                                                                                      "porcentaje_completitud:", seriesHist1$porcTtl, "% <br>") %>% lapply(htmltools::HTML)) %>%
		  leaflet::addLegend(pal = pal, values = seriesHist1$lngSerie, position = "topleft") %>%
		  addLayersControl(
		    overlayGroups = c("Subregiones", "Estaciones"),
		    options = layersControlOptions(collapsed = FALSE)
		  )

  })
  
  output$BoxPlotSeries = renderPlotly({
    seriesHist2 = seriesHistA()
    
    if (seriesHist2$DEPARTA == "Amazonas" | seriesHist2$DEPARTA == "Arauca" | seriesHist2$DEPARTA == "Bogotá" |
        seriesHist2$DEPARTA == "Caquetá" |  seriesHist2$DEPARTA == "Casanare" | seriesHist2$DEPARTA == "Guainía" |
        seriesHist2$DEPARTA == "Guaviare" | seriesHist2$DEPARTA == "Meta" | seriesHist2$DEPARTA == "Putumayo" |
        seriesHist2$DEPARTA == "Vaupes" | seriesHist2$DEPARTA == "Vichada") {
      
      fig <- plot_ly(y = seriesHist2$lngtdSr,color = seriesHist2$MUNICIP, type = "box", boxpoints = "all", jitter = 0.6, pointpos = -2,
                     text = ~paste0("Nombre: ",seriesHist2$nombre,"\n Estado: ",seriesHist2$ESTADO,
                                    "\n Longitud serie : ",seriesHist2$lngtdSr," Años\nPorcentaje Completitud: ",seriesHist2$porcTtl,"%"),
                     width = 510, height = 430) %>%
        layout(#title = list(text = 'Comportamiento de longitudes de serie \nde estaciones activas\ndepartamento de Valle del Cauca',y = 1.6), 
          yaxis = list(title = '</b> Longitud de series (Años) </b>'), xaxis = list(title = '</b> Municipio </b>'),showlegend = FALSE)
      fig
    } else {
      fig <- plot_ly(y = seriesHist2$lngtdSr,color = seriesHist2$NOM_SUBREG, type = "box", boxpoints = "all", jitter = 0.6, pointpos = -2,
                     text = ~paste0("Nombre: ",seriesHist2$nombre,"\n Municipio: ",seriesHist2$MUNICIP,"\n Estado: ",seriesHist2$ESTADO,
                                    "\n Longitud serie : ",seriesHist2$lngtdSr," Años\nPorcentaje Completitud: ",seriesHist2$porcTtl,"%"),
                     width =550, height = 400) %>%
        layout(#title = list(text = 'Comportamiento de longitudes de serie \nde estaciones activas\ndepartamento de Valle del Cauca',y = 1.6), 
          yaxis = list(title = '</b> Longitud de series (Años) </b>'), xaxis = list(title = '</b> Subregión </b>'),showlegend = FALSE)
      fig
    }
  })
  output$myImage = renderImage({
    filename <- normalizePath(file.path('www/series',paste0(input$etiq,'-',names(dptos)[dptos == input$seleccionarDpto],'.png')))
    seriesB = seriesHistB()
    
    if (dim(seriesB)[1] <= 10) {sizeY = 500}
    else if (dim(seriesB)[1] > 10 & dim(seriesB)[1] <= 50) {sizeY = 800} 
    else if (dim(seriesB)[1] > 50 & dim(seriesB)[1] <= 100) {sizeY = 900} 
    else if (dim(seriesB)[1] > 100 & dim(seriesB)[1] <= 150) {sizeY = 1100} 
    else if (dim(seriesB)[1] > 150) {sizeY = 1200}
    
    list(src = filename,
         contentType = "www/series/png",
         width = 590,
         height = sizeY,
         alt = paste0(input$etiq,'-',input$seleccionarDpto,'.png'))
    
  }, deleteFile = FALSE)
  
  output$TableText = renderPrint({
    seriesHist2 = seriesHistA()
    table(seriesHist2$ESTADO)
  })
  
  output$tablaAnualX = ({
    renderDataTable({datatable(seriesHistB(),options = list(scrollX = T,pageLength = 10),rownames = F)})
  })
  output$downloadTabla15 <- downloadButtonTable(seriesHistB())

  
  listadoEstaciones = reactive({
    est_AO = CNE_IDEAMA %>% filter(AREA_OPERATIVA == input$AOSCoompleto1)
    listado = unique(conteoEtiquetas$codigo)
    listadoFiltrado = data.frame("estacion" = listado[which(listado %in% est_AO$CODIGO)],stringsAsFactors = F)
    return(listadoFiltrado)
  })

  observe({
    updateSelectInput(session, inputId = "selEstacion",label = "Seleccione código de la estación de interés", 
                      choices = c(listadoEstaciones()$estacion))
  })
  
  observe({
    updateSelectInput(session, inputId = "seleccionarDpto",label = "Seleccione el departamento de interés", 
                      choices = unique(c(analisisSeries$DEPARTA)))
  })
  
  observe({
    updateSelectInput(session, inputId = "seleccionarEstacion",label = "Seleccione código de la estación de interés", 
                      choices = c(LISTADOESTACIONESCONV$codigoEstacion))
  })
  
  listadoEstacionesB = reactive({
    est_AO = TABLA_88_CODS %>% filter(AREA_OPERATIVA == input$AOSCoompleto2)
    listadoFiltrado = data.frame("estacion" = est_AO$codigo,stringsAsFactors = F)
    return(listadoFiltrado)
  })
  
  observe({
    updateSelectInput(session, inputId = "selecEstacion",label = "Seleccione código de la estación de interés", 
                      choices = c(listadoEstacionesB()$estacion))
  })
  
  observe({
    updateSelectInput(session, inputId = "selecEstacionNV",label = "Seleccione código de la estación de interés", 
                      choices = c(LISTADOESTACIONESNV_AUT_60$codigoEstacion))
  })
  
  
  coordsMalas = reactive({
    CNE_UNION_A = CNE_UNION %>% filter(AREA_OPERATIVA == input$AOCoordsErroneas)
    return(CNE_UNION_A)
  })
  
  output$tablaCoordsMalas = ({
    renderDataTable({datatable(coordsMalas(),options = list(scrollX = T,pageLength = 10),rownames = F)})
  })
  output$downloadTabla27 <- downloadButtonTable(coordsMalas())
  
  observe({
    updateSelectInput(session, inputId = "selEstacionErroneas",label = "Seleccione código", 
                      choices = c(coordsMalas()$codigo))
  })
  
  output$TableText3 = renderPrint({
    coordsMalasZ = coordsMalas()
    table(coordsMalasZ$estadoCNE_2021)
  })
  
  output$mapCoordsErroneas = renderLeaflet({
    
    COORDSMALAS_A = coordsMalas()[,c("codigo","nombreCNE_2014","longitudCNE_2014","latitudCNE_2014","entidadCNE2014")]
    COORDSMALAS_A = COORDSMALAS_A %>% filter(codigo == input$selEstacionErroneas)
    COORDSMALAS_B = coordsMalas()[,c("codigo","nombreCNE_2021","longitudCNE_2021","latitudCNE_2021","entidadCNE2021","municipioCNE_2021","departamentoCNE_2021","estadoCNE_2021")]
    COORDSMALAS_B = COORDSMALAS_B %>% filter(codigo == input$selEstacionErroneas)
    
    leaflet() %>%
      addTiles(group = "Google Maps") %>%
      addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>% addProviderTiles(providers$Stamen.TonerLines,group = "WorldImagery") %>% addProviderTiles(providers$Stamen.TonerLabels,group = "WorldImagery") %>% 
      addCircleMarkers(data = COORDSMALAS_A,group = "Catalogo 2014",lng=COORDSMALAS_A$longitudCNE_2014,lat=COORDSMALAS_A$latitudCNE_2014,color = "#0917F1",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_A$codigo, "<br>",
                                                                                                                         "nombre:", COORDSMALAS_A$nombreCNE_2014, "<br>",
                                                                                                                         "longitud:", COORDSMALAS_A$longitudCNE_2014, "<br>",
                                                                                                                         "latitud:", COORDSMALAS_A$latitudCNE_2014, "<br>",
                                                                                                                         "entidad:", COORDSMALAS_A$entidadCNE2014, "<br>"
                                                                                                                         ) %>%
                         lapply(htmltools::HTML)
      ) %>% 
      addCircleMarkers(data = COORDSMALAS_B,group = "Catalogo 2021",lng=COORDSMALAS_B$longitudCNE_2021,lat=COORDSMALAS_B$latitudCNE_2021,color = "#F1093A",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_B$codigo, "<br>",
                                                                                                                                   "nombre:", COORDSMALAS_B$nombreCNE_2021, "<br>",
                                                                                                                                   "longitud:", COORDSMALAS_B$longitudCNE_2021, "<br>",
                                                                                                                                   "latitud:", COORDSMALAS_B$latitudCNE_2021, "<br>",
                                                                                                                                   "entidad:", COORDSMALAS_B$entidadCNE2021, "<br>",
                                                                                                                                   "municipio:", COORDSMALAS_B$municipioCNE_2021, "<br>",
                                                                                                                                   "departamento:", COORDSMALAS_B$departamentoCNE_2021, "<br>",
                                                                                                                                   "estado:", COORDSMALAS_B$estadoCNE_2021, "<br>"
      ) %>%
        lapply(htmltools::HTML)
      ) %>%
      addDrawToolbar(
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
      ) %>% 
      addMeasure(primaryLengthUnit="kilometers",activeColor = "#F41A0C",completedColor = "#F1A709",primaryAreaUnit = "sqmeters",
                 secondaryAreaUnit = "hectares") %>%
      addLayersControl(
        baseGroups = c("Google Maps","WorldImagery"),
        overlayGroups = c("Catalogo 2014", "Catalogo 2021"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet::addLegend("topright", 
                colors =c("#0917F1","#F1093A"),
                labels= c("Catalogo 2014","Catalogo 2021"),
                opacity = 1) %>%
      leafem::addMouseCoordinates() %>%
      # leafem::clip2sfc()
      addScaleBar()
    
  })
  
  output$mapAut = renderLeaflet({
    tre<-subset(CNE,CNE$CODIGO==input$selecEstacion)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data=tre,lng=tre$longitud,lat=tre$latitud,label = tre$nombre,color="#FF0017",weight = 3,radius=10,layerId = tre$OBJECTID)
  })
  
  output$mapAutNV_AUT_60 = renderLeaflet({
    treNV<-subset(CNE,CNE$CODIGO==input$selecEstacionNV)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data=treNV,lng=treNV$longitud,lat=treNV$latitud,label = treNV$nombre,color="#FF0017",weight = 3,radius=10,layerId = treNV$OBJECTID)
  })
  
  
  output$data2 = renderText({
        print(paste(CNE$nombre[CNE$CODIGO == input$selEstacion],"|",CNE$MUNICIPIO[CNE$CODIGO == input$selEstacion],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$selEstacion],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$selEstacion]))
      })
  output$data3 = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$selecEstacion],"|",CNE$MUNICIPIO[CNE$CODIGO == input$selecEstacion],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$selecEstacion],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$selecEstacion]))
  })
  output$dataNVAut = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$selecEstacionNV],"|",CNE$MUNICIPIO[CNE$CODIGO == input$selecEstacionNV],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$selecEstacionNV],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$selecEstacionNV]))
  })

  
  TABLA_D = reactive({
    TABLA_D = TABLA_4 %>% filter(CodigoEstacion == input$selEstacion)
    TABLA_D$anio = NULL
    TABLA_D$AREA_OPERATIVA = NULL
    TABLA_D$CodigoEstacion = NULL
    TABLA_D
  })
  output$tabla4 = renderDataTable({datatable(TABLA_D(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla4 <- downloadButtonTable(TABLA_D())

  TABLA_LL = reactive({
    TABLA_LL = TABLA_45 %>% filter(CodigoEstacion == input$selEstacion)
    TABLA_LL$anio = NULL
    TABLA_LL$AREA_OPERATIVA = NULL
    TABLA_LL$CodigoEstacion = NULL
    TABLA_LL
  })
  output$tabla45 = renderDataTable({datatable(TABLA_LL(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla45 <- downloadButtonTable(TABLA_LL())
  
  TABLA_F = reactive({
    TABLA_F = TABLA_Y %>% filter(CodigoEstacion == input$selEstacion & anio == input$anio4)
    TABLA_F$anio = NULL
    TABLA_F$AREA_OPERATIVA = NULL
    TABLA_F$CodigoEstacion = NULL
    TABLA_F
  })
  output$tablaY = renderDataTable({datatable(TABLA_F(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaY <- downloadButtonTable(TABLA_F())
  
  TABLA_H = reactive({
    TABLA_H = TABLA_8 %>% filter(Codigo == input$selecEstacion)
    TABLA_H$Codigo = NULL
    TABLA_H
  })
  output$tabla8 = renderDataTable({datatable(TABLA_H(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla8 <- downloadButtonTable(TABLA_H())

  TABLA_HH = reactive({
    TABLA_HH = TABLA_88 %>% filter(Codigo == input$selecEstacion)
    TABLA_HH$Codigo = NULL
    TABLA_HH
  })
  output$tabla88 = renderDataTable({datatable(TABLA_HH(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla88 <- downloadButtonTable(TABLA_HH())
  
  
  TABLA_MES_ORG = reactive({
    TABLA_YY = CONTEO_DATOS_MES_ORG %>% filter(AREA_OPERATIVA == input$AO_ModGeneral & etiqueta == input$variable_ModGeneral & anio == input$anio_ModGeneral)
    TABLA_YY$AREA_OPERATIVA = NULL
    TABLA_YY$etiqueta = NULL
    TABLA_YY
  })
  
  TABLA_MES_ALT = reactive({
    TABLA_XX = CONTEO_DATOS_MES_ALT %>% filter(AREA_OPERATIVA == input$AO_ModGeneral & etiqueta == input$variable_ModGeneral & anio == input$anio_ModGeneral)
    TABLA_XX$AREA_OPERATIVA = NULL
    TABLA_XX$etiqueta = NULL
    TABLA_XX
  })
  
  output$tablaMesOrig = renderDataTable({datatable(TABLA_MES_ORG(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMesOrig <- downloadButtonTable(TABLA_MES_ORG())
  
  output$tablaMesAlte = renderDataTable({datatable(TABLA_MES_ALT(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMesAlte <- downloadButtonTable(TABLA_MES_ALT())

  output$TableTextMesOrg = renderPrint({
    summMesOrg = TABLA_MES_ORG()
    table(summMesOrg$ESTADO)
  })
  
  
  TABLA_V = reactive({
    TABLA_V = TABLA8NV_AUT_60 %>% filter(Codigo == input$selecEstacionNV)
    TABLA_V$Codigo = NULL
    TABLA_V
  })
  output$tabla8NV_AUT_60 = renderDataTable({datatable(TABLA_V(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla8NV_AUT_60 <- downloadButtonTable(TABLA_V())
  
  TABLA_I = reactive({
    TABLA_I = TABLA_9 %>% filter(Codigo == input$selecEstacion & anio == input$anio5)
    TABLA_I$Codigo = NULL
    TABLA_I$anio = NULL
    TABLA_I
  })
  output$tabla9 = renderDataTable({datatable(TABLA_I(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla9 <- downloadButtonTable(TABLA_I())
  
  TABLA_KK = reactive({
    TABLA_KK = TABLA_9NV_AUT_60 %>% filter(Codigo == input$selecEstacionNV & anio == input$anio5NV_AUT_60)
    TABLA_KK$Codigo = NULL
    TABLA_KK$anio = NULL
    TABLA_KK
  })
  output$tabla9NV_AUT_60 = renderDataTable({datatable(TABLA_KK(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla9NV_AUT_60 <- downloadButtonTable(TABLA_KK())
  
  querySQLite = reactive({
    BDETemp = BDEstaciones[BDEstaciones$etiqueta == input$etiqSeriesTime & BDEstaciones$codigo == input$seleccionarEstacion,]
    conn <- dbConnect(RSQLite::SQLite(), paste0(RUTA,"sqlite/",BDETemp$sqliteDB))
    query = dbGetQuery(conn,paste0("SELECT * FROM valores WHERE Codigo = ",input$seleccionarEstacion,""))
    query$year = as.numeric(substr(query$Fecha,1,4))
    query = subset(query,query$year >= input$year[1] & query$year <= input$year[2])
    dbDisconnect(conn)
    return(query)
  })
  
  querySQLiteMes = reactive({
    queryTempMes = querySQLite()
    queryTempMes$mes = substr(queryTempMes$Fecha,6,7)
    queryTempMes$mesA = NA
    queryTempMes$mesA[queryTempMes$mes == "01"] = "01_ENE" ; queryTempMes$mesA[queryTempMes$mes == "02"] = "02_FEB"
    queryTempMes$mesA[queryTempMes$mes == "03"] = "03_MAR" ; queryTempMes$mesA[queryTempMes$mes == "04"] = "04_ABR"
    queryTempMes$mesA[queryTempMes$mes == "05"] = "05_MAY" ; queryTempMes$mesA[queryTempMes$mes == "06"] = "06_JUN"
    queryTempMes$mesA[queryTempMes$mes == "07"] = "07_JUL" ; queryTempMes$mesA[queryTempMes$mes == "08"] = "08_AGO"
    queryTempMes$mesA[queryTempMes$mes == "09"] = "09_SEP" ; queryTempMes$mesA[queryTempMes$mes == "10"] = "10_OCT"
    queryTempMes$mesA[queryTempMes$mes == "11"] = "11_NOV" ; queryTempMes$mesA[queryTempMes$mes == "12"] = "12_DIC"
    return(queryTempMes)
  })

  output$summarymonthText = renderPrint({
    aa = tidyr::spread(data = querySQLiteMes(),key = mesA,value = Valor)
    print(summary(aa[c("01_ENE","02_FEB","03_MAR","04_ABR","05_MAY","06_JUN","07_JUL","08_AGO","09_SEP","10_OCT","11_NOV","12_DIC")]))
  })
  
  
  querySQLiteOrd = reactive({
    queryTemp = querySQLite()
    queryTemp = queryTemp[,c("Fecha","Valor")]
    
    
    if (input$etiqSeriesTime == "PTPM_CON" | input$etiqSeriesTime == "TMX_CON" | input$etiqSeriesTime == "TMN_CON" |
        input$etiqSeriesTime == "TSSM_MEDIA_D" | input$etiqSeriesTime == "Q_MEDIA_D" | input$etiqSeriesTime == "RCAM_CON") {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d")
      SECUENCIA = data.frame("Fecha"=seq(min(queryTemp$Fecha,na.rm = T),max(queryTemp$Fecha,na.rm = T), by = '1 day'),stringsAsFactors = F)
    } else {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d %H:%M:%S")
      SECUENCIA = data.frame("Fecha"=seq(min(queryTemp$Fecha,na.rm = T),max(queryTemp$Fecha,na.rm = T), by = '1 hour'),stringsAsFactors = F) #
    }
    
    SECUENCIA$Fecha = as.character(SECUENCIA$Fecha)
    queryTemp$Fecha = as.character(queryTemp$Fecha)
    queryTemp = merge(queryTemp,SECUENCIA,by = "Fecha",all = T)
    
    if (input$etiqSeriesTime == "TSSM_CON" | input$etiqSeriesTime == "THSM_CON") {
      queryTemp$dia = as.numeric(substr(queryTemp$Fecha,12,13)) #
      queryTemp = queryTemp[queryTemp$dia %in% c(7,13,18),] #
      queryTemp$dia = NULL
    } else if (input$etiqSeriesTime == "BSHG_CON") {
      queryTemp$dia = as.numeric(substr(queryTemp$Fecha,12,13)) #
      queryTemp = queryTemp[queryTemp$dia %in% c(5:18),] #
      queryTemp$dia = NULL
    } else if (input$etiqSeriesTime == "NVLM_CON") {
      queryTemp$dia = as.numeric(substr(queryTemp$Fecha,12,13)) #
      queryTemp = queryTemp[queryTemp$dia %in% c(6,18),] #
      queryTemp$dia = NULL
    }
    
    if (input$etiqSeriesTime == "PTPM_CON" | input$etiqSeriesTime == "TMX_CON" | input$etiqSeriesTime == "TMN_CON" |
        input$etiqSeriesTime == "TSSM_MEDIA_D" | input$etiqSeriesTime == "Q_MEDIA_D" | input$etiqSeriesTime == "RCAM_CON") {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d")
    } else {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d %H:%M:%S")
    }
    
    xts_uno = xts::xts(queryTemp$Valor,order.by = queryTemp$Fecha,frequency = 365,tz = "GMT")
    return(xts_uno)
  })
  
  output$dygraphDashboard = renderDygraph({
    dygraph(data = querySQLiteOrd(), main = paste0("Serie de tiempo ",input$etiqSeriesTime), xlab = "Fecha", ylab = "Valor") %>% 
      dyRangeSelector()
  })
  
  output$BoxPlotMeses = renderPlotly({
    fig <- plot_ly(y = querySQLiteMes()$Valor,color = querySQLiteMes()$mesA, type = "box") %>%
      layout(#title = list(text = 'Comportamiento de longitudes de serie \nde estaciones activas\ndepartamento de Valle del Cauca',y = 1.6), 
        yaxis = list(title = '</b> Valor </b>'), xaxis = list(title = '</b> Mes </b>'),showlegend = FALSE)
    fig
  })
  
  output$titleDashboard = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$seleccionarEstacion],"|",CNE$MUNICIPIO[CNE$CODIGO == input$seleccionarEstacion],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$seleccionarEstacion],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$seleccionarEstacion]))
  })
  
  output$clientdataText = renderPrint({
    queryTemp2 = querySQLite()
    queryTemp2 = queryTemp2[,c("year","Valor")]
    print(summary(queryTemp2))
  })
}