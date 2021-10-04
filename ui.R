#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  # headerPanel("Saludes de Shiny"),
  # titlePanel(
  #   h1("Análisis de digitación de valores en DHIME por las Áreas Operativas", align = "center")
  #   ),
  navbarPage(id = "inTabset",
    windowTitle = "Seguimiento DHIME", #title for browser tab
    title = div(tags$a(img(src="FONDO_DHIME_2.png", height=55), href= "http://dhime.ideam.gov.co/webgis/home/"),
                style = "position: relative; top: -16px;"), # Navigation bar
    # theme = bslib::bs_theme(
    #   bg = "#F2F4F8",
    #   fg = "#000000",
    #   primary = "#F02F1F",
    #   base_font = font_google("Prompt"),
    #   code_font = font_google("JetBrains Mono")
    #   ),
    # HTML("Actualizacion:<br/>04/06/2021 19:46"),
    tabPanel(icon = icon("home"),title = strong("Principal"),
             fluidRow(
               column(1),
               column(10,
                      p(strong("Análisis de digitación de valores en DHIME por las Áreas Operativas"), style = "font-size:29px;text-align : center")
                      ),
               column(1,
                      botonAyuda("principal")
                      )
             ),
             br(),
             fluidRow(
               p("La herramienta propuesta facilita el análisis estadístico descriptivo por medio de gráficas, tablas y mapas para algunas de las etiquetas básicas de DHIME más representativas:
                 PTPM_CON, TMX_CON, TMN_CON, TSSM_CON, THSM_CON, TSSM_MEDIA_D, BSHG_CON, EVTE_CON, NIVEL_H, Q_MEDIA_D, RCAM_CON, NVLM_CON, CAUDAL_H, PT_AUT_10 y NV_AUT_60.","Los datos fueron descargados de las plataformas de DHIME, con periodos de series de tiempo que van desde el año",strong(" 2010")," hasta el", strong(" 4 de octubre de 2021"),". La herramienta se compone de 7 módulos lo cuáles se explican a continuación:",style="text-align:justify"),
               br()
               # p("La herramienta se compone de 5 módulos lo cuáles se explican a continuación:")
             ),
             fluidRow(
               column(4,
                      div(actionButton('jumpToP1',strong('Seguimiento Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Seguimiento Áreas Operativas: "),"A través de este módulo, se puede conocer el estado de digitación de las estaciones de cada Área Operativa del IDEAM. A partir de unos rangos de digitación propuestos, 
                        en escala anual y mensual se agrupan las estaciones, así cómo gráficas de barras apiladas y mapas, junto con tablas que resumen la cantidad de datos por rangos para cada estación asi,
                        cómo el porcentaje de estaciones y cantidad que se encuentra en cada rango propuesto.",style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP2',strong('Porcentaje digitación Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Porcentaje digitación Áreas Operativas: "),"A través de este módulo, se puede conocer el porcentaje de digitación de valores de las etiquetas de DHIME básicas por Área Operativa.
                        Además, se resumen en tablas las metas mensuales y anuales por Área Operativa asociadas con las estaciones con registros en el periodo de tiempo
                        de estudio y las estaciones que son reportadas en Intranet por algunas Áreas Operativas.",style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP3',strong('Avances Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Avances Áreas Operativas: "),"A través de este módulo, se puede apreciar los avances de digitación de algunas etiquetas de DHIME por parte de las Áreas Operativas.
                         A partir de la descarga realizada el ",strong("28 de septiembre de 2021")," y la última descarga del ", strong("4 de octubre de 2021")," se calculan diferencias que 
                         permiten asociar el estado de avance por cada Área Operativa.",style="text-align:justify")
                      )
             ),
             # br(),
             fluidRow(
               column(4,
                      div(actionButton('jumpToP4',strong('Sensores Convencionales'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Sensores Convencionales: "),"A través de este módulo, se pueden realizar consultas de información de ubicación, cantidad de datos mensual y anual 
                         de una estación en partícular con etiquetas de DHIME consideradas de estaciones convencionales." ,style="text-align:justify")

                      ),
               column(4,
                      div(actionButton('jumpToP5',strong('Sensores Automáticos'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Sensores Automáticos: "),"A través de este módulo, se pueden realizar consultas de información de ubicación, cantidad de datos mensual y anual
                         de una estación en partícular con etiquetas de DHIME consideradas de estaciones automáticas (las etiquetas disponibles en éste módulo son PT_AUT_10 y NV_AUT_60)." ,style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP6',strong('Series Históricas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Series Históricas: "),"A través de este módulo, se puede explorar por departamento y por variable, productos relacionados con las series históricas y ubicaciones de estaciones categorizadas por la longitud de series. Información acrtualizada hasta el mes de JULIO DE 2021." ,style="text-align:justify")
                      )
             ),
             fluidRow(
               column(4),
               column(4,
                      div(actionButton('jumpToP7',strong('Coordenadas Erróneas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Estaciones con Coordenadas Erróneas: "),"A través de este módulo, se identifican las estaciones con coordenadas erróneas, lo cual fueron comparadas con las estaciones del catálogo del año 2014. Se dispone de un mapa para que el usuario tome una decisión para decidir las coordenadas apropiadas para cada estación e informar al grupo de Planeación Operativa" ,style="text-align:justify")
                     ),
               column(4)
             ),
             fluidRow(
               hr(),
               p(em("Desarrollado por:"),em("Julián David Urrea Urrego (Ingeniero Ambiental | Especialista SIG)"),style="text-align:center; font-family: times"),
               p(em("CTO. 501-2020 | IDEAM-Planeación Operativa"),style="text-align:center; font-family: times")
               
             )
             ),
    

    
    navbarMenu(strong("Áreas Operativas"),
               tabPanel(strong(" Seguimiento "),value = "panel1",
                        fluidRow(
                          column(7,
                                 
                                 fluidRow(
                                   column(1,
                                          botonAyuda("preview")                                   ),
                                   column(1),
                                   column(4,
                                          filtroEtiquetasDHIME('variable', 'ETIQUETA DHIME')
                                   ),
                                   column(6,
                                          filtroAnios('anio', 'AÑO')
                                   )
                                 ),
                                 fluidRow(
                                   h4(strong(textOutput("text")), align = "center"),
                                   # h4("Análisis de cantidad de estaciones por rangos de días de digitación de datos anual para la etiqueta PTPM_CON", align = "center"),
                                   plotlyOutput('grafico',height = "330px")
                                 )
                                 
                          ),
                          column(5,
                                 selectInput('mes',
                                             label = "MES",
                                             choices = c("Enero" = 1,"Febrero" = 2,"Marzo" = 3,"Abril" = 4,
                                                         "Mayo" = 5,"Junio" = 6,"Julio" = 7,"Agosto" = 8,
                                                         "Septiembre" = 9,"Octubre" = 10,"Noviembre" = 11,"Diciembre" = 12)),
                                 h4(strong(textOutput("text2")), align = "center"),
                                 # h4("Análisis de cantidad de estaciones por rangos de días de digitación de datos mensual", align = "center"),
                                 plotlyOutput('grafico2',height = "330px")
                          )
                        ),
                        
                        fluidRow(
                          column(1,
                                 botonAyuda("ayuda2")
                          ),
                          #column(1),
                          column(4,
                                 filtroV1AreasOperativas('AO', "AREA OPERATIVA"),
                                 br()
                          )
                        ),
                        fluidRow(
                          column(7,
                                 tabsetPanel(
                                   tabPanel("Mapa Anual",
                                            h4(strong(textOutput("text3")), align = "center"),
                                            # div(tags$a(img(src="SimbologiaAnual.png", height=55,width= 800)),style = "position: relative; top: -4px",align ="center"),
                                            leafletOutput("mapAnual")
                                   ),
                                   tabPanel("Tabla Anual 1",
                                            setDownTable("text5","tablaAnual","downloadTablaAnual")
                                   ),
                                   tabPanel("Tabla Anual 2",
                                            setDownTable("text8","RESUMEN_ORG_PORC_MES","downloadRESUMEN_ORG_PORC_MES")
                                   ),
                                   tabPanel("Tabla Anual 3",
                                            setDownTable("text10","ConteoMes","downloadConteoMes")
                                   ),
                                   tabPanel("Tabla Anual 4",
                                            setDownTable("text7","tablaQ","downloadTablaQ")
                                   ),
                                   tabPanel("Tabla Anual 5",
                                            setDownTable("text9","tablaT","downloadTablaT")
                                   ),
                                   tabPanel("Tabla Anual 6",
                                            setDownTable("text11","tablaS","downloadTablaS")
                                   )
                                 )
                          ),
                          column(5,
                                 tabsetPanel(
                                   tabPanel("Mapa Mensual",
                                            h4(strong(textOutput("text4")), align = "center"),
                                            # div(tags$a(img(src="SimbologiaMes.png", height=55,width= 350)),style = "position: relative; top: -4px",align ="center"),
                                            leafletOutput("mapMensual")
                                   ),
                                   tabPanel("Tabla Mensual 1",
                                            setDownTable("text6","tabla3","downloadTabla3")
                                   )
                                 )
                          )
                        )
                        
                        ),
               tabPanel(strong("Porcentaje digitación"),value = "panel2",
                        fluidRow(
                          column(6,
                                 fluidRow(
                                   botonAyuda("ayuda3")
                                 ),
                                 fluidRow(
                                   column(2),
                                   column(4,
                                          filtroEtiquetasDHIME('variablePorc','ETIQUETA DHIME')
                                   ),
                                   column(4,
                                          filtroAnios('anioPorc', 'AÑO')
                                   ),
                                   column(2)
                                 ),
                                 h4(strong(textOutput("text12")), align = "center"),
                                 # h4("Porcentaje de digitación de datos por Área Operativa", align = "center"),
                                 plotlyOutput('graficoPorc'),
                                 br(),
                                 fluidRow(
                                   column(2),
                                   column(3,
                                          botonAyuda("ayuda5")
                                   )
                                 )
                          ),
                          column(6,
                                 fluidRow(
                                   column(10),
                                   column(1,
                                          botonAyuda("ayuda4")
                                   )
                                 ),
                                 tabsetPanel(
                                   tabPanel("Tabla 1",
                                            h4(strong(textOutput("text14")), align = "center"),
                                            # h4("Metas anuales por Área Operativa con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10)", align = "center"),
                                            div(dataTableOutput("tablaMetaBeta"),style = "font-size:70%")
                                   ),
                                   tabPanel("Tabla 2",
                                            h4(strong(textOutput("text13")), align = "center"),
                                            # h4("Metas anuales por Área Operativa con las estaciones que tuvieron registros en el año", align = "center"),
                                            div(dataTableOutput("tablaMeta"),style = "font-size:70%")
                                   )
                                 )
                          )
                          
                        ),
                        fluidRow(
                          column(1),
                          column(10,
                                 tabsetPanel(
                                   tabPanel("Tabla 1",
                                            h4(strong(textOutput("text16")), align = "center"),
                                            # h4("Meses procesados por Área Operativa a escala mensual con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10)", align = "center"),
                                            div(dataTableOutput("tablaMetaBetaMes"),style = "font-size:70%")
                                   ),
                                   tabPanel("Tabla 2",
                                            h4(strong(textOutput("text15")), align = "center"),
                                            # h4("Meses procesados por Área Operativa a escala mensual con las estaciones que tuvieron registros en el año", align = "center"),
                                            div(dataTableOutput("tablaMetaMes"),style = "font-size:70%")
                                   )
                                   
                                 )
                          ),
                          column(1)
                        )
                        ),
               tabPanel(strong("Avances"),value = "panel3",
                        fluidRow(
                          column(5,
                                 fluidRow(
                                   column(2),
                                   column(4,
                                          filtroEtiquetasDHIME('variableX','ETIQUETA DHIME')
                                   ),
                                   column(4,
                                          filtroAnios('anio2', 'AÑO')
                                   ),
                                   column(2)
                                 ),
                                 fluidRow(
                                   br(),
                                   br(),
                                   plotlyOutput('grafico3',height = "330px")
                                 )
                          ),
                          column(7,
                                 fluidRow(
                                   column(10),
                                   column(2,
                                          botonAyuda("ayuda6")                                   )
                                 ),
                                 setDownTable("Comparación de porcentajes de digitación de Áreas Operativas con descargas anteriores","tabla5","downloadTabla5",titleStatic = TRUE)
                          ),
                          fluidRow(
                            column(5,
                                   fluidRow(
                                     column(9,
                                            selectInput('mesB',
                                                        label = 'mes',
                                                        choices = c("Enero" = "01","Febrero" = "02",
                                                                    "Marzo" = "03","Abril" = "04",
                                                                    "Mayo" = "05","Junio" = "06",
                                                                    "Julio" = "07","Agosto" = "08",
                                                                    "Septiembre" = "09","Octubre" = "10",
                                                                    "Noviembre" = "11","Diciembre" = "12"))
                                     ),
                                     column(1,
                                            botonAyuda("ayuda7")
                                     )
                                   ),
                                   fluidRow(
                                     h4(strong(textOutput("textNN")), align = "center"),
                                     plotlyOutput('grafico4',height = "330px")
                                   )
                                   
                            ),
                            column(7,
                                   fluidRow(
                                     column(7,
                                            filtroV1AreasOperativas('AO_ModAvances', "AREA OPERATIVA")
                                            ),
                                     column(5,
                                            botonAyuda("ayuda24")
                                     )
                                     
                                   ),
                                   fluidRow(
                                     tabsetPanel(
                                       tabPanel(strong("Mapa"),
                                                leafletOutput("mapAvances")
                                                ),
                                       tabPanel(strong("Tabla"),
                                                div(dataTableOutput("tablaAvancesEst"),style = "font-size:70%"),
                                                downloadButton("downloadTablaAvancesEst")
                                                )
                                     )

                                   )
                            )
                          )
                        )
                        )
               ),
    navbarMenu(strong("Sensores Convencionales"),
               tabPanel(strong(" Sensores Convencionales "),value = "panel4",
                        tabsetPanel(
                          tabPanel(strong("Consulta por Estación"),
                                   fluidRow(
                                     column(4,
                                            fluidRow(
                                              column(9,
                                                     filtroV1AreasOperativas("AOSCoompleto1","ÁREA OPERATIVA"),
                                                     selectInput(inputId = "selEstacion",
                                                                 label = "Seleccione código de la estación de interés",
                                                                 choices = NULL)
                                              ),
                                              column(1,
                                                     botonAyuda("ayuda8")
                                              )
                                            ),
                                            # selectInput(inputId = "selEstacion",
                                            #             label = "Seleccione cÃÂÃÂÃÂÃÂ³digo de la estaciÃÂÃÂÃÂÃÂ³n de interÃÂÃÂÃÂÃÂ©s",
                                            #             choices = c(LISTADOESTACIONESCONV$codigoEstacion),multiple=F, selectize=FALSE),
                                            h4(strong("Ubicación de la estación"), align = "center"),
                                            leafletOutput("map")
                                     ),
                                     column(8,
                                            h3(strong(textOutput("data2")), align = "center"),
                                            br(),
                                            tabsetPanel(
                                              tabPanel(strong("Tabla Anual"),
                                                       br(),
                                                       tabsetPanel(
                                                         tabPanel(strong("Conteos"),
                                                                  setDownTable("Cantidad de datos o días de la variable de interés en DHIME por año","tabla4","downloadTabla4",titleStatic = TRUE)
                                                                  ),
                                                         tabPanel(strong("Porcentajes"),
                                                                  setDownTable("Cantidad de porcentajes de datos o días de la variable de interés en DHIME por año","tabla45","downloadTabla45",titleStatic = TRUE)
                                                                  )
                                                       )
                                              ),
                                              tabPanel(strong("Tabla Mensual"),
                                                       filtroAnios('anio4', 'AÑO'),
                                                       setDownTable("Cantidad de datos o días de la variable de interés en DHIME por mes","tablaY","downloadTablaY",titleStatic = TRUE)
                                              )
                                            )
                                            # plotOutput('graficoB')
                                            # dygraphOutput("dygraph1")
                                            
                                     )
                                   )
                                   ),
                          tabPanel(strong("Consulta General"),
                                   fluidRow(
                                            column(2,
                                                   br(),
                                                   filtroV1AreasOperativas('AO_ModGeneral', "AREA OPERATIVA")
                                                   ),
                                            column(2,
                                                   br(),
                                                   filtroEtiquetasDHIME('variable_ModGeneral','ETIQUETA DHIME')
                                                   ),
                                            column(2,
                                                   br(),
                                                   filtroAnios('anio_ModGeneral', 'AÑO')
                                                   ),
                                            column(3,
                                                   p(strong("Cantidad Estaciones:")),
                                                   verbatimTextOutput("TableTextMesOrg")
                                            ),
                                            
                                            column(1,
                                                   fluidRow(downloadButton("downloadTablaMesOrig",label = "DownloadTable1")),
                                                   br(),
                                                   fluidRow(downloadButton("downloadTablaMesAlte",label = "DownloadTable2"))
                                                   ),
                                            column(1),
                                            column(1,
                                                   br(),
                                                   br(),
                                                   botonAyuda("ayuda30")
                                                   )
                                            ),
                                   fluidRow(
                                     tabsetPanel(
                                       tabPanel(strong("Cantidad datos por mes"),
                                                div(dataTableOutput("tablaMesOrig"),style = "font-size:60%")
                                                ),
                                       tabPanel(strong("Cantidad dias con datos por mes"),
                                                div(dataTableOutput("tablaMesAlte"),style = "font-size:60%")
                                                )
                                     )
                                   )
                                   )
                        ),

                        
                        ),
               tabPanel(strong(" Sensores convencionales PRUEBA "),
                        dashboardPage(
                          dashboardHeader(disable = T),
                          dashboardSidebar(
                            selectInput(inputId = "seleccionarEstacion",
                                        label = shiny::span(strong("Seleccione código de la estación de interés"), style = "color:red"),#div("Seleccione código de la estación de interés",style = "color: #1C2322"),
                                        choices = NULL),
                            filtroEtiquetasDHIME('etiqSeriesTime','ETIQUETA DHIME'),
                            sliderInput("year","Data time period",
                                        min = 2010,
                                        max = 2021,
                                        value = c(2010,2021),sep = "")#,
                            # verbatimTextOutput("clientdataText")
                           
                            
                          ),
                          dashboardBody(
                            
                            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style_dashboard.css")),
                            fluidRow(box(dygraphOutput("dygraphDashboard")),
                                     box(plotlyOutput("BoxPlotMeses"))),
                            fluidRow(box(width=4,verbatimTextOutput("clientdataText")),
                                     box(width=8,verbatimTextOutput("summarymonthText"))),
                            # fluidRow(
                            #   column(11,
                            #          fluidRow(),
                            #          fluidRow()
                            #          ),
                            #   column(11,
                            #          fluidRow(),
                            #          fluidRow()
                            #          )),
                            fluidRow(
                              botonAyuda("ayuda20")
                            )
                          )
                        )

                        )
               ),
    navbarMenu(strong("Sensores Automáticos"),
               tabPanel(strong("PT_AUT_10"),value = "panel5",
                        fluidRow(
                          column(4,
                                 filtroV1AreasOperativas("AOSCoompleto2","ÁREA OPERATIVA"),
                                 selectInput(inputId = "selecEstacion",
                                             label = "Seleccione código de la estación de interés",
                                             choices = NULL),
                                 h4(strong("Ubicación de la estación"), align = "center"),
                                 leafletOutput("mapAut"),
                                 filtroAnios('anio3', 'AÑO'),
                                 h4("Análisis de cantidad de estaciones por rangos de días de digitación de datos anual", align = "center"),
                                 plotlyOutput('graficoPrecAut')
                                 ),
                          column(8,
                                 tabsetPanel(
                                   tabPanel(strong("TABLA ANUAL"),
                                            br(),
                                            tabsetPanel(
                                              tabPanel(strong("Conteos"),
                                                       h5(textOutput("data3"), align = "center"),
                                                       setDownTable("Cantidad de días con valores de PT_AUT_10 en DHIME por año","tabla8","downloadTabla8",titleStatic = TRUE)
                                                       ),
                                              tabPanel(strong("Porcentajes"),
                                                       setDownTable("Porcentaje de días con valores de PT_AUT_10 en DHIME por año","tabla88","downloadTabla88",titleStatic = TRUE)
                                                       )
                                            )
                                   ),
                                   tabPanel(strong("TABLA MENSUAL"),
                                            filtroAnios('anio5', 'AÑO'),
                                            setDownTable("Cantidad de días con valores de PT_AUT_10 en DHIME por mes","tabla9","downloadTabla9",titleStatic = TRUE)
                                   )
                                 )
                                 
                                 
                          )
               )
               ),
               tabPanel(strong("NV_AUT_60"),
                        fluidRow(
                          column(4,
                                 fluidRow(
                                   p("A través de este módulo, se pueden realizar consultas de información de ubicación, cantidad de datos mensual y anual de la etiqueta NV_AUT_60
                                     para una estación automática en partícular" ,style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")
                                   ),
                                 selectInput(inputId = "selecEstacionNV",
                                             label = "Seleccione código de la estación de interés",
                                             choices = NULL),
                                 h4(strong("Ubicación de la estación"), align = "center"),
                                 leafletOutput("mapAutNV_AUT_60"),
                                 filtroAnios('anioNV', 'AÑO'),
                                 h4("Análisis de cantidad de estaciones por rangos de días de digitación de datos anual", align = "center"),
                                 plotlyOutput('graficoNVAut')
                                 ),
                          column(8,
                                 h5(textOutput("dataNVAut"), align = "center"),
                                 setDownTable("Cantidad de días con valores de NV_AUT_60 en DHIME por año","tabla8NV_AUT_60","downloadTabla8NV_AUT_60",titleStatic = TRUE),
                                 filtroAnios('anio5NV_AUT_60', 'AÑO'),
                                 setDownTable("Cantidad de días con valores de NV_AUT_60 en DHIME por mes","tabla9NV_AUT_60","downloadTabla9NV_AUT_60",titleStatic = TRUE)
                          )
               )
               )
      
    ),
    
    tabPanel(strong("Series Históricas"),value = "panel6",
             dashboardPage(
               dashboardHeader(disable = T),
               dashboardSidebar(
                 botonAyuda("ayuda23"),
                 selectInput(inputId = "seleccionarDpto",
                             label = "Seleccione el departamento de interés",
                             choices = NULL),
                 filtroEtiquetasDHIME('etiq','ETIQUETA DHIME'),
                 sliderInput("porcComplet","Porcentaje de completitud series",
                             min = 0,
                             max = 100,
                             value = c(0,100),sep = "")
               ),
               dashboardBody(
                 
                 tabsetPanel(
                   tabPanel(strong("Dashboard"),
                            fluidRow(
                              box("MAPA",leafletOutput("mapPrueba")),
                              box(plotlyOutput("BoxPlotSeries"))
                            ),
                            fluidRow(column(7,
                                            div(tags$a(img(src="leyendaEstaciones.png", height=30,width= 550)),style = "position: relative; top: -4px",align ="center")
                            )
                            ),
                            fluidRow(
                              box(imageOutput("myImage",height = 1600),width = 7,collapsible = TRUE),
                              box(verbatimTextOutput("TableText"),width = 5)
                            )
                            
                   ),
                   tabPanel(strong("Tabla"),
                            downloadButton("downloadTabla15"),
                            box(div(dataTableOutput("tablaAnualX"),style = "font-size:70%"),width = 12)
                            
                   )
                 )
                 
                 
               )
             )
             
    ),
    
    tabPanel(strong("Coordenadas Erróneas"),value = "panel7",
             fluidRow(
               column(2,
                      botonAyuda("ayuda28"),
                      br(),
                      filtroV1AreasOperativas('AOCoordsErroneas', "AREA OPERATIVA")
               ),
               column(3,
                      p(strong("Cantidad Estaciones:")),
                      verbatimTextOutput("TableText3")
                      ),
               column(7,
                      p(strong("La tabla enlista las estaciones con diferencias de coordenadas en los catálogos de los años 2014 y 2021. Además de la información básica de cada estación en estas versiones de 
                               catálogos, los últimos campos describen para cada estación la distancia en metros que hay entre sus distintas versiones de ubicaciones así como las orientaciones entre las mismas.") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:15px;border-radius:10px")
                      )
             ),
             tabsetPanel(
               tabPanel(strong("Catálogo estaciones con coordenadas erróneas"),
                        tabPanel(
                          strong("Tabla"),
                          box(div(dataTableOutput("tablaCoordsMalas"),style = "font-size:70%"),width = 12),
                          downloadButton("downloadTabla27")
                        )
               ),
               tabPanel(strong("MAPA"),
                        # fluidRow(
                        #   column(7),
                        #   column(3,
                        #          selectInput(inputId = "selEstacionErroneas",
                        #                      label = "Seleccione código de la estación de interés",
                        #                      choices = NULL)
                        #          ),
                        #   column(2)
                        # ),
                        fluidRow(
                          column(5,
                                 br(),
                                 p(strong("A partir del filtro de códigos de estaciones que está encima del mapa, el mapa se actualiza con la estación seleccionada. Aparecerán dos ubicaciones asociadas a la estación 
                                   para el Catálogo del 2014 (",shiny::span(strong("AZUL"), style = "color:blue"),") y el Catálogo del 2021 (",shiny::span(strong("ROJO"), style = "color:red"),"). Al pasar el cursor del mouse sobre 
                                   el lienzo del mapa, aparecerá en la esquina superior derecha del mapa las coordenadas del mapa en donde se encuentra el mouse sin embargo, con Ctrl + Click  también obtendrá
                                   las coordenadas en el portapapeles para que pegue el texto de las coordenadas en otro lado.") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:10px;border-radius:10px"),
                                 p(strong("El mapa también presenta herramientas relacionadas con la medición de distancias (Esquina superior Derecha), y dibujo de polígonos en el mapa (Izquierda) junto con un Menú 
                                          para prender o apagar las capas y los mapas base (Google Maps e Imágenes de Satélite) que están en la Derecha del mapa. ¡¡ Ayudará para la toma de decisiones al momento de 
                                          escoger unas adecuadas coordenadas!!") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:10px;border-radius:10px"),
                                 p(strong("Finalmente, puede informar al grupo de Planeación Operativa o por Mesa de Ayuda la propuesta de nuevas coordenadas para la estación de interés, así se evaluará desde
                                          el grupo y se actualizará el Catálogo de Estaciones del IDEAM del 2021 con esas nuevas coordenadas.") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:10px;border-radius:10px")
                                 ),
                          column(7,
                                 fluidRow(
                                   column(4),
                                   column(4,
                                          selectInput(inputId = "selEstacionErroneas",
                                                      label = "Seleccione estación",
                                                      choices = NULL)
                                          ),
                                   column(2),
                                   column(2,
                                          botonAyuda("ayuda29")
                                   )
                                 ),
                                 leafletOutput("mapCoordsErroneas")
                                 )
                        )
               )
             )
    )

  ),
  
  
  div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
  ###############################################.             
  ##############Footer----    
  ###############################################.
  #Copyright warning
  tags$footer( "© Instituto de Hidrología, Meteorología y Estudios Ambientales (Grupo de Planeación Operativa) v5.0 2021", 
              style = "
              position:fixed;
              text-align:center;
              left: 0;
              bottom:0;
              width:100%;
              z-index:1000;  
              height:30px; /* Height of the footer */
              color: white;
              padding: 5px;
              font-weight: bold;
              background-color: #2C3E50"
  )
))