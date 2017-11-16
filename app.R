
####### Instalation des différents packets nécéssaires : 
    .list.of.packages <- c("shiny","shinydashboard","DT","fitdistrplus","CDVine","asbio","copula","VineCopula","shinyjs","HAC")
    .new.packages <- .list.of.packages[!(.list.of.packages %in% installed.packages()[,"Package"])]
    if(length(.new.packages)) install.packages(.new.packages)
    lapply(.list.of.packages,function(x){library(x,character.only=TRUE)}) 

    
    
    
############### UI
ui <- ui <- dashboardPage(
  
  dashboardHeader(title = "Estimtion de copules archimédiennes hierarchiques", disable=TRUE),
  dashboardSidebar(
    sidebarMenuOutput("Menu")
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName="InputData",
        fluidRow(
          column(3,h5(tags$b("1. Choississez un fichier CSV de données"))),
          column(3,h5(tags$b("2. Selectionnez les variables à modéliser"))),
          column(3,h5(tags$b("3. Renomez eventuellement les variables"))),
          column(3,h5(tags$b("4. Petit summary de vos données ")))
        ),
        fluidRow(
          column(3,fileInput(inputId = 'datafile',label = "",accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
          column(3,selectInput(inputId = "selectVariables",label="",choices = "",multiple=TRUE)),
          column(3,uiOutput(outputId = "renamer")),
          column(3,verbatimTextOutput("resume",placeholder = TRUE))
        ),
        h5("Vos données sont les suivantes :"),
        fluidRow(
          column(1),
          column(10,dataTableOutput("fileTable")),
          column(1)
        )
      ),
      tabItem(tabName="Margins",
        fluidRow(
          selectInput("selectCopToFit", 
                      label = "Type de copule a fitter", 
                      choices = c("HAC Gumbel",
                                  "AC Gumbel",
                                  "HAC Clayton",
                                  "AC Clayton",
                                  "HAC Franck",
                                  "AC Franck",
                                  "HAC Joe",
                                  "AC Joe",
                                  "HAC Ali-Mikhail-Haq",
                                  "AC Ali-Mikhail-Haq")
                      , selected = "HAC Gumbel"),
          verbatimTextOutput("valeur")
        )),
      tabItem(tabName="ChoixCopule")
    )
  )
)

####### Serveur
server <- function(input, output, session) {

  ##### Gestion du menu : 
  output$Menu <- renderMenu({
    menu = sidebarMenu(
      menuItem("Input des data", tabName="InputData", icon=icon("dashboard"))
    )
    
    if(!is.null(input$datafile)){
      menu = sidebarMenu(
        menuItem("Input des data", tabName="InputData", icon=icon("dashboard")),
        menuItem("Lois marginales", tabName="Margins", icon=icon("th")),
        menuItem("Copule archimédiennes hierarchique", tabName="ChoixCopule", icon=icon("th")) #,
        #menuItem("Résultats", tabName="Resultat", icon=icon("th"))
      )
    }
    
    return(menu)
  })
  
  # Récupération des données : 
  fichier <- reactive({
    req(input$datafile)
    data.frame(read.csv(input$datafile$datapath))
  })                      # On récupère les données
  observe({
    req(fichier())
    updateSelectInput(session,"selectVariables",choices=names(fichier()),selected=names(fichier()))
  })                                  # On feed le selecteur de champs
  output$renamer <- renderUI({
    lapply(colnames(fichier()[,input$selectVariables]),function(i){
      textInput(paste0("col_",i),i,i)
    })
  })               # On feed le renamer
  data <- reactive({
    Data <- fichier()[,input$selectVariables]
    
    for ( i in names(input) ){
      if(grepl(pattern = "col_",i)){
        colnames(Data)[which(colnames(Data)==substr(i,5,nchar(i)))]=input[[i]]
      }
    }
    
    return(Data)
  })                         # On fabrique les données finales
  
  # Output des données brutes : 
  output$fileTable <- DT::renderDataTable({
    DT::datatable(data(),options=list(pageLength = 10 ))
  })  # On output les donnée pour pouvoir les regarder.
  output$resume <- renderPrint({summary(data())})
  
  
  # cop <- estimate.copula(data(),margins="edf",type=1)
  
  choices = c("HAC Gumbel",
              "AC Gumbel",
              "HAC Clayton",
              "AC Clayton",
              "HAC Franck",
              "AC Franck",
              "HAC Joe",
              "AC Joe",
              "HAC Ali-Mikhail-Haq",
              "AC Ali-Mikhail-Haq")

  
    output$valeur <- reactive({
      num_cop <- (1:10)[(choices == input$selectCopToFit)]
      
      capture.output(estimate.copula(data(),type=num_cop,margins="edf"))
      
    })
  
  

}


####### Running the app.
shinyApp(ui = ui, server = server)

