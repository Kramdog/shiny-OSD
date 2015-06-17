
shinyUI(fluidPage(
  
  fluidRow(
    column(4,
         textInput("series", "Enter Soil Name", value ="brownsto")     
         ),
    column(8,
        br()  
        #submitButton("update") 
         )
  ),
  
    wellPanel("OSD Soil Profile with Profile Image (its in the works)"),
  fluidRow(
    column(6,
    plotOutput('osd')
    ),
    column(6
    #imageOutput("image")
    )),
  
    wellPanel("KSSL Lab data with various selectable properties, sorted by Pedon_ID"),
    selectInput("radiok", label = h5("Choose property to view"),
                choices = names(hk), selected = names(hk)[[1]]), 
    #submitButton("update"),
 
    plotOutput('kssl'),
    
    wellPanel("Summary of Lab Texture Class of A, B or C Horizons"),
    "This is the number of aggregated horizons being combined with the OSD horizon names",
    verbatimTextOutput("symbols"),
    "The left column is the OSD's horizon names, and the top row in all the KSSL pedon horizons",
    verbatimTextOutput("table"),
    
  fluidRow(
      column(4,
        selectInput("radio", label = h5("Choose horizons layer to view"),
                       choices = colnames(h), selected = 1)
    ),
      column(7,
        br(),br()
        #submitButton("update") 
      )
    ),
    
  fluidRow(
      column(6,
        "Histogram plot of textures",       
        br(),br(),br(),
        plotOutput('hist')       
    ),
      column(5,
        "Textural Triangle Plot of KSSL Horizon Data",
        checkboxInput('plabel', 'Turn Labels on or off', value=FALSE),
        plotOutput('triangle', width=425, height=400)       
      )
    ),
  
  fluidRow(
    plotOutput('triangle2')      
    ),
  
  textOutput("counter"), 
  
  "Statistics for each horizon, aggregated",  
  checkboxInput('stats2', 'Turn Statistics on or off'),
  conditionalPanel(
    verbatimTextOutput("stats"),    
    condition = "input.stats2 == true"
  )

))

