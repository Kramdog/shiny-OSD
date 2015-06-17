# by Ben Marshall 
library(shiny)
library(soilDB)
library(plotrix)
library(ggtern)  # for the dotted lines on the textural triangle

shinyServer(function(input, output) {
  
  # fetch osd
  data <- reactive({
    osd <- fetchOSD(input$series)
    })
  # fetch kssl
  data2 <- reactive({
    ks <- fetchKSSL(input$series)
    })
  
  # osd - Generalize
  data3 <- reactive({
    osd <- data()
    ks <- data2()
    osd.names <- data.frame(osd$hzname)
    n <- c(paste(osd.names$osd.hzname, sep = ""))
    p <- c(paste(osd.names$osd.hzname, sep = ""))  
    ks$genhz <- generalize.hz(ks$hzn_desgn, n, p)
    })
  
  output$osd <- renderPlot({
    data()
    #osd <- fetchOSD(input$series)
    par(mar=c(1,1,0,1))
    plot(data(), name='hzname', color='soil_color', cex.names=0.9, max.depth=175)
    abline(h=c(50, 100, 150), lty=2, col='grey')
    })
  
  #output$image <- renderImage({
  #  filename <- normalizePath(file.path('./images', paste(input$series, '.jpg', sep='')))
  #  list(src = filename, contentType = 'image/jpg',  height = 400)
  #  }, deleteFile = FALSE)
  
  output$symbols <- renderPrint({
    table(data3())
    })
  
  output$table <- renderPrint({
    osd <- data()
    ks <- data2()
    osd.names <- data.frame(osd$hzname)
    n <- c(paste(osd.names$osd.hzname, sep = ""))
    p <- c(paste(osd.names$osd.hzname, sep = "")) 
    ks$genhz <- generalize.hz(ks$hzn_desgn, n, p)
    table(ks$genhz, ks$hzn_desgn)
  })
  
  output$kssl <- renderPlot({
    kssl <- data2()
    kssl$ph_h2o<-round(kssl$ph_h2o, 1)
    kssl$ex_ca<-round(kssl$ex_ca, 1)
    kssl$ex_mg<-round(kssl$ex_mg, 1)
    kssl$db_od<-round(kssl$db_od, 1)
    kssl$Est_OM<-round(kssl$Est_OM, 1)
    kssl$whc<-round(kssl$whc, 1)
    par(mar=c(1,1,3,1))
    #plot(kssl, name='hzn_desgn', color='clay', id.style='side', label='pedon_id', cex.names=0.8, max.depth=175)
    plot(kssl, name=input$radiok, color=input$radiok, id.style='side', label='pedon_id', cex.names=0.8, max.depth=175)
    abline(h=c(50, 100, 150), lty=2, col='grey')
    })
    
  output$hist <- renderPlot({
    #ks$hzn_mid <- with(horizons(ks), (hzn_bot + hzn_top) / 2)
    h<-horizons(data2())
    t<-subset(h, grepl(input$radio, h$hzn_desgn))
    #t<-subset(h, grepl("A$", h$hzn_desgn))
    t$lab_texture_class <- toupper(t$lab_texture_class)
    par(mar=c(5,5,2,2))
    barplot(table(t$lab_texture_class), xlab='Textures of Horizons', ylab='Frequency')
    })
  
  output$triangle <- renderPlot({
    d <- as(data2(), "data.frame")
    # subset each series, and only those rows with non-missing clay values
    d.A <-subset(d, grepl(input$radio, d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
    # plot soil textures on a triangle 
    soil.texture(show.names=FALSE, col.lines = "black", show.grid = TRUE)  # plot empty triangle
    triax.points(d.A[, c("sand", "silt", "clay")], col.symbols = "blue", pch = 16, label.points=input$plabel, point.labels=d.A$hzn_desgn)  
    legend("topleft", legend = paste(input$radio, "Horizons selected"), col = "blue", pch = 16, cex = 1.1, bty = "n")   
    #ggtern(data=d.A,aes(sand,clay,silt)) + geom_point() + geom_confidence()
    })
 
  output$triangle2 <- renderPlot({
    d <- as(data2(), "data.frame")
    d.A <-subset(d, grepl("A", d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
    d.B <-subset(d, grepl("B", d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
    d.C <-subset(d, grepl("C", d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
    a <- ggtern(data=d.A,aes(y=clay,x=sand,z=silt)) + geom_point() + labs(title="A horizons") + 
    geom_confidence()
    b <- ggtern(data=d.B,aes(y=clay,x=sand,z=silt)) + geom_point() + labs(title="B horizons") +
    geom_confidence()
    c <- ggtern(data=d.C,aes(y=clay,x=sand,z=silt)) + geom_point() + labs(title="C horizons") +
    geom_confidence()
    ggtern(data=d,aes(sand,clay,silt)) + geom_point()
    ggtern.multi(a,b,c,cols=3) 
    })
  
  output$stats <- renderPrint({
    d <- as(data2(), "data.frame")
    d.A <-subset(d, grepl(input$radio, d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
    summary(d.A)
    })
  
  output$counter <-
    renderText({
      if (!file.exists("counter.Rdata"))
        counter <- 0
      else
        load(file="counter.Rdata")
      counter <- counter + 1
      save(counter, file="counter.Rdata")
      paste0("Website Hits: ", counter)      
    })
})

# Filter by mlra, using ks$site$mlra
# Save all these as a plot ??

# This one shows all  A, B, and C, horizons all in ine plot
#output$triangle <- renderPlot({
#d <- as(data2(), "data.frame")
## subset each series, and only those rows with non-missing clay values
#d.A <-subset(d, grepl("A", d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
#d.B <-subset(d, grepl("B", d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
#d.C <-subset(d, grepl("C", d$hzn_desgn) & !is.na(clay) & !is.na(silt) & !is.na(sand))
## plot soil textures on a triangle 
#soil.texture(col.lines = "black", show.grid = TRUE)  # plot empty triangle
#triax.points(d.A[, c("sand", "silt", "clay")], col.symbols = "red", pch = 16)  
#triax.points(d.B[, c("sand", "silt", "clay")], col.symbols = "blue", pch = 17) 
#triax.points(d.C[, c("sand", "silt", "clay")], col.symbols = "light green", pch = 19)  
#legend("topright", legend = c("A horizons", "B horizons", "C Horizons"), col = c("red", 
#      "blue", "light green"), pch = 16, cex = 1.1, bty = "y")
#})

#p <- c(paste(osd.names$osd.hzname, sep = ""), 
 #      paste("A$", sep = ""),   #   These need to be taken out 
  #     paste("B$", sep = ""),   #   These need to be taken out 
   #    paste("C$", sep = ""))   #   These need to be taken out
