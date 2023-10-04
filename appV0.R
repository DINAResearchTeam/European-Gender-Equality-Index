#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c("shiny","shinydashboard","shinyWidgets","leafpop","tidyverse","gghighlight",
             "fmsb", "heatmaply", "RColorBrewer", "leaflet", "sf", "itsadug", "hexbin", "hrbrthemes",
             "stringr", "plotly", "gganimate", "ggnewscale")
# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE,repos='http://cran.rediris.es')
  }
  library(x, character.only = TRUE)
})
#verify they are loaded
search()
rm(list = ls())

# Read the dataset
datos <- read.csv("datos.csv", header=TRUE, sep=";", dec=",", fileEncoding="UTF-8-BOM")
datos <- datos %>% gather(key = "categoria", value = "valor", 
                          "Gender.Equality.Index", "WORK", "MONEY", "KNOWLEDGE", "TIME",
                          "POWER", "HEALTH")
datos$categoria <- str_to_title(str_replace_all(datos$categoria, "\\."," "))

# Read the dataset that contains the coordinates for the cartogram
dsq <- read.csv("EU27-coord.csv", header=TRUE, sep=";", dec=",", fileEncoding="UTF-8-BOM")
colnames(dsq) <- c("Country", "lng", "lat")
dsq <- rbind(dsq, c("EU", 2, 12))
dsq$lng <- as.numeric(dsq$lng)
dsq$lat <- as.numeric(dsq$lat)

datos <- merge(datos, dsq)

rownames(datos) <- NULL

x_rect = c()
y_rect = c()

# Create the coordinates
for (j in 1:nrow(datos)){
  x_rect <- c(x_rect, datos$lng[j]-0.5,datos$lng[j]+0.5,
              datos$lng[j]+0.5,datos$lng[j]-0.5)
  y_rect<- c(y_rect, datos$lat[j]+0.5,datos$lat[j]+0.5,
             datos$lat[j]-0.5,datos$lat[j]-0.5)
  
}

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  #' addLegend_decreasing
  #'
  #' Function that makes the legend on descending order
  #' 
  #' Created by mpriem89 (https://github.com/rstudio/leaflet/issues/256#issuecomment-440290201)
  #'
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


## read shapefile
EU27map <- st_read('EU27-basic.shp')
EU27map_ <- st_transform(EU27map, 4326) #reproject
st_crs(EU27map_) #check CRS

EU27map_dtf <- left_join(EU27map_, datos, by = c("CNTR_CODE"= "Country")) #add attributes to the map


datos <- datos[rep(seq_len(nrow(datos)), each = 4), ]

datos$x <- x_rect
datos$y <- y_rect

rownames(datos) <- NULL

datos <- datos %>% dplyr::arrange(Index.year, categoria)

createMaps<-function(year, cat, dat, tipo, escala){
  #' Create Maps
  #'
  #' Function that creates the maps
  #' 
  #' @param cat Domain (Gender Equality Index, Work, Money, Knowledge, Time, Power, Health)
  #' @param dataset Dataset containing the values for each domain
  #' @param tipo Variable that determines the page where the maps are displayed (Compare, evolution or animation)
  #' @param escala On the "Evolution" tab, the type of scale (relative or absolute)
  #' @param year Year to filter the dataset on the "Comparative tab"
  #' @param fulldataset Dataset original (not filtered by category or year) to determine the limits on the scale
  
  # "Comparative" tab
  if (tipo == "tie"){
    EUdata<- dat %>% dplyr::filter(Index.year == as.integer(year)) %>%
      dplyr::filter(categoria == cat)
    
    df <- dat %>% dplyr::filter(categoria == cat)
    lim = c(min(df$valor), max(df$valor))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g<br/>%g",
      EUdata$CNTR_CODE, EUdata$valor, EUdata$Index.year
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorNumeric(
      palette = "BuPu",
      reverse = FALSE,
      domain = c(min(df$valor), max(df$valor)))

  }
  # Other tabs
  else {
    EUdata <- dat %>% 
      dplyr::filter(categoria == cat)
    
    lim = c(min(EUdata$valor), max(EUdata$valor))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g<br/>",
      EUdata$CNTR_CODE, EUdata$valor
    ) %>% lapply(htmltools::HTML)
    
    if(escala == "Relative"){
      pal <- colorNumeric(
        palette = "PiYG",
        reverse = FALSE,
        domain = c(-max(abs(EUdata$valor)), max(abs(EUdata$valor))))
      lim = c(-max(abs(EUdata$valor)), max(abs(EUdata$valor)))
    }
    
    else if(escala == "Absolute"){
      
      df <- dat
      
      pal <- colorNumeric(
        palette = "PiYG",
        reverse = FALSE,
        domain = c(-df$valor[which.max( abs(df$valor) )],df$valor[which.max( abs(df$valor) )]))
      lim = c(-df$valor[which.max( abs(df$valor) )],df$valor[which.max( abs(df$valor) )])#c(-max(abs(df$valor)), max(abs(df$valor)))
      
    }
    else{
      pal <- colorNumeric(
        palette = "BuPu",
        reverse = FALSE,
        domain =  c(min(EUdata$valor), max(EUdata$valor)))
    }
  }
  
  # Create the leaflet map
  lflt<-leaflet(data = EUdata) %>% addTiles(options = tileOptions(minZoom=3)) %>%
      addPolygons(., fillColor =  ~pal(valor), 
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  # Add the legend
  lflt = lflt %>% addLegend_decreasing(pal = pal, values = ~lim, opacity = 1, decreasing = TRUE, title="")
  
  
  
  lflt
  
}


# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "",
  #theme = "journal",
  windowTitle="Extended Visualizations of the European Gender Equality Index",
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  
  
  tabPanel("Info",
           h1("Extended Visualizations of the European Gender Equality Index"),
           
           "This webpage has been elaborated by DINA, the Data Interaction aNd Augmentation research team of the Universitat de València (Spain), with the purpose to bring extended visualizations to the EIGE’s Gender Equality Index that involve both the geographical and the temporal dimensions. Useful links:",
           tags$div(
             tags$ul(
               tags$li("Original data:", tags$a(href="https://eige.europa.eu/modules/custom/eige_gei/app/content/downloads/gender-equality-index-2013-2015-2017-2019-2020-2021-2022.xlsx", "https://eige.europa.eu/modules/custom/eige_gei/app/content/downloads/gender-equality-index-2013-2015-2017-2019-2020-2021-2022.xlsx")),
               tags$li("EIGE’s Gender Equality Index webpage:",tags$a(href="https://eige.europa.eu/gender-equality-index/2022/EU", "https://eige.europa.eu/gender-equality-index/2022/EU")),
               tags$li("DINA webpage:",tags$a(href="https://www.uv.es/dinateam/index.html", "https://www.uv.es/dinateam/index.html"))
             )
           ),
           
           br(),
           br(),
           "Copyright © 2023"
           
  ),
                 
  tabPanel("Comparative",
    fluidRow(
      column(3, 
             conditionalPanel(
               condition = "input.base != 'Heatmap'",
               selectInput(inputId = "categoria", choices = unique(datos$categoria), label = "Select a category")
             ),
             
      ),
      column(3,
             shiny::radioButtons(inputId = "base", label = "Base", choices = c("Map", "Cartogram", "Heatmap"),
                           inline = TRUE, selected = "Cartogram")
      )
    ),
    fluidRow(uiOutput("distPlot"))
  ),
  
  tabPanel("Evolution",
           fluidRow(
             column(3, 
                    selectInput(inputId = "anyo_i", choices = unique(datos$Index.year), label = "Base year")
             ),
             column(3, 
                    uiOutput(outputId = "anyos")
             ),
             column(3,
                    shiny::radioButtons(inputId = "base1", label = "Base", choices = c("Map", "Cartogram", "Heatmap"),
                                        inline = TRUE, selected = "Cartogram")
             ),
             column(3,
                    shiny::radioButtons(inputId = "escala", label = "Scale", choices = c("Relative", "Absolute"),
                                        inline = TRUE, selected = "Relative")
             )
           ),
           fluidRow(uiOutput("distPlot1"))
           ),
  
  tabPanel("Animations",
           fluidRow(
             column(3,
                    shiny::radioButtons(inputId = "base2", label = "Base", choices = c("Map", "Cartogram", "Heatmap"),
                                        inline = TRUE, selected = "Map")
             ),
             column(3,
                    uiOutput("slider")
             )
             ),
           fluidRow(uiOutput("plot2"))
           )
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Comparative tab
    output$distPlot <- renderUI({
      req(input$base)
      
      datos <- datos %>% mutate(text = paste0("Country: ",Country, "\n",
                                              categoria, ": ", valor,
                                              "\nYear: ", Index.year))
      
      
      if(input$base == "Cartogram"|input$base == "Map"){
        p = unique(datos$Index.year)
      }
      else if(input$base == "Heatmap"){
        p = unique(datos$categoria)
      }
       
      plots <- lapply(p, function(i) {
        
        
        
        if(input$base == "Cartogram"|input$base == "Map"){
          df <- datos %>% dplyr::filter(categoria == input$categoria) %>% dplyr::filter(Index.year == i)
          eu = df %>% dplyr::filter(Country == "EU")
          df = df %>% dplyr::filter(Country != "EU")
          
        }
        else if(input$base == "Heatmap"){
          df <- datos %>% dplyr::filter(categoria == i)
          eu = df %>% dplyr::filter(Country == "EU")
          
        }
        
        d = datos %>% dplyr::filter(categoria == input$categoria)
        
        
        
        lim = c(min(d$valor), max(d$valor))
        
        
        pal <- colorNumeric(
          palette = "BuPu",
          reverse = FALSE,
          domain = df$valor)

        rank = min(d$valor) + 
          (max(d$valor)-min(d$valor))/2
        
        for (j in 1:nrow(df)){
          df$fill[j] <- pal(df$valor[j])
          df$col[j] <- if (df$valor[j] < rank) 'black' else 'white'
        }
        
        for (j in 1:nrow(eu)){
          eu$fill[j] <- pal(eu$valor[j])
          eu$col[j] <- if (eu$valor[j] < rank) 'black' else 'white'
        }
        # Cartogram visualization
        if(input$base == "Cartogram"){
          
          g = ggplotly(
            ggplot(df, aes(x=x,y=y, label = Country, fill = valor, text = text)) + 
              geom_polygon() +
              geom_point(data = eu, aes(x=lng,y=lat, label = Country,
                                        fill = valor, text = text, colour="white"),
                         size=10)+
              labs(fill = input$categoria, title = i) +
              scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5, "BuPu"), limits = lim) +
              scale_colour_manual(values = c("black", "white"), guide=FALSE) + 
              guides(fill = "colorbar", colour="none") +
              geom_text(data = df, aes(x=lng, y=lat, colour=col))+
              geom_text(data = eu, aes(x=lng, y=lat, colour=col))+
              theme_bw() +
              theme(text = element_text(family = 'Fira Sans'),
                    axis.text.x=element_blank(), 
                    axis.title.x=element_blank(),
                    axis.ticks.x=element_blank(), 
                    axis.text.y=element_blank(),  
                    axis.ticks.y=element_blank(),
                    axis.title.y=element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()),
            tooltip = "text"
            
          )
          bo <- box(width = 4, background = NULL,
                    g)
          
        }
        # Heatmap visualization
        else if(input$base == "Heatmap"){
          df <- df %>% arrange(desc(valor))
          l = c(min(df$valor), max(df$valor))
          
          g = ggplotly(ggplot(df, aes(x = Country, y = as.character(Index.year), text=text)) +
                         geom_bin2d(aes(fill=valor)) +
                         scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5, "BuPu"), limits = l) +
                         scale_y_discrete(limits=rev) +
                         labs(title = i, fill="") +
                         theme_minimal() +
                         theme(text = element_text(family = 'Fira Sans'),
                               plot.title = element_text(hjust = 0.5),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank()),
                       tooltip = "text"
          )
          bo <- box(width = 12, background = NULL,
                    #title = h2(i, style = "display:inline; font-weight:bold"),
                    g)
        }
        # Map visualization
        else{
          p = createMaps(i, input$categoria, EU27map_dtf, "tie", "")
          
          bo <- box(width = 4, background = NULL,
                    title = i,
                    p)
        }
        return(bo)
        
      })
      
    })
    
    # Year to compare the index year
    output$anyos <- renderUI({
      c <- unique(datos$Index.year) 
      c <- c[ c > input$anyo_i]
        
      selectInput(inputId = "anyo_f", choices = c, label = "Year to compare")
    })
    # Reactive variable based on the index year and the year to compare
    evol <- reactive({
      req(input$anyo_f, input$anyo_i)
      d1 = datos %>% dplyr::filter(Index.year == input$anyo_i) 
      d2 = datos %>% dplyr::filter(Index.year == input$anyo_f)
      
      d3 = d1 %>% select(-"Index.year") %>% right_join(d2 %>% select("categoria", "Country", "valor", "x", "y") %>% unique(), by=c("categoria", "Country", "x", "y"))
      colnames(d3)[grep("^valor.x", colnames(d3))] = "v1"
      colnames(d3)[grep("^valor.y", colnames(d3))] = "v2"
      
      d3 <- d3 %>% mutate(valor = round(v2 - v1, 2))
      
      d3 <- d3 %>% mutate(text = paste0("Country: ",Country, "\n",
                                              categoria, ": ", valor,
                                              "\nYear: ", paste(input$anyo_i, input$anyo_f, sep = "-")))
      
      d3
      
    })
    # "Evolution" tab
    output$distPlot1 <- renderUI({
      req(input$base1)
      
        EU27map_dtf <- left_join(EU27map_, evol(), by = c("CNTR_CODE"= "Country")) #add attributes to the map
        EU27map_dtf <- EU27map_dtf %>% select(-lat, -lng, -x, -y)
        EU27map_dtf = EU27map_dtf %>% distinct()
    # Loop through categories
    plots <- lapply(unique(evol()[["categoria"]]), function(i) {
        
        df1 <- evol() %>% dplyr::filter(categoria == i) 
        eu = df1 %>% dplyr::filter(Country == "EU")
        df = df1 %>% dplyr::filter(Country != "EU")
        
        if (input$escala == "Relative"){
          pal <- colorNumeric(
            palette = "PiYG",
            reverse = FALSE,
            domain = df$valor[which.max( abs(df$valor) )]:-df$valor[which.max( abs(df$valor) )]) 
          lim = c(-max(abs(df$valor)), max(abs(df$valor)))
        }
        else{
          pal <- colorNumeric(
            palette = "PiYG",
            reverse = FALSE,
            domain = evol()[["valor"]][which.max( abs(evol()[["valor"]]) )]:-evol()[["valor"]][which.max( abs(evol()[["valor"]]) )]) 
          lim = c(-max(abs(evol()[["valor"]])), max(abs(evol()[["valor"]])))
        }
        
        rank = min(df$valor) + 
          (max(df$valor)-min(df$valor))/2
        
        for (j in 1:nrow(df)){
          df$fill[j] <- pal(df$valor[j])
          df$col[j] <- if (df$valor[j] < rank) 'black' else 'white'
          
        }
        
        
        for (j in 1:nrow(eu)){
          eu$fill[j] <- pal(eu$valor[j])
          eu$col[j] <- if (eu$valor[j] < rank) 'black' else 'white'
        }
        
        if(i == "Gender Equality Index"){
          f = "Gender\nEquality\nIndex"
          
        }
        else{
          f = i
        }
        # Cartogram visualization
        if(input$base1 == "Cartogram"){
          
          g = ggplotly(
            ggplot(df, aes(x=x,y=y, label = Country, fill = valor, text = text)) + 
              geom_polygon() +
              geom_point(data = eu, aes(x=lng,y=lat, label = Country,
                                        fill = valor, text = text),
                         size=10, colour="white")+
              labs(fill = f, title = i) +
              scale_fill_gradient2(low=brewer.pal(5, "PiYG")[1], mid=brewer.pal(5, "PiYG")[3], high=brewer.pal(5, "PiYG")[5], 
                                   midpoint=0,   
                                   limits=lim)+
              guides(fill = "colorbar", colour="none") +
              geom_text(data = df, aes(x=lng, y=lat), colour="black")+
              geom_text(data = eu, aes(x=lng, y=lat), colour="black")+
              theme_bw() +
              theme(text = element_text(family = 'Fira Sans'),
                    axis.text.x=element_blank(), 
                    axis.title.x=element_blank(),
                    axis.ticks.x=element_blank(), 
                    axis.text.y=element_blank(),  
                    axis.ticks.y=element_blank(),
                    axis.title.y=element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()),
            tooltip = "text"
            
          ) %>%
          layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
          bo <- box(width = 4, background = NULL, 
                    g)
          
        }
        # Heatmap visualization
        else if(input$base1 == "Heatmap"){
          
          g = ggplotly(ggplot(df1, aes(x =  Country, y = 1, text=text)) +
                     geom_point(aes(colour  = valor), shape=15, size=12, stroke=2)+#, position = position_dodge(width=0.75)) +
                     ylim(c(0.5,1.5))+
                       scale_color_gradient2(low=brewer.pal(5, "PiYG")[1], 
                                             mid=brewer.pal(5, "PiYG")[3], high=brewer.pal(5, "PiYG")[5], 
                                             midpoint=0,
                                             limits=lim) +
                     geom_text(data = df1, aes(x=Country, y=1, label=Country), colour = "black") +
                       
                     theme_minimal()+
                       labs(title=i, fill = "")+
                       
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           plot.title = element_text(margin=margin(0,0,100,0)),
                           axis.title.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks.y=element_blank(),
                           axis.title.x=element_blank(),
                           axis.text.x=element_blank(),
                           legend.position="bottom"
                     ), tooltip = "text", height = 300
          ) %>% animation_opts(frame = 1000, transition = 0, redraw = TRUE) %>%
            layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
          
          bo <- box(width = 12, background = NULL, 
                    g)
        }
        # Map visualization
        else{
          p = createMaps("", i, EU27map_dtf, "cat", input$escala)
          
          bo <- box(width = 4, background = NULL,
                    title = i,
                    p)
        }
        
        return(bo)
        
      })
      
      #subplot(plots, nrows=2) 
    })
    
    output$slider <- renderUI({
      
      req(input$base2 == "Map")
      
      d1 = year(as.Date(as.character(unique(datos$Index.year)), format = "%Y"))
      
      sliderTextInput(inputId = "slid", label = "Date:", choices = unique(datos$Index.year), animate = animationOptions(interval = 5000, loop = FALSE))
    })
    
    
    points <- reactive({
      req(input$slid)
      
      d = datos %>%
        filter(Index.year == input$slid)
      
      withProgress(message = 'Loading data, please wait', value = 0, {
        EU27map_dtf2 <- left_join(EU27map_, data.frame(d) , by = c("CNTR_CODE"= "Country")) #add attributes to the map
        incProgress(1/3)
        EU27map_dtf2 <- EU27map_dtf2 %>% select(-lat, -lng, -x, -y)
        incProgress(2/3)
        EU27map_dtf2 = EU27map_dtf2 %>% distinct()
        incProgress(3/3)
      })
      EU27map_dtf2
    })
    
    output$plot2 <- renderUI({
      
      
      plots <- lapply(unique(datos$categoria), function(i) {
        
        d1 <- datos %>% filter(categoria == i)
        
        lim = c(min(d1$valor), max(d1$valor))
        
        if (input$base2 == "Cartogram"){
          
          d1 <- d1 %>% mutate(text = paste0("Country: ",Country, "\n",
                                            categoria, ": ", valor,
                                            "\nYear: ", Index.year))
          
          eu = d1 %>% dplyr::filter(Country == "EU")
          d1 = d1 %>% dplyr::filter(Country != "EU")
          
          
          g = ggplotly(
            ggplot(d1, aes(x=x,y=y, label = Country, fill = valor))+#, frame=Index.year)) + 
              geom_polygon(data = d1, aes(frame=Index.year)) +
              geom_point(data = eu, aes(x=lng,y=lat, label = Country, 
                                        fill = valor, frame=Index.year),  col='white',
                         size=10)+
              scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5, 'BuPu'), limits =lim) +
              guides(fill = 'colorbar', colour='none') +
              labs(title = i, fill="") +
              geom_text(data = d1, aes(x=lng, y=lat), colour='black')+
              geom_text(data = eu, aes(x=lng, y=lat), colour='black')+
              theme_bw() +
              theme(text = element_text(family = 'Fira Sans'),
                    axis.text.x=element_blank(), 
                    axis.title.x=element_blank(),
                    axis.ticks.x=element_blank(), 
                    axis.text.y=element_blank(),  
                    axis.ticks.y=element_blank(),
                    axis.title.y=element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
          )  %>% animation_opts(frame = 1000, transition = 0, redraw = TRUE)%>% style(hoverinfo = 'none') %>%
            animation_slider(
              currentvalue = list(prefix = "Index Year ")
            )
          
          bo <- box(width = 4, background = NULL,
                    g)
          
        }
        
        else if(input$base2 == "Map"){
          req(input$slid)
    

          p = createMaps("", i, points(), "cat2", "")
          
          bo <- box(width = 4, background = NULL,
                    title = i,
                    p)
          
        }
        
        else{
          d1 <- datos %>% filter(categoria == i)
          
          d1 <- d1 %>% mutate(text = paste0("Country: ",Country, "\n",
                                            categoria, ": ", valor,
                                            "\nYear: ", Index.year))
          
          rank = min(d1$valor) + 
            (max(d1$valor)-min(d1$valor))/2
          
          for (j in 1:nrow(d1)){
            d1$col[j] <- if (d1$valor[j] < rank) 'black' else 'white'
            
          }
          
          g =  ggplotly(ggplot(d1, aes(x =  Country, y = 1, text=text)) +
                             geom_point(aes(colour  = valor, frame=Index.year), shape=15, size=12, stroke=2)+
                          scale_colour_gradientn(colors = RColorBrewer::brewer.pal(5, "BuPu"), limits = lim)+
                             geom_text(data = d1, aes(x=Country, y=1, label=Country), colour='black') +
                          
                             theme_minimal()+
                          labs(title = i, colour = "") +
                          
                             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.text.x=element_blank()
                                   
                             ), tooltip = "text"
          ) %>% animation_opts(frame = 1000, transition = 0, redraw = TRUE)  %>%
            animation_slider(
              currentvalue = list(prefix = "Index Year ")
            )
          bo <- box(width = 12, background = NULL,
                    g)
        }
        return(bo)
        
      })

      
      
    })
    
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
