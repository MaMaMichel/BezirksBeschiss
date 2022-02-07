library(bslib)
library(tidyverse)
library(thematic)
library(sf)

CombinedData <- read_csv('data/CombinedData.csv')
DistrictOutlines <- st_read("data/BEZIRKSGRENZEOGDPolygon.shp")

DistrictOutlines <- DistrictOutlines %>% 
  arrange(BEZNR) %>% 
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  ) %>%
  mutate(DISTRICT = CombinedData$DISTRICT[-1])

CombinedData <- CombinedData %>% left_join(DistrictOutlines, by="DISTRICT")

thematic::thematic_shiny() 

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = bs_theme( bg = "#6d9cbf", fg = "white", primary = "#FCC780",
                    base_font = font_google("Space Mono"),
                    code_font = font_google("Space Mono")),
  # App title ----
  titlePanel("#BezirksBeschiss"),
  
  # Sidebar layout with input and output definitions ----
  tabsetPanel(
    tabPanel("Karte", 
             sidebarLayout(
                sidebarPanel(
                  h3("Warum ist mein Bezirk so beschissen?"),
                  p("\"Wenngleich in anderen Regionen dieses wunderschönen Landes die Schneeschmelze das langsame Erwachen des Frühlings einläutet, so offenbart sich auf manchen Straßen und Plätzen Wiens ein anderes Bild. Über den Winter arbeiteten emsigst die Hunde der Stadt unter dem wachsamen Auge ihrer Besitzer:innen. Wie der Feldhamster der sich auf schlechtere Zeiten vorbereitet schissen sie tunlichst unter die schlammgraue Wiener Schneesuppe. Und so erscheinen sie, die halbzersetzten Häufchen, wie längst verloren geglaubte Artefakte aus dem Permafrost der Sibirischen Tundra und verwandeln die Stadt in ein Minenfeld faul riechender Irritation.\" - Hans Kelsen, 1919"),
                  p("Dieses Dashbaord soll den doch teils unterschiedlich ausfallenden #BezirksBeschiss in Wien für die Bürger:innen des Landes erfahrbar machen. Viel Spaß!"),
                  
                ),
                mainPanel( h3("Beschiss-Score pro Bezirk:"),
                           p("Der Score errechnet sich aus der Anzahl der Sackerlspender pro Hund und dem prozentuellen Grünflächenanteil der Bezirke."),
                           plotOutput("map", width = "auto", height = "800px"))
              )
    ),
    tabPanel("Scatter Plot", 
             sidebarLayout(
               sidebarPanel(
                 h3("Warum ist mein Bezirk so beschissen?"),
                 p("Zur näheren Betrachtung der Sachlage kann diese Grafik herangezogen werden:"),
                 p("Auf der X- und Y-Achse befinden sich die beiden Komponenten des berechneten Beschiss-Score, Farbe signalisiert den berechneten Wert. Für die Größe der Punkte ist die Hundedichte eines Bezirkes ausschlaggebend. Die Hundedichte ist einfach die Anzahl an Hunden pro Hektar Fläche eines Bezirkes."),
                 p(""),
                 varSelectInput("point_size", "Variable:", data.frame(c("Hunde_pro_Ha", "Mietpreis")), selected = "Hunde_pro_Ha"),
                 sliderInput("graphics_size", "Size Adjust:", min = 1, 
                             max = 30, value = 15, ticks = T, sep = ""),
               ),
               mainPanel(plotOutput("scatter", width = "auto", height = "800px"))
             )
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$scatter <- renderPlot({
    
    ggplot(CombinedData[-1,], aes(x = Prozent_Grünfläche, y = Sackerlspender_pro_Hund, label = DISTRICT)) +
      theme_classic(base_size = input$graphics_size) +
      theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent", color = "white"),
            axis.title.x = element_text(color="white"),
            axis.title.y = element_text(color="white"),
            axis.text.x = element_text(color="white"),
            axis.text.y = element_text(color="white"),
            axis.ticks = element_line(color = "white"),
            axis.line = element_line(color = "white"),
            legend.title = element_text(color="white"),
            legend.text = element_text(color="white"),
            plot.title = element_text(color="white"),
            plot.subtitle = element_text(color="white"),
            plot.caption = element_text(color="white"),
            plot.tag = element_text(color="white")
            ) +
      geom_point(aes(size = !!input$point_size,  col = SCORE)) +
      scale_size_continuous(range=c(input$graphics_size/3,input$graphics_size)) +
    
      scale_color_continuous(low="#ff5a47", high='#7bff47') +
      geom_text(nudge_x = 0.01, nudge_y = 0.004, size = input$graphics_size/3)
    
  },  bg=NA, execOnResize=T)
  
  output$map <- renderPlot({
    theme_set(theme_void())
    ggplot(data = CombinedData[-1,]) +
      geom_sf(aes(geometry = geometry, fill = SCORE), color = "black") +
      scale_fill_continuous(low="#ff5a47", high='#7bff47') +
      geom_text(aes(label = paste0(BEZ_RZ, "\n", round(SCORE, digits =2)), x = lon, y = lat), col = "black")
    
    
  },  bg=NA, execOnResize=T)
  
}

shinyApp(ui, server)