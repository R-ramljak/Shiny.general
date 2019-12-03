library(leaflet)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scales)
library(sf)
library(readxl)
library(rsconnect)
library(DT)
library(maptools)
library(rgdal)
library(broom)
library(rgeos)
library(shiny)
library(sp)
library(rmapshaper)
library(readxl)


# Kita Daten werden mit Geodaten jeweils zusammengebracht und dann als geeinte sf File in
# die App geladen. Somit gibt es für die drei Ansichten jeweils eine eigene sf File.


#Polygondaten Deutschlands
Germany <- readRDS("Data/gadm36_DEU_1_sf.rds")
#Germany <- readOGR("Data/DEU_adm/DEU_adm1.shp")
Germany <- ms_simplify(Germany, keep_shapes = T, keep = 0.002)
#MATCHED_BL <- spTransform(MATCHED, CRS("+init=epsg:4326"))

#Kinderbetreungsrate 0-2 & 3-5 Jahre im Zeitraum von 2012-2017 auf Bundesebene
kita_bund <- read_xlsx("Data/Daten_Kita_App.xlsx", sheet = "Factsheet.Gesamt")
#Kinderbetreungsrate 0-2 & 3-5 Jahre im Zeitraum von 2012-2017 auf Ost/West Ebene
kita_ow <- read_xlsx("Data/Daten_Kita_App.xlsx", sheet = "Factsheet.Ost-West")
#Kinderbetreungsrate 0-2 & 3-5 Jahre im Zeitraum von 2012-2017 auf Bundeslandebene
kita_bl <- read_xlsx("Data/Daten_Kita_App.xlsx", sheet = "Factsheet.BL")

# ost west Dummy Kreation
dummy_bund <- c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
                50, 50, 50, 50, 50)
dummy_ow <- c(30, 30, 20, 20, 30, 30, 30, 20, 30, 30, 30,
              30, 20, 20, 30, 20)

#Merging der Objekte
#Bundesebene
Germany$dummy_bund <- dummy_bund
kita_bund_sf <- Germany %>%
  group_by(dummy_bund) %>%
  summarize(first(dummy_bund)) %>%
  rename(bereichsnummer_1 = `first(dummy_bund)`) %>%
  left_join(kita_bund, by = c("dummy_bund" = "bereichsnummer_1")) %>%
  mutate_at(c("dummy_bund", "bereichsnummer_1"), ~as.character(.)) %>%
  mutate_if(is.numeric, ~round(., digits = 1))
kita_bund_sf$`first(NAME_1)` <- NULL

#Ost West
Germany$dummy_ow <- dummy_ow
kita_ow_sf <- Germany %>%
  group_by(dummy_ow) %>%
  summarize(first(dummy_ow)) %>%
  rename(bereichsnummer_1 = `first(dummy_ow)`) %>%
  left_join(kita_ow, by = c("dummy_ow"= "bereichsnummer_1")) %>%
  mutate_at(c("dummy_ow", "bereichsnummer_1"), ~as.character(.)) %>%
  mutate_if(is.numeric, ~round(., digits = 1))
kita_ow_sf$`first(NAME_1)` <- NULL

#Bundesland
kita_bl_sf <- Germany %>%
  group_by(NAME_1) %>%
  summarize(first(NAME_1)) %>%
  left_join(kita_bl, by = c("NAME_1" = "Einheit")) %>%
  rename(Einheit = NAME_1) %>%
  mutate_at(c("bereichsnummer_1"), ~as.character(.)) %>%
  mutate_if(is.numeric, ~round(., digits = 1))
kita_bl_sf$`first(NAME_1)` <- NULL
plot(kita_ow_sf)


#Abspeichern der finalen Datei für die weitere Leaflet Berabeitung
  saveRDS(kita_bl_sf, file = "Data/KITA_Spatial_BL.rds")
  saveRDS(kita_bund_sf, file = "Data/KITA_Spatial_ALL.rds")
  saveRDS(kita_ow_sf, file = "Data/KITA_Spatial_OST.rds")
  
  #############################
  
  # setwd("~/Projekte/BMFSFJ/Kitastudie/Shinyapp 4.0/")

# Data
FS.gesamt <- readRDS("Data/KITA_Spatial_ALL.rds")
FS.ost.west <- readRDS("Data/KITA_Spatial_OST.rds")
FS.bl <- readRDS("Data/KITA_Spatial_BL.rds")


# UI Essentials

# Hier de Ansichten einbauen
Ansicht = c("Deutschland - Gesamt",
            "Deutschland - Ost/West",
            "Deutschland - Bundesland")

UB <- read_xlsx("Data/Daten_Kita_App.xlsx", sheet = "Uebersicht")

Altersgruppe <- UB %>%
  select(Altersgruppe, Label) %>%
  mutate(Label = substr(Label, start = 2, stop = 3)) %>%
  dplyr::distinct() %>%
  filter(!Altersgruppe == "Grundschulalter") %>% # diese zeile raus machen um wieder die grundschulkategorie anzuzeigen
  deframe()



#UI Seite
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(inputId = "Ansicht", label = "Ansicht:", choices = Ansicht, selected = Ansicht[3]),
                  selectInput(inputId = "Altersgruppe", label = "Altersgruppe:", choices = Altersgruppe, selected = Altersgruppe[1]),
                  uiOutput("Kategorie"),
                  # selectInput("Kategorie", label = "Kategorie:", Kategorie),
                  uiOutput("Slider")
                  # textOutput("Text")
                  # plotlyOutput("Barplot")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Karte", leafletOutput("map", height = "650")),
                    tabPanel("Tabelle", dataTableOutput("tableview"), uiOutput("quellen"))))))


server <- function(input, output, session) {
  
  # Kategorien für UI
  
  ## Indikatorenauswahl ====
  
  ### Reactive Function
  kategorie.rec <- reactive({
    # validate(
    #   need(input$inItems.FS != "", "")
    # )
    UB %>%
      select(Variable, Label) %>%
      filter(grepl(input$Altersgruppe, Label)) %>%
      deframe()
  })
  
  ### Implementation
  output$Kategorie <- renderUI({
    selectInput(inputId = "inKategorie", label = "Kategorie:", 
                choices = kategorie.rec())
  })
  
  
  
  ## Zeitslider ====
  
  ### Reactive Function
  zeitraum <- reactive({
    validate(
      need(input$inKategorie != "", "")
    )
    UB %>%
      select(Label, Verfuegbarkeit) %>%
      separate_rows(Verfuegbarkeit, sep = ",") %>%
      split(.$Label) %>%
      map_df(~mutate(., min = min(as.numeric(Verfuegbarkeit)), max = max(as.numeric(Verfuegbarkeit)))) %>%
      select(-Verfuegbarkeit) %>%
      dplyr::distinct() %>%
      filter(Label == as.character(input$inKategorie))
  })
  
  ### Implementation
  output$Slider <- renderUI({
    sliderInput("inSlider", "Zeitleiste:",
                step = 1,
                min = zeitraum()$min,
                max = zeitraum()$max,
                value = zeitraum()$max,
                sep = "")
  })
  
  
  # Datenreactives
  
  ## Ansichtsebene Datensatz ----
  ansicht <- reactive({
    switch(input$Ansicht,
           "Deutschland - Gesamt" = FS.gesamt,
           "Deutschland - Ost/West" = FS.ost.west,
           "Deutschland - Bundesland" = FS.bl)
  })
  
  ## Altersgruppe Datensatz ----
  altersgruppe <- reactive({
    validate(
      need(input$Altersgruppe != "", "")
    )
    ansicht() %>%
      select(Einheit, matches(input$Altersgruppe))
  })
  
  ## Kategorie Datensatz ----
  kategorie <- reactive({
    validate(
      need(input$inKategorie != "", "")
    )
    altersgruppe() %>%
      select(Einheit, matches(input$inKategorie))
  })
  
  ## Jahr Datensatz ----
  jahr <- reactive({
    validate(
      need(input$inSlider != "", "")
    )
    kategorie() %>%
      select(Einheit, matches(paste0(".", input$inSlider)))
  })
  
  
  # Helpers Reactives
  
  ## Farbpaletten ----
  pal.farben <- reactive({
    UB %>%
      select(Label, Min, Max) %>%
      filter(grepl(input$inKategorie, Label))
  })
  
  einheit <- reactive({
    validate(
      need(input$inKategorie != "", "")
    )
    UB %>%
      select(Label, Einheit) %>%
      filter(grepl(input$inKategorie, Label))
  })
  
  Quellen <- reactive({
    UB %>%
      select(Label, Quelle) %>%
      filter(grepl(input$inKategorie, Label)) %>%
      select(Quelle)
    # list(a = a(.$Quelle, href = paste(.$Link), target="_blank"))
  })
  
  tablenames <- reactive({
    UB %>%
      select(Label, Variable) %>%
      filter(grepl(input$inKategorie, Label)) %>%
      select(Variable) %>%
      deframe()
  })
  
  # Outputs
  
  ## Tabelle ----
  output$tableview <- renderDataTable({
    req(ncol(jahr()) > 2)
    table <- jahr() %>%
      st_set_geometry(NULL)
    names(table) <- c("Einheit", tablenames())
    datatable(table, options = list(paging = FALSE, searching = FALSE))
  })
  
  
  ## Quellen ----
  output$quellen <- renderUI({
    tagList("Quellen:", Quellen())
  })
  
  
  ## Karte ----
  ### Base Map
  output$map <- renderLeaflet({
    leaflet(data = ansicht()) %>%
      setView(lng = 10.451526, lat = 51.165691, zoom = 6) %>%
      addPolygons(group = "Base",
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  color = "#1C1C1C",
                  weight = 1)
  })
  
  ### Adding values to map ----
  observe({
    
    req(ncol(jahr()) > 2)
    table.jahr <- jahr() %>%
      st_set_geometry(NULL)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      as.character(table.jahr$Einheit), pull(table.jahr, 2), einheit()$Einheit) %>%
      lapply(htmltools::HTML)
    
    
    pal.kreis <- colorNumeric(palette = "YlGnBu", domain = c(pal.farben()$Min, pal.farben()$Max))
    
    
    leafletProxy("map", data = jahr()) %>%
      addPolygons(group = "Jahr",
                  fillColor = ~pal.kreis(pull(table.jahr, 2)),
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  color = "#1C1C1C",
                  label = labels,
                  layerId = pull(table.jahr, 1),
                  weight = 1)
    
  })
  
  ### Adding legend to map ----
  observe({
    req(ncol(jahr()) > 2)
    table.jahr <- jahr() %>%
      st_set_geometry(NULL)
    
    pal.kreis <- colorNumeric(palette = "YlGnBu", domain = c(pal.farben()$Min, pal.farben()$Max))
    
    if(input$Ansicht == "Deutschland - Gesamt"){
      leafletProxy("map", data = jahr()) %>%
        clearControls()
    } else
      leafletProxy("map", data = jahr()) %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                pal = pal.kreis,
                values = ~pull(table.jahr, 2),
                title = paste("Angaben in<br>", as.character(pull(einheit(), 2)), sep = " "))
  })
  
  
  # ## Panel Plot ----
  # ### Klick Funktionalität 
  # click <- eventReactive(input$map_shape_click, {
  #   input$map_shape_click[[1]]
  # })
  # 
  # output$Text <- renderText({
  #   click()
  # })
  # 
  # panel.data <- eventReactive(click(), {
  #   kategorie() %>%
  #     st_set_geometry(NULL) %>%
  #     filter(grepl(click(), Einheit)) %>%
  #     gather("Key", "Wert", -Einheit) %>%
  #     mutate(Jahr = str_extract(string = Key, pattern = "[[:digit:]]+"))
  # })
  # 
  # v <- reactiveValues(clearPlot = TRUE)
  # 
  # 
  # output$Barplot <- renderPlotly({
  #   if (v$clearPlot)
  #     return()
  #   else
  #     p <- ggplot(data = panel.data()) +
  #       geom_line(data = panel.data(), aes(x = Jahr, y = Wert, group = 1)) +
  #       geom_point(data = panel.data(), aes(x = Jahr, y = Wert, group = 1))
  #   g <- ggplotly(p)
  #   g %>% config(displayModeBar = F) %>%
  #     layout(xaxis = Jahr) %>%
  #     layout(yaxis = Wert) %>%
  #     # layout(title = title(), titlefont = t) %>%
  #     layout(plot_bgcolor='rgb(254, 247, 234)')
  # })
  # 
  # observeEvent(kategorie(), {
  #   v$clearPlot <- T
  # }, priority = 10)
  # 
  # observeEvent(click(), {
  #   v$clearPlot <- F
  # }, priority = 10)
  # 
  
  
}

shinyApp(ui, server)

# options(encoding = "UTF-8")
