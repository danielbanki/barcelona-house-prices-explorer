# SERVER ---------------------------------

server <- function(input, output, session) {



  # Color maps  ---------------------------------------------------------------------------------


  # base map for parameter-based search
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(bbox_bcn[1], bbox_bcn[2], bbox_bcn[3], bbox_bcn[4]) %>%
      addProviderTiles("CartoDB.Positron", group = "Greyscale")
  })


  # get reactive value as input that is visible

  filter_on_column <- reactive({
    slider <- switch(input$info,
      "sales_price" = input$sales_slider,
      "rent" = input$rent_slider,
      "number_flats" = input$number_flats_slider
    )

    column_to_subset <- switch(input$info,
      "sales_price" = "sale_price_flat",
      "rent" = "rent_price_flat",
      "number_flats" = "number_flats"
    )

    palette <- switch(input$info,
      "sales_price" = "Greens",
      "rent" = "Blues",
      "number_flats" = "Reds"
    )

    legend_labels <- switch(input$info,
      "sales_price" = "Sales price",
      "rent" = "Rent",
      "number_flats" = "Number of flats"
    )

    output <- list(
      slider = slider,
      column_to_subset = column_to_subset,
      palette = palette,
      legend_labels = legend_labels
    )

    return(output)
  })






  filtered_volume <- reactive({
    df <- barrios_bcn %>%
      filter(between(
        .data[[filter_on_column()$column_to_subset]],
        filter_on_column()$slider[[1]],
        filter_on_column()$slider[[2]]
      ))
  })



  pal <- reactive({
    colorNumeric(
      palette = filter_on_column()$palette,
      domain = NULL
    )
  })




  observe({
    proxy <- leafletProxy("map")


    proxy %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = filtered_volume(),
        fillColor = ~ pal()(get(filter_on_column()$column_to_subset)),
        color = "black",
        weight = 0.5,
        fillOpacity = 0.8,
        label = ~Nom_Barri,
        highlightOptions = highlightOptions(
          bringToFront = TRUE,
          sendToBack = TRUE,
          weight = 5
        )
      ) %>%
      addLegend("bottomright",
        pal = pal(), values = filtered_volume()[[filter_on_column()$column_to_subset]],
        title = filter_on_column()$legend_labels
      )
  })




  observeEvent(input$info, {
    updateTabsetPanel(session, "params", selected = input$info)
  })



  # Compare neighbourhoods  ----------------------------------------------------------------------

  clickedIds <- reactiveValues(ids = vector())

  # base map for barrio-based search
  output$map_barrio <- renderLeaflet({
    leaflet() %>%
      fitBounds(bbox_bcn[1], bbox_bcn[2], bbox_bcn[3], bbox_bcn[4]) %>%
      addProviderTiles("CartoDB.Positron", group = "Greyscale") %>%
      addPolygons(
        data = barrios_bcn,
        layerId = ~Nom_Barri,
        fillColor = "#FFF", # fill color
        color = "grey", # border color
        weight = 2,
        label = ~Nom_Barri,
        highlightOptions = highlightOptions(
          bringToFront = TRUE,
          sendToBack = TRUE,
          weight = 5
        )
      )
  })


  # -------------------------------------------------------


  observeEvent(input$map_barrio_shape_click, {
    click <- input$map_barrio_shape_click


    proxy <- leafletProxy("map_barrio")

    clickedIds$ids <- c(clickedIds$ids, click$id)



    clickedPolys <- barrios_bcn %>% filter(Nom_Barri %in% clickedIds$ids)


    if (click$id %in% clickedPolys$Codi_Barri) {
      nameMatch <- clickedPolys$Nom_Barri[clickedPolys$Codi_Barri == click$id]


      # remove the current click$id and its name match from the clickedPolys shapefile
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id]
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]




      proxy %>% removeShape(layerId = click$id)
    } else {
      proxy %>% addPolygons(
        data = clickedPolys,
        fillColor = "#000000",
        layerId = ~Codi_Barri,
        label = ~Nom_Barri,
        group = "highlighted-polygons"
      )
    }
  })


  # ------------------------------------------------------------------

  # clear polygon selection on button click

  observeEvent(input$clear, {

    proxy <- leafletProxy("map_barrio")

    proxy %>% clearGroup("highlighted-polygons")

    clickedIds$ids <- NULL
  })

  # ---------------------------------------------------------------------

  # comparison table

  comparisonServer(
    "comp_table",
    reactive(clickedIds),
    reactive(input$compare)
  )



  # -----------------------------------------------

  # start with filter + manually adjust; or just add manually


  rent_slider_polygon <- reactive({
    barrios_bcn <- barrios_bcn %>%
      filter(between(
        rent_price_flat,
        input$rent_price_slider[1],
        input$rent_price_slider[2]
      ))
  })

  sales_slider_polygon <- reactive({
    barrios_bcn <- barrios_bcn %>%
      filter(between(
        sale_price_flat,
        input$sales_price_slider[1],
        input$sales_price_slider[2]
      ))
  })





  observeEvent(input$apply_filter, {

    barrios_bcn <- dplyr::intersect(rent_slider_polygon(), sales_slider_polygon())

    if (nrow(barrios_bcn) != 0) {
      proxy <- leafletProxy("map_barrio")

      # both manual and filtered polygons have the same group
      proxy %>%
        clearGroup("highlighted-polygons") %>%
        addPolygons(
          data = barrios_bcn,
          layerId = ~Codi_Barri,
          label = ~Nom_Barri,
          group = "highlighted-polygons"
        )


      # reset highlighted polygon list
      clickedIds$ids <- NULL

      # add those polygons that were filtered
      clickedIds$ids <- barrios_bcn$Nom_Barri

    } else {
      message("no barrios were selected")
    }
  })
}
