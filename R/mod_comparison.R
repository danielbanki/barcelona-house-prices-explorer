# module: generate comparison table


comparisonUI <- function(id) {
  tagList(
    reactableOutput(NS(id, "comparison_table")),
    
  )
}

comparisonServer <- function(id, clicked, compare_button) {
  moduleServer(id, function(input, output, session) {
    
    # create df from sf df
    st_geometry(barrios_bcn) <- NULL
    
    # base table
    output$comparison_table <- renderReactable({
      reactable(
        barrios_bcn %>% 
          select(Nom_Barri,
                 Nom_Districte,
                 sale_price_flat,
                 sale_price_m2,
                 sale_volume,
                 rent_price_flat,
                 rent_price_m2,
                 rent_volume,
                 price_to_rent,
                 number_flats
                 ), 
        defaultColDef = colDef(na = "-"),
        columns = list(
          Nom_Barri = colDef(name = "Area name"),
          Nom_Districte = colDef(name = "District name"),
          sale_price_flat = colDef(name = "Average sales price",
                                   format = colFormat(prefix = "€",
                                                      separators = TRUE
                                   )),
          sale_price_m2 = colDef(name = "Average sales price / m2",
                                 format = colFormat(prefix = "€",
                                                    separators = TRUE
                                                    )),
          sale_volume = colDef(name = "Yearly sales"),
          rent_price_flat = colDef(name = "Average monthly rent",
                                   format = colFormat(prefix = "€",
                                                      separators = TRUE
                                   )),
          rent_price_m2 = colDef(name = "Average monthly rent / m2",
                                 format = colFormat(prefix = "€",
                                                    separators = TRUE
                                 )),
          rent_volume = colDef(name = "Yearly rental contracts"), 
          price_to_rent = colDef(name = "Price-to-rent ratio"),
          number_flats = colDef(name = "Number of flats")
        ), 
        defaultPageSize = nrow(barrios_bcn), 
        searchable = TRUE, 
        bordered = TRUE, 
        striped = TRUE, 
        highlight = TRUE,
        height = 500,
        theme = reactableTheme(
          stripedColor = "#f6f8fa"
        )
      )
      
    })
    
    
    observeEvent(compare_button(), {
      
      barrios_compared <- barrios_bcn %>% filter(Nom_Barri %in% clicked()$ids)
      
      updateReactable("comparison_table", 
                      data = barrios_compared)
      
    })
  })
}