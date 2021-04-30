
ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Barcelona house prices explorer",
  id = "nav",
  tabPanel(
    "About project",
  includeHTML(paste0(here(), "/www/about_project.html"))
  ),
  tabPanel(
    "Color maps",

    sidebarLayout(
      sidebarPanel(
        selectInput("info", "Based on which factor should we create the map?",
          choices = list(
            "Sales price" = "sales_price",
            "Rent" = "rent",
            "Number of flats" = "number_flats"
          )
        ),
        tabsetPanel(
          id = "params",
          type = "hidden",
          choropleth_tabPanels(
            tabpanel_id = "number_flats",
            slider_id = "number_flats_slider",
            slider_text = "Number of flats",
            df_column = "number_flats"
          ),
          choropleth_tabPanels(
            tabpanel_id = "sales_price",
            slider_id = "sales_slider",
            slider_text = "Sales price (EUR)",
            df_column = "sale_price_flat"
          ),
          choropleth_tabPanels(
            tabpanel_id = "rent",
            slider_id = "rent_slider",
            slider_text = "Monthly rent (EUR)",
            df_column = "rent_price_flat"
          )
        )
      ),
      mainPanel(
        leafletOutput("map", width = "100%"),
        verbatimTextOutput("view")
      )
    )
  ),
  tabPanel(
    "Compare neighbourhoods",
    tags$head(
      includeCSS(paste0(here(), "/www/styles.css"))
    ),
    useShinyalert(),
    fixedRow(
      column(
        4,
        wellPanel(
          tags$h4(
            "Step 1: Filter & highlight",
            create_infoButton("step1", paste(
              "Use the sliders to select the range of rental and sales prices you are interested in. <br> <br>",
              "The prices are average prices for a given area, not prices for individual properties. <br> <br>",
              "If you move both sliders, then an area will only be highlighted if its prices are within both slider ranges.",
              sep = ""
            ))
          ),
          sliderInput("rent_price_slider", "Choose rent range (EUR)",
            min = 0, max = max(barrios_bcn$rent_price_flat, na.rm = TRUE),
            value = c(0, max(barrios_bcn$rent_price_flat, na.rm = TRUE))
          ),
          sliderInput("sales_price_slider", "Choose sales price range (EUR)",
            min = 0, max = max(barrios_bcn$sale_price_flat, na.rm = TRUE),
            value = c(0, max(barrios_bcn$sale_price_flat, na.rm = TRUE))
          ),
          actionButton("apply_filter", "Show on map"),
          actionButton("clear", "Clear highlighted areas")
        ),
        wellPanel(
          tags$h4(
            "Step 2: Manually adjust",
            create_infoButton(
              "step2",
              paste(
                "To ensure you can compare all areas you are interested in (and only those), you can manually add and remove individual areas directly on the map. <br> <br>",
                "Clicking on an area once allows you to select or unselect it.",
                sep = ""
              )
            )
          )
        ),
        wellPanel(
          tags$h4(
            "Step 3: Generate table",
            infoButton("step3"),
            infoText(
              "step3",
              "Now that you have selected all the areas you want to compare, you are ready to generate your comparison table."
            )
          ),

          actionButton("compare", "Generate table ", width = "100%")
        )
      ),
      column(
        8,
        leafletOutput("map_barrio", height = "667.6px") # manually looked up
      )
    ),
    fixedRow(
      comparisonUI("comp_table")
    )
  )
)
