
# to put extra info buttons in UI

infoButton <- function(id) {
  bsButton(id,
           label = "",
           icon = icon("question-circle"),
           style = "info",
           size = "extra-small")
  }

  

# Popover text for extra info button

infoText <- function(id, text) {
  bsPopover(id,
            title = "What happens in this step?", 
            content = text, 
            placement = "right",
            trigger = "hover")
}


create_infoButton <- function(id, text){
  
 button <-  shinyBS::bsButton(id,
           label = "",
           icon = icon("question-circle"),
           style = "info",
           size = "extra-small")


popover <- shinyBS::bsPopover(id,
          title = "What happens in this step?", 
          content = text, 
          placement = "right",
          trigger = "hover")
  
  
  return(HTML(paste0(button, "\n", popover)))
  
}

# UI function to create tabPanels for tabPanelSet
choropleth_tabPanels <- function(tabpanel_id, slider_id, slider_text, df_column) {
  tabPanel(
    tabpanel_id,
    sliderInput(slider_id, slider_text,
                min = min(barrios_bcn[[df_column]], na.rm = TRUE),
                max = max(barrios_bcn[[df_column]], na.rm = TRUE),
                value = c(
                  min(barrios_bcn[[df_column]], na.rm = TRUE),
                  max(barrios_bcn[[df_column]], na.rm = TRUE)
                )
    )
  )
}




