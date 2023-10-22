library(shiny)
library(DT)
library(tidyr)

ui <- fluidPage(
  DTOutput("table")
)



data <- as.data.frame(readRDS("khi2.rds") %>%
  pivot_wider(
    id_cols = "vd",
    values_from = "v_cramer",
    names_from = "vi"
  ) %>% 
  na.omit())

server <- function(input, output) {
  print(head(data))
  output$table <- renderDT({
    print("Creating datatable...")
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(searching = TRUE, paging = TRUE, ordering = TRUE),
      filter = 'top',
      class = 'cell-border stripe'
    )
    print("Applying formatStyle...")
    dt <- dt %>%
      formatStyle(
        columns = names(data)[2:ncol(data)],
        backgroundColor = styleColorBar(range(data[, 2:ncol(data)], na.rm = TRUE), c('white', 'blue'))
      )
    print("Returning datatable...")
    return(dt)
  }
  )
}


shinyApp(ui, server)
