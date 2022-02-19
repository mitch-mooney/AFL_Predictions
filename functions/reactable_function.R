reactable_function <- function(data){
  
  options(reactable.theme = reactableTheme(
    color = "hsl(233, 9%, 87%)",
    backgroundColor = "hsl(233, 9%, 19%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  ))
  
  col_order <- c("Rank", "change", "Player",
                 "Rating", "sparkline")
  spark_table <- spark_table[, col_order]
  
  return(spark_table)
}

render_table <- function(spark_table){
  
  # Icon to indicate trend: unchanged, up, down, or new
  trend_indicator <- function(change = c("Unchanged", "Up", "Down")) {
    value <- match.arg(change)
    label <- switch(change,
                    Unchanged = "Unchanged", Up = "Trending up",
                    Down = "Trending down")
    
    # Add img role and tooltip/label for accessibility
    args <- list(role = "img", title = change)
    
    if (value == "Unchanged") {
      args <- c(args, list(shiny::icon("minus"), style = "color: #666; font-weight: 700"))
    } else if (value == "Up") {
      args <- c(args, list(shiny::icon("caret-up"), style = "color: #1ed760"))
    } else if (value == "Down") {
      args <- c(args, list(shiny::icon("caret-down"), style = "color: #cd1a2b"))
    } else {
      args <- c(args, list(shiny::icon("circle"), style = "color: #2e77d0; font-size: 10px"))
    }
    do.call(span, args)
  }
  
  reactable(spark_table,defaultSorted = list(Rank= "asc"), pagination = FALSE, defaultPageSize = 20,  
            rowStyle = function(index) {
              if (spark_table[index, "Rank"] > 8) {
                list(background = "hsl(233, 9%, 45%)")
              }
            },
            rowClass = function(index) {
              if (spark_table[index, "Rank"] < 9) {
                "bold"
              }
            }, columns = list(
              Rank = colDef(maxWidth = 75, align = "center"),
              change = colDef(
                header = span("", class = "sr-only"),
                sortable = FALSE,
                align = "left",
                width = 20,
                cell = function(change) trend_indicator(change)
              ),
              Player = colDef(name = "Team", maxWidth = 150, align = "center", cell = function(value) {
                img_src <- knitr::image_uri(sprintf("images/%s.png", value))
                image <- img(src = img_src, height = "45px", alt = value)
                tagList(
                  div(style = list(display = "inline-block", width = "150px"), image)
                )
              }),
              Rating = colDef(maxWidth = 100, align = "center", format = colFormat(digits = 0)),
              
              sparkline = colDef(name = "2021 Progress", cell = function(value, index) {
                sparkline(spark_table$sparkline[[index]], lineWidth = 2, height = "50px", width = "150px")
              })
              
            ))
}
