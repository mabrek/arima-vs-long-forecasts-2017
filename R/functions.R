library(ggplot2)
library(zoo)
library(xts)
library(forecast)
library(TSPred)
library(scales)
library(shiny)
library(dygraphs)
library(multitaper)
library(lubridate)


image_device <- function(name, width=image_width, height=image_height) {
    svg(paste("../img/", name, ".svg", sep=""), width, height)
}

lsd <- function(pos = 1) {
  names(grep("^function$", 
             sapply(ls(pos = pos), function(x) {mode(get(x))}),
             value = T, 
             invert = T))
}

multiplot <- function(metrics, limit = 15, vline = NA) {
  data <- metrics
  if (length(colnames(data)) == 0) {
    colnames(data) <- 1:ncol(data)
  }
  data <- data[,
               which(sapply(data, function(v) {any(!is.na(v))})),
               drop = FALSE]
  r <- nrow(data)
  n <- ncol(data)
  i <- 1
  k <- min(n, limit)
  repeat {
    m <- data[, i:k, drop = FALSE]
    labels <- make.unique(colnames(m))
    df <- data.frame(index(m)[rep.int(1:r, ncol(m))],
                     factor(rep(1:ncol(m), each = r), levels = 1:ncol(m),
                            labels = labels),
                     as.vector(coredata(m)))
    names(df) <- c("Index", "Series", "Value")
    p <- ggplot(data = df) + geom_path(aes(x = Index, y = Value), na.rm = TRUE) + 
      xlab(NULL) + ylab(NULL) + facet_grid(Series ~ ., scales = "free_y") + 
      theme(strip.text.y = element_text(angle = 0))
    if (!is.na(vline)) {
      p <- p + geom_vline(xintercept = as.numeric(index(metrics)[vline]), colour = "red")
    }
    print(p)
    if (k >= n) {
      break
    } else {
      choice <- readline("empty line to continue, anything else to stop :")
      if (choice == "") {
        i <- i + limit
        k <- min(n, k + limit)
      } else {
        break
      }
    }
  }
}

sameplot <- function(metrics, ...) {
  autoplot(metrics, facet = NULL, ...)
}


shinyplot <- function(metrics, limit = 100, breakpoints = data.frame(), vline = c()) {
  data <- metrics
  if (length(colnames(data)) == 0) {
    colnames(data) <- 1:ncol(data)
  }
  data <- data[,
               which(sapply(data, function(v) {any(!is.na(v))})),
               drop = FALSE]
  data <- data[, 1:min(limit, ncol(data))]
  bp <- breakpoints
  app <- shinyApp(
    ui = fluidPage(
      lapply(
        1:ncol(data),
        function(n) {
          # TODO column name filter
          fluidRow(
            style = "padding-bottom: 5px;",
            column(
              dygraphOutput(paste("graph_series_", n, sep = ""),
                            height = "100px"),
              width = 6),
            column(
              textOutput(paste("text_series_", n, sep = "")),
              width = 6))
        })
      ),
    server = function(input, output) {
      lapply(
        1:ncol(data),
        function(n) {
          single <- data[, n, drop = FALSE]
          output[[paste("graph_series_", n, sep = "")]] <- renderDygraph({
            d <- dygraph(single, group = "series") %>%
              dyLegend(show = "never") %>%
              dyOptions(drawXAxis = (n %% 9 == 0) | (n == ncol(data)))
            Reduce(function(g, b) {
                     g %>% dyEvent(b)
                   },
                   append(bp[bp$name == colnames(single), ][["time"]], vline),
                   d)
          })
          output[[paste("text_series_", n, sep = "")]] <- renderText({
            colnames(single)
          })
          
        })
    })
  runApp(app)
}
