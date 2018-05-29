#' style_roc 
#' 
#' 
#' @references 
#' 
#' plotROC
#' 
#' @export

style_roc <- 
  function (
      major.breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), 
      minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)), 
      guide = TRUE, xlab = "False positive fraction", ylab = "True positive fraction", 
      theme = theme_hc) 
  {
      res <- list(
        scale_x_continuous(xlab, breaks = major.breaks, 
          minor_breaks = minor.breaks)
        , scale_y_continuous(ylab, 
          breaks = major.breaks, minor_breaks = minor.breaks)
        , theme())
      
      if (guide) {
          pcol <- theme()$panel.grid.major$colour
          if (is.null(pcol)) 
              pcol <- "white"
          res <- append(res, geom_abline(slope = 1, intercept = 0, 
              color = pcol))
      }
      res
  }