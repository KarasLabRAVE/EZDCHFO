# A plot function that takes a data frame and returns a heatmap plot
makeHeatMap <- function(df, xTicksNum = 10){
  xLabels <- colnames(df)
  yLabels <- rownames(df)

  if(is.null(xLabels)){
    xLabels <- seq_len(ncol(df))
  }
  if(is.null(yLabels)){
    yLabels <- seq_len(nrow(df))
  }

  df$y <- yLabels
  df_long <- reshape2::melt(df, id.vars = "y", variable.name = "x", value.name = "value")
  colnames(df_long) <- c("y", "x", "value")

  ## sort df_long by rownames(ERMatReorderd)
  df_long$x <- factor(df_long$x, levels = xLabels)
  df_long$y <- factor(df_long$y, levels = rev(yLabels))
  ## show 10 time points on x-axis at most
  if (length(xLabels) > xTicksNum){
    step <- ceiling(length(xLabels) / xTicksNum)
    breaksIdx <- seq(1, length(xLabels), by = step)
    breaks <- xLabels[breaksIdx]
  } else {
    breaks <- xLabels
  }

  ggplot2::ggplot(df_long) +
    ggplot2::geom_tile(ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value)) +
    ggplot2::scale_x_discrete(labels = breaks, breaks = breaks) +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5)) +
    viridis::scale_fill_viridis(option = "turbo") +
    ggplot2::theme_minimal()
}

#' Visualization function (PLHG ratio matrix)
#'
#' @description `plotPLHGHeatmap`: plot PLHG heatmap with electrodes marked as soz colored
#'
#' @param EI EpileptogenicIndex object from \code{computeEpileptogenicIndex}
#' @param sozIndex Integer or string. A group of electrodes to mark as in the Seizure Onset Zone (SOZ)
#'
#' @return A ggplot object
#'
#' @examples
#'
#' data("pt01EcoG")
#'
#' ## sozIndex is the index of the electrodes we assume are in the SOZ
#' sozIndex <- attr(pt01EcoG, "sozIndex")
#' ## precomputed Epileptogenic Index object
#' data("pt01PLHG")
#'
#' ## plot the mean power heatmap
#' plotPLHG<-plotPLHGHeatmap(plhg=pt01PLHG,sozIndex=sozIndex)
#' plotPLHG
#' @rdname plotPLHGHeatmap
#' @export
plotPLHGHeatmap <- function(
    plhg,
    sozIndex = NULL) {
  ## TODO: make sozID an optional
  ## TODO: add plot support to PLHG
  PLHGMat <- plhg$plhg
  elecNum <- nrow(PLHGMat)
  windowNum <- ncol(PLHGMat)

  elecNames <- plhg$electrodes
  sozIndex <- checkIndex(sozIndex, elecNames)

  group1 <- sozIndex
  group2 <- setdiff(seq_len(elecNum), sozIndex)

  elecColor <- rep("blue", elecNum)
  elecColor[seq_along(group2)] <- "black"

  startTime <- plhg$startTimes
  if (is.null(startTime)) {
    xlabel <- "Time Index"
    stimes <- seq_len(windowNum)
  } else {
    xlabel <- "Time (s)"
    stimes <- startTime
  }

  rownames(PLHGMat) <- plhg$electrodes
  colnames(PLHGMat) <- stimes

  ## prepare the data.frame for visualization
  allIndex <- c(group1, group2)
  df <- as.data.frame(PLHGMat[allIndex, ])


  makeHeatMap(df) +
    ggplot2::labs(x = xlabel, y = "Electrode", size = 2) +
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(size = 6, colour = elecColor), # Adjust depending on electrodes
    )
}



