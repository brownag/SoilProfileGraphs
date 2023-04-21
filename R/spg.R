#' spg
#'
#' soil profile property graphs with 'calibrated' pit photo backdrop.
#'
#' @param spc SoilProfileCollection
#' @param filename filename
#' @param prop character. Property column name
#' @param destfile Optional: output filename
#' @param depth_min Minimum image depth (cm)
#' @param depth_max Maximum image depth (cm)
#' @param prop_min Property minimum value
#' @param prop_max Property maximum value
#' @param spline Default: `FALSE`
#' @param midpoint Default: `FALSE` If `spline=FALSE` use midpoints of horizons?
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param no_data_value Replacement value for `NA` in `prop`
#' @param panel_ontop Default: `TRUE`
#' @param prop_linetype Default: `longdash`
#' @param prop_linewidth Default: `1`
#' @param prop_color Default: `"yellow"`
#' @param prop_breaks Default: `6`
#' @importFrom grDevices rgb
#' @return ggplot plot object; also shown on graphics pane as side-effect.
#' @export
#' @examplesIf !inherits(requireNamespace("soilDB", quietly=TRUE), 'try-error')
#' @examples
#' \donttest{
#' library(soilDB)
#'
#' spc <- readRDS(system.file("extdata", "aecpedons.rds",
#'                    package = "SoilProfileGraphs"))[4, ]
#' lab <- soilDB::fetchLDM(spc$pedlabsampnum)
#'
#' spg(
#'   system.file("extdata",
#'               "sueredo.png",
#'               package = "SoilProfileGraphs"),
#'   spc = lab,
#'   prop = "sand_total",
#'   depth_max = 185,
#'   prop_min = 65,
#'   prop_max = 95
#' )
#' }
spg <-
  function(spc,
           filename,
           prop,
           destfile = NULL,
           depth_min = 0,
           depth_max = 200,
           prop_min = NULL,
           prop_max = NULL,
           spline = FALSE,
           midpoint = FALSE,
           xlab = prop,
           ylab = "Depth, cm",
           no_data_value = NULL,
           panel_ontop = TRUE,
           prop_linetype = "longdash",
           prop_linewidth = 1,
           prop_color = "yellow",
           prop_breaks = 6
  ) {
    .BOTTOM <- NULL
    .TOP <- NULL
    x_var  <- NULL
    y_var  <- NULL

    # Load the image
    img <- png::readPNG(filename)
    spc <- aqp::glom(spc, depth_min, depth_max)

    if (try(requireNamespace("mpspline2", quietly = TRUE)) &&
        (isTRUE(spline) || isFALSE(midpoint))) {
      spc <- aqp::spc2mpspline(spc, prop)
      if (isTRUE(spline)) {
        prop <- paste0(prop, "_spline")
      }
    }

    .data <- data.frame(
      x_var = spc[[prop]],
      y_var = rowMeans(spc[, , .TOP, .BOTTOM])
    )

  if (!is.null(no_data_value))
    .data$x_var[is.na(.data$x_var)] <- no_data_value

  xlim <- pretty(range(.data$x_var, na.rm = TRUE),
                          n = 0, min.n = 0)
  xidx <- c(which.min(xlim), which.max(xlim))
  xlim <- xlim[xidx]

  ylim <- pretty(range(.data$y_var, na.rm = TRUE),
                          n = 0, min.n = 0)
  yidx <- c(which.min(ylim), which.max(ylim))
  ylim <- ylim[yidx]

  img_aspect_ratio <- dim(img)[2] / dim(img)[1]
  dat_aspect_ratio <-  diff(xlim) / diff(ylim)
  plot_ratio <- mean(c(img_aspect_ratio, dat_aspect_ratio))

  img_bounds <- mean(xlim) + (diff(xlim) / c(-2, 2))
  pimg_bounds <- pretty(img_bounds)
  pimg_bounds <- pimg_bounds[c(which.min(pimg_bounds), which.max(pimg_bounds))]

  suppressMessages({
    p <- ggplot2::ggplot(data = .data, ggplot2::aes(x = x_var, y = y_var)) +
      ggplot2::theme_minimal() +
      ggplot2::annotation_raster(img, xmin = pimg_bounds[1],
                        xmax = pimg_bounds[2],
                        ymin = -depth_min,
                        ymax = -depth_max) +
      ggplot2::theme(
        panel.ontop = TRUE,
        panel.grid.major = ggplot2::element_line(linetype = "dashed",
                                                 color = grDevices::rgb(1, 1, 1, 0.5)),
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ) +
      ggplot2::geom_path(linetype = prop_linetype,
                         color = prop_color,
                         linewidth = prop_linewidth) +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_x_continuous(limits = c(prop_min, prop_max),
                                  n.breaks = prop_breaks) +
      ggplot2::ylim(-ylim) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_fixed(ratio = plot_ratio)
  })

  # TODO: expose file output options
  if (!is.null(destfile)) {
    ggplot2::ggsave(destfile, p, device = "jpeg",
           height = 11, width = 8.5, units = "in", dpi = 144)
  }
  p
}
