library(ggplot2)
library(grid)
library(png)

# load the image
img <- readPNG("inst/extdata/sueredo.png")
spc <- readRDS("inst/extdata/aecpedons.rds")[4, ]
lab <- soilDB::fetchLDM(spc$pedlabsampnum)

depth_min <- -2
depth_max <- 185
prop_min <- 65
prop_max <- 95
prop_breaks <- 6
w <- 1

# data prep
spc <- aqp::glom(spc, depth_min, depth_max)
lab <- aqp::glom(lab, depth_min, depth_max)
a <- aqp::spc2mpspline(lab, "sand_total")
my_data <-
  data.frame(
    x_var = a$sand_total_spline,
    y_var = rowMeans(a[, , .TOP, .BOTTOM])
  )
my_data$x_var[is.na(my_data$x_var)] <- 76.5

# set plot dimensions and aspect ratio
x_axis_limits <- pretty(range(my_data$x_var, na.rm = TRUE),
                        n = 0, min.n = 0)
xidx <- c(which.min(x_axis_limits), which.max(x_axis_limits))
x_axis_limits <- x_axis_limits[xidx]

y_axis_limits <- pretty(range(my_data$y_var, na.rm = TRUE))
yidx <- c(which.min(y_axis_limits), which.max(y_axis_limits))
y_axis_limits <- y_axis_limits[yidx]

img_aspect_ratio <- dim(img)[1] / diff(y_axis_limits) # pixels per vertical cm
dat_aspect_ratio <- dim(img)[2] / diff(x_axis_limits) # pixels per prop unit
plot_ratio <- img_aspect_ratio / dat_aspect_ratio

img_bounds <- mean(x_axis_limits) + (diff(x_axis_limits) / c(-2, 2))
pimg_bounds <- pretty(img_bounds)
pimg_bounds <- pimg_bounds[c(which.min(pimg_bounds), which.max(pimg_bounds))]

p <- ggplot(data = my_data, aes(x = x_var, y = y_var)) +
      theme_minimal() +
      annotation_raster(img, xmin = pimg_bounds[1],
                             xmax = pimg_bounds[2],
                             ymin = -depth_min,
                             ymax = -depth_max) +
      theme(
            panel.ontop = TRUE,
            panel.grid.major = element_line(linetype = "dashed",
                                            color = rgb(1, 1, 1, 0.5)),
            panel.grid.minor = element_blank(),
            axis.ticks.y = element_blank()) +
      geom_path(linetype = "longdash",
                color = "yellow",
                linewidth = 1) +
      scale_x_continuous(limits = c(prop_min, prop_max),
                         n.breaks = prop_breaks) +
      scale_y_reverse() +
      ylim(-y_axis_limits) +
      xlab("Total Sand, %") +
      ylab("Depth, cm") +
      scale_y_reverse() +
      coord_fixed(ratio = plot_ratio)
p
ggsave("man/images/sueredo.jpg", p, device = "jpeg",
       height = 11, width = 8.5, units = "in", dpi = 144)
