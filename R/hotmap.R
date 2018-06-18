# Plotting heatmaps
# ==================
# small helper functions
# ==================
colours <- function() {
  c("deepskyblue2", "white", "orangered2")
}


colours2 <- function() {
  # http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=11
  # note [11:1] to get blue (low) to red (high)
    c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')[11:1]
}

#' Tick (break) positions
#'
#' @param levels chr vector of (x or y) variables in correct order
#' @param by tick spacer length
#'
#' @return chr vector of break/tick positions to be displayed
#' @export
#'
break_breaks <- function(levels, by=50) {
  # levels are the axis labels,
  # in the order they appear on plot
  levels[c(rep(FALSE, by - 1), TRUE)]
}

#' Tick labels
#'
#' @param until length of levels vector
#' @param by tick spacer length
#'
#' @return chr vector of tick labels to be displayed
#' @export
#'
label_breaks <- function(until, by=50) {
  # until is an integer
  as.character(seq(by, until, by))
}
# ==================
# big helper functions
# ==================
#' Breaks and labels
#'
#' @param levels vector of (x or y) variables in correct axis order
#' @param breaks when break positions unchanged, same as positions in <levels>
#' @param labels when labels unchanged, same as names in <levels>
#' @param by tick spacer length
#' @param labels.numeric replace labels with numbering
#'
#' @return list of two character vectors: breaks and labels
#' @export
#'
hotmap_break_label <- function(levels, breaks, labels, by=50, labels.numeric=FALSE) {

  if (is.null(by) && isTRUE(labels.numeric)) {
    labels <- label_breaks(until=length(levels), by=1)
  }
  else if (!is.null(by) && !isTRUE(labels.numeric)) {
    breaks <- break_breaks(levels=levels, by=by)
  }
  else if (!is.null(by) && isTRUE(labels.numeric)) {
    breaks <- break_breaks(levels=levels, by=by)
    labels <- label_breaks(until=length(levels), by=by)
  }

  return(list(breaks=breaks, labels=labels))
}

#' Ordering matrix
#'
#' @param mat matrix to be plotted
#' @param x.order if chr vect: order with this. if TRUE: generate order. if F: do nothing.
#' @param y.order if chr vect: order with this. if TRUE: generate order. if F: do nothing.
#' @param order if FALSE, do not generate orders.
#'
#' @return (ordered) matrix
#' @export
hotmap_order <- function(mat, x.order=TRUE, y.order=TRUE, order=TRUE) {
  # Order the data

  if (order == FALSE) {
    x.order=FALSE
    y.order=FALSE
  }

  orders <- list(x.order, y.order)

  if (any(sapply(orders, isTRUE)) || any(sapply(orders, is.character))) {
    if (is.character(x.order)) mat <- mat[, x.order]
    else if (isTRUE(x.order)) mat <- mat[, statistrics::hcorder(mat)]

    if (is.character(y.order)) mat <- mat[y.order, ]
    else if (isTRUE(y.order)) mat <- mat[statistrics::hcorder(t(mat)), ]
  }

  mat
}

# Dependencies
#' @importFrom reshape2 melt
#' @export
reshape2::melt
# ==================
# data prep function
# ==================
#' Data preparation for plotting
#'
#' @param mat matrix to be plotted
#' @param x.order if chr vect: order with this. if TRUE: generate order. if F: do nothing.
#' @param y.order if chr vect: order with this. if TRUE: generate order. if F: do nothing.
#' @param order if FALSE, do not generate orders.
#'
#' @return (ordered) melted matrix
#' @export
#'
hotmap_prep <- function(mat,
                        x.order=TRUE,
                        y.order=TRUE,
                        order=TRUE) {
  # Order data
  mat <- hotmap_order(mat=mat, x.order=x.order, y.order=y.order, order=order)

  # Tidy data
  mat <- reshape2::melt(t(mat))

  mat
}

# ==================
# epic package function
# ==================
#' Plot hotmap
#' @param mat matrix to be plotted
#'
#' @param melted TRUE if matrix in tidy format
#' @param x axis
#' @param y axis
#' @param fill value
#' @param x.order order of variables on x axis
#' @param y.order order of variables on x axis
#' @param order do not x.order or y.order if FALSE
#' @param limits colour limits
#' @param cols colours
#' @param title plot
#' @param x.breaks tick mark positions
#' @param x.labels tick mark labels
#' @param x.name axis label
#' @param x.labels.by tick spaces
#' @param x.labels.numeric convert labels to numbering
#' @param y.breaks tick mark positions
#' @param y.labels tick mark labels
#' @param y.name axis label
#' @param y.labels.by tick spaces
#' @param y.labels.numeric convert labels to numbering
#' @param title.hjust adjust title position. 0.5 is middle
#' @param legend.position hide or display key and if so where
#' @param panel.border show plot border, if so which
#' @param margins border space around plot: c(x,x,x,x)
#' @param ratio aspect ratio of x and y axis
#' @param x.ticksize tick label size
#' @param y.ticksize tick label size
#' @param bl.label add text on plot bottom left
#' @param br.label add text on plot bottom right
#' @param tl.label add text on plot top left
#' @param tr.label add text on plot top right
#' @param label.size text size
#'
#' @import ggplot2
#' @export
hotmap <- function(mat,
                   melted=FALSE,
                   return.data=FALSE,
                   x=NULL,
                   y=NULL,
                   fill=NULL,
                   x.order=TRUE,
                   y.order=TRUE,
                   order=TRUE,
                   limits=c(-10,10),
                   cols=colours(),
                   title="",
                   x.breaks=waiver(),
                   x.labels=waiver(),
                   x.name=NULL,
                   x.labels.by=NULL,
                   x.labels.numeric=FALSE,
                   y.breaks=waiver(),
                   y.labels=waiver(),
                   y.name=NULL,
                   y.labels.by=NULL,
                   y.labels.numeric=FALSE,
                   title.hjust=0.5,
                   legend.position="none",
                   panel.border=element_blank(),
                   margins=c(0,0,0,0),
                   ratio=1,
                   x.ticksize=6,
                   y.ticksize=6,
                   bl.label="",
                   br.label="",
                   tl.label="",
                   tr.label="",
                   label.size=0.1){

  # Check / Prepare data
  if (!isTRUE(melted)) {
    mat <- hotmap_prep(mat=mat, x.order=x.order, y.order=y.order, order=order)
  }
  if (is.null(x)) x <- mat$Var1
  if (is.null(y)) y <- mat$Var2
  if (is.null(fill)) fill <- mat$value

  # Breaks and labels
  x.bl <- hotmap_break_label(levels=levels(x),
                             breaks=x.breaks,
                             labels=x.labels,
                             by=x.labels.by,
                             labels.numeric=x.labels.numeric)
  x.breaks <- x.bl$breaks
  x.labels <- x.bl$labels
  y.bl <- hotmap_break_label(levels=levels(y),
                             breaks=y.breaks,
                             labels=y.labels,
                             by=y.labels.by,
                             labels.numeric=y.labels.numeric)
  y.breaks <- y.bl$breaks
  y.labels <- y.bl$labels

  # Plot heatmap
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data=mat, aes(x=x, y=y, fill=fill)) +
    ggplot2::scale_fill_gradient2(limits=limits,
                                  low=cols[1],
                                  mid=cols[2],
                                  high=cols[3],
                                  oob=scales::squish) +
    ggplot2::scale_x_discrete(name=x.name, breaks=x.breaks, labels=x.labels) +
    ggplot2::scale_y_discrete(name=y.name, breaks=y.breaks, labels=y.labels) +
    ggplot2::ggtitle(label=title) +
    ggplot2::theme(aspect.ratio=ratio,
                   plot.title=element_text(hjust=title.hjust),
                   axis.text.y=element_text(size=y.ticksize),
                   axis.text.x=element_text(size=x.ticksize),
                   plot.margin=unit(margins, "pt"),
                   legend.position=legend.position,
                   panel.border=panel.border) +
    ggplot2::geom_text(aes(x=-Inf, y=-Inf, hjust=0, vjust=0), label=bl.label, size=label.size) +
    ggplot2::geom_text(aes(x=Inf, y=-Inf, hjust=1, vjust=0), label=br.label, size=label.size) +
    ggplot2::geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1), label=tl.label, size=label.size) +
    ggplot2::geom_text(aes(x=Inf, y=Inf, hjust=1, vjust=1), label=tr.label, size=label.size)

  if (isTRUE(return.data)) {
    return(list(plot=p, data=mat))
  } else {
    p
  }
}


# ==================
# epic package function
# ==================
#' Plot hotmap2
#' @param mat matrix to be plotted
#'
#' @param melted TRUE if matrix in tidy format
#' @param x axis
#' @param y axis
#' @param fill value
#' @param x.order order of variables on x axis
#' @param y.order order of variables on x axis
#' @param order do not x.order or y.order if FALSE
#' @param limits colour limits
#' @param cols colours
#' @param title plot
#' @param x.breaks tick mark positions
#' @param x.labels tick mark labels
#' @param x.name axis label
#' @param x.labels.by tick spaces
#' @param x.labels.numeric convert labels to numbering
#' @param y.breaks tick mark positions
#' @param y.labels tick mark labels
#' @param y.name axis label
#' @param y.labels.by tick spaces
#' @param y.labels.numeric convert labels to numbering
#' @param title.hjust adjust title position. 0.5 is middle
#' @param legend.position hide or display key and if so where
#' @param panel.border show plot border, if so which
#' @param margins border space around plot: c(x,x,x,x)
#' @param ratio aspect ratio of x and y axis
#' @param x.ticksize tick label size
#' @param y.ticksize tick label size
#' @param bl.label add text on plot bottom left
#' @param br.label add text on plot bottom right
#' @param tl.label add text on plot top left
#' @param tr.label add text on plot top right
#' @param label.size text size
#'
#' @import ggplot2
#' @export
hotmap2 <- function(mat,
                   melted=FALSE,
                   return.data=FALSE,
                   x=NULL,
                   y=NULL,
                   fill=NULL,
                   x.order=TRUE,
                   y.order=TRUE,
                   order=TRUE,
                   limits=c(-10,10),
                   cols=colours2(),
                   title="",
                   x.breaks=waiver(),
                   x.labels=waiver(),
                   x.name=NULL,
                   x.labels.by=NULL,
                   x.labels.numeric=TRUE,
                   y.breaks=waiver(),
                   y.labels=waiver(),
                   y.name=NULL,
                   y.labels.by=NULL,
                   y.labels.numeric=TRUE,
                   title.hjust=0.5,
                   legend.position="none",
                   panel.border=element_blank(),
                   margins=c(5.5, 5.5, 0, 5.5),
                   ratio=1,
                   x.ticksize=6,
                   y.ticksize=6,
                   bl.label="",
                   br.label="",
                   tl.label="",
                   tr.label="",
                   label.size=0.1){

  # Check / Prepare data
  if (!isTRUE(melted)) {
    mat <- hotmap_prep(mat=mat, x.order=x.order, y.order=y.order, order=order)
  }
  if (is.null(x)) x <- mat$Var1
  if (is.null(y)) y <- mat$Var2
  if (is.null(fill)) fill <- mat$value

  # Breaks and labels
  x.bl <- hotmap_break_label(levels=levels(x),
                             breaks=x.breaks,
                             labels=x.labels,
                             by=x.labels.by,
                             labels.numeric=x.labels.numeric)
  x.breaks <- x.bl$breaks
  x.labels <- x.bl$labels
  y.bl <- hotmap_break_label(levels=levels(y),
                             breaks=y.breaks,
                             labels=y.labels,
                             by=y.labels.by,
                             labels.numeric=y.labels.numeric)
  y.breaks <- y.bl$breaks
  y.labels <- y.bl$labels

  # Plot heatmap
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data=mat, aes(x=x, y=y, fill=fill)) +
    ggplot2::scale_fill_gradientn(colours = cols,
                                  limits = limits,
                                  oob = scales::squish) +
    ggplot2::scale_x_discrete(name=x.name, breaks=x.breaks, labels=x.labels) +
    ggplot2::scale_y_discrete(name=y.name, breaks=y.breaks, labels=y.labels) +
    ggplot2::ggtitle(label=title) +
    ggplot2::theme(aspect.ratio=ratio,
                   plot.title=element_text(hjust=title.hjust),
                   axis.text.y=element_text(size=y.ticksize),
                   axis.text.x=element_text(size=x.ticksize),
                   plot.margin=unit(margins, "pt"),
                   legend.position=legend.position,
                   panel.border=panel.border) +
    ggplot2::geom_text(aes(x=-Inf, y=-Inf, hjust=0, vjust=0), label=bl.label, size=label.size) +
    ggplot2::geom_text(aes(x=Inf, y=-Inf, hjust=1, vjust=0), label=br.label, size=label.size) +
    ggplot2::geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1), label=tl.label, size=label.size) +
    ggplot2::geom_text(aes(x=Inf, y=Inf, hjust=1, vjust=1), label=tr.label, size=label.size)

  if (isTRUE(return.data)) {
    return(list(plot=p, data=mat))
  } else {
    p
  }
}
