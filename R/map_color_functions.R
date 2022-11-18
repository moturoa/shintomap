

#' Returns a function to make colors from a numeric vector (n distinct values)
#' @param vals A numeric vector
#' @param palette_function A palette function from the `pals` package
#' @param method If bin, cuts the numeric vector in equal parts. If quantile,
#' cuts it in equal quantile parts (so that each bin contains ca. equal number of observations)
#' @param na.color Color for NA values (default white)
#' @export
#' @importFrom leaflet colorBin colorQuantile
#' @importFrom  utils getFromNamespace
#' @importFrom grDevices rgb
binned_numeric_map_color <- function(vals, n = 20,
                                     palette_function = "viridis",
                                     colors = NULL,
                                     reverse = FALSE,
                                     method = c("bin","quantile"),
                                     pretty = TRUE,
                                     na.color = grDevices::rgb(0,0,0,0), ...){

  method <- match.arg(method)

  if(is.null(colors)){
    colors <- get_pals_colors(palette_function, n)
  } else if(length(colors) < n){
    stopifnot(length(colors)>0)
    warning("not enough colors provided; recycling")
    colors <- rep(colors, times = ceiling(n / length(colors))+1)[1:n]
  }


  if(method == "quantile"){
    out <- leaflet::colorQuantile(colors,
                                  domain = c(0,max(vals, na.rm = TRUE)),
                                  n = n,
                                  na.color = na.color,
                                  reverse = reverse)
  }

  if(method == "bin"){
    out <- leaflet::colorBin(colors,
                             domain = c(min(vals, na.rm = TRUE),
                                        max(vals, na.rm = TRUE)),
                             bins = n,
                             reverse = reverse,
                             pretty = pretty,
                             na.color = na.color)
  }

  return(out)
}


#f <- binned_numeric_map_color(buurten_data$a_inw, n = 4)


# Util. Get color function from the 'pals' package
get_pals_colors <- function(palette_function, n){
  # Get palette function from pals package, get colors for each of vals.
  palfun <- utils::getFromNamespace(palette_function, "pals")
  palfun(n)
}


#' Map colors for a factor variable
#' @export
factor_map_color <- function(vals, colors = NULL,
                             palette_function = "viridis",
                             reverse = FALSE,
                             na.color = grDevices::rgb(0,0,0,0),
                             ...
){

  if(!is.factor(vals)){
    vals <- as.factor(vals)
  }

  val_uniq <- levels(vals)
  n <- nlevels(vals)

  if(is.null(colors)){
    colors <- get_pals_colors(n = n, palette_function = palette_function)
  }

  if(reverse){
    colors <- rev(colors)
  }

  colors <- setNames(colors, val_uniq)

  function(value){
    ii <- match(value, names(colors))
    out <- colors[ii]
    out[is.na(out)] <- na.color
    unname(out)
  }


}

#' Map colors, auto
#' @export
shinto_auto_color <- function(vals, ...){


  if(is.numeric(vals)){
    binned_numeric_map_color(vals = vals, ...)
  } else {
    factor_map_color(vals = vals, ...)
  }

}


