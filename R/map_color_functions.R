

#' Returns a function to make colors from a numeric vector (n distinct values)
#' @description See [shintoMapModule()] on how to use this function inside a shintomap map module.
#' @param vals A numeric vector
#' @param n Number of colors to generate (in binned_numeric_map_color)
#' @param palette_function A palette function from the `pals` package ("kovesi.rainbow","parula")
#' @param colors Optional, a vector of colors (then, colors are not taken from a palette function)
#' @param reverse If TRUE, reverses the colors generated from the palette
#' @param method If bin, cuts the numeric vector in equal parts. If quantile,
#' cuts it in equal quantile parts (so that each bin contains ca. equal number of observations)
#' @param pretty If TRUE, uses an internal prettyizing function for the bin breaks
#' @param bins_predefined If provided (optional), do not make the bins but use these bins. See wbm3.0 for usage.
#' @param na.color Color for NA values (default white)
#' @param ... Further args passed from shinto_auto_color to binned_numeric_map_color or factor_map_color
#' @export
#' @rdname map_colors
#' @importFrom leaflet colorBin colorQuantile
#' @importFrom  utils getFromNamespace
#' @importFrom grDevices rgb
#' @importFrom stats setNames
binned_numeric_map_color <- function(vals,
                                     n = 20,
                                     palette_function = "viridis",
                                     colors = NULL,
                                     reverse = FALSE,
                                     method = c("bin","quantile","predefined"),
                                     pretty = TRUE,
                                     bins_predefined = NULL,
                                     na.color = grDevices::rgb(0,0,0,0), ...){

  method <- match.arg(method)

  if(is.null(colors)){
    colors <- get_pals_colors(palette_function, n)
  } else if(method != "predefined" && length(colors) < n){
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

  if(method == "predefined"){

    if(is.null(colors)){
      stop("with method = predefined, also predefine the colors!")
    }

    out <- function(values, ...){
      bins <- bins_predefined

      # convention: predfined bins do not inlclude maximum.
      # add it here.
      maxval <- ceiling(10*max(vals, na.rm = TRUE))/10

      if(maxval > max(bins, na.rm = TRUE)){
        bins <- c(bins, maxval)
      }

      ii <- findInterval(values, bins, all.inside = FALSE)
      lookup_colors <- c(na.color, colors)
      lookup_colors[ii + 1]  # so that 0 (no bin found) is the first, which is the na.color

    }

  }

  return(out)
}




# Util. Get color function from the 'pals' package
get_pals_colors <- function(palette_function, n){
  # Get palette function from pals package, get colors for each of vals.
  palfun <- utils::getFromNamespace(palette_function, "pals")
  palfun(n)
}


#' Map colors for a factor variable
#' @param color_other Color for bin not in bins_predefined
#' @rdname map_colors
#' @export
factor_map_color <- function(vals,
                             colors = NULL,
                             palette_function = "viridis",
                             reverse = FALSE,
                             color_other = "#D3D3D3",
                             na.color = grDevices::rgb(0,0,0,0),
                             bins_predefined = NULL,
                             method = c("auto","predefined"),
                             ...
){

  method <- match.arg(method)

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

  if(method == "auto"){
    colors <- stats::setNames(colors, val_uniq)
  } else {
    colors <- stats::setNames(colors, bins_predefined)
  }

  function(value){
    ii <- match(value, names(colors))
    out <- colors[ii]
    out[is.na(value)] <- na.color
    out[is.na(out)] <- color_other
    unname(out)
  }


}

#' Map colors, auto
#' @param force_factor If TRUE, treats a numeric column as if it is a factor
#' @rdname map_colors
#' @export
#' @examples
#'  vals <- runif(100,0,100)
#'  bins <- c(0,10,25,50)
#'  colors <- c("green","red","blue","black")
#'
#'  fun <- shinto_auto_color(vals, bins = bins, method = "predefined", colors = colors)
#'
#'  fun(vals)
shinto_auto_color <- function(vals, ..., force_factor = FALSE){

  if(is.numeric(vals) & !force_factor){
    binned_numeric_map_color(vals = vals, ...)
  } else {
    factor_map_color(vals = vals, ...)
  }

}





if(FALSE){


  vals <- runif(100,0,100)
  bins <- c(0,10,25,50)
  colors <- c("green","red","blue","black")

  fun <- shinto_auto_color(vals, bins = bins, method = "predefined", colors = colors)

  fun(vals)



  vals <- sample(LETTERS[1:3],20, replace = T)

  colors <- c("green","red","blue")

  fun <- shinto_auto_color(vals, bins = bins, method = "predefined", colors = colors)

  fun(vals)
}
