
#########################################
############ Data preparation
#########################################


#' @export
generate_VAF_distribution_at_t <- function(x, ...)
  UseMethod("generate_VAF_distribution_at_t")


#' @export
generate_tumor_history <- function(x, ...)
  UseMethod("generate_tumor_history")


#' @export
generate_mutation_history <- function(x, ...)
  UseMethod("generate_mutation_history")


#########################################
############ Old visualizations
#########################################


#' @export
plot_VAFs_1D <- function(x, ...)
  UseMethod("plot_VAFs_1D")


#' @export
plot_VAFs_2D_tile <- function(x, ...)
  UseMethod("plot_VAFs_2D_tile")


#' @export
animate_VAFs_2D_tile <- function(mv, mut_id)
  UseMethod("animate_VAFs_2D_tile")


#########################################
############ New point visualizations
#########################################


#' @export
plot_VAFs_2D_points <- function(x, ...)
  UseMethod("plot_VAFs_2D_points")


#' @export
plot_VAFs_2D_points_interactive <- function(x, ...)
  UseMethod("plot_VAFs_2D_points_interactive")


#' @export
animate_VAFs_2D_points <- function(x, ...)
  UseMethod("animate_VAFs_2D_points")


#' @export
plot_aob_2D_points <- function(x, ...)
  UseMethod("plot_aob_2D_points")


#' @export
animate_aob_2D_points <- function(x, ...)
  UseMethod("animate_aob_2D_points")


#' @export
plot_aob_2D_points_interactive <- function(x, ...)
  UseMethod("plot_aob_2D_points_interactive")


#########################################
############ Histogram visualizations
#########################################


#' @export
plot_aob_2D_histogram <- function(x, ...)
  UseMethod("plot_aob_2D_histogram")


#' @export
animate_aob_2D_histogram <- function(x, ...)
  UseMethod("animate_aob_2D_histogram")
