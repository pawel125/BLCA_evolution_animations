

new_mapvis <- function(x) {
  stopifnot(is.list(x))
  structure(x, class = "mapvis")
}


#' Initialise mapvis
#'
#' @param params Tbl with mutation_id, and a, b, c params
#' @param vafs Tbl with 3 columns: mutation_id, region, VAF (long format)
#'
#' @return mapvis object.
#' @export
#' @importFrom  magrittr %>%
init_mapvis <- function(params, vafs) {

  validate_inputs(params, vafs)

  vafs <- vafs %>%
    select(mutation_id, region, VAF) %>%
    group_by(mutation_id) %>%
    arrange(mutation_id, desc(VAF))

  regions <- vafs$region %>% unique()

  mv <- list(
    mutation_params = params,
    mutation_vafs = vafs,
    max_age = max(params$b),
    aob_range = range(params$a / params$b),
    times = seq(0, max(params$b), length.out = 250),
    n_regions = length(regions),
    grid = create_map_grid(regions)
  )

  mv$all_regions <- create_regions_tbl(regions, mv$grid)
  mv$plot_data <- list()
  mv$offsets <- list()

  new_mapvis(mv)
}


validate_inputs <- function(params, vafs) {
  assertthat::assert_that(
    identical(
      params$mutation_id %>% unique() %>% sort(),
      vafs$mutation_id %>% unique() %>% sort()
    )
  )
}


create_map_grid <- function(regions) {
  letters <- regions %>%
    str_extract("[A-Z]") %>%
    unique() %>%
    sort()
  numbers <- regions %>%
    str_extract("[:digit:]+") %>%
    unique() %>%
    parse_integer() %>%
    sort() %>%
    rev() %>%
    as.character()
  list(columns = letters, rows = numbers)
}


create_regions_tbl <- function(regions, grid) {
  tibble(
    region = regions,
    column = str_sub(region, start = 1, end = 1) %>%
      parse_factor(levels = grid$columns),
    row = str_sub(region, start = 2) %>%
      parse_factor(levels = grid$rows)
  )
}


#' @export
generate_tumor_history.mapvis <- function(mv) {
  for (mut_id in mv$mutation_params$mutation_id) {
    mv <- generate_mutation_history(mv, mut_id)
    message(mut_id)
  }
  mv
}


#' @export
generate_mutation_history.mapvis <- function(mv, mut_id) {
  mv$plot_data[[mut_id]] <- mv$times %>%
    map(~generate_VAF_distribution_at_t(mv, mut_id, .x)) %>%
    bind_rows()
  mv$offsets[[mut_id]] <- runif(2, min = -0.4, max = 0.4) %>%
    set_names(c("x", "y"))
  mv
}


#' @export
generate_VAF_distribution_at_t.mapvis <- function(mv, mut_id, t) {
  param <- mv$mutation_params %>%
    filter(mutation_id == mut_id) %>%
    select(a, b, c) %>%
    as.list()
  param <- get_params_at_time_t(t, param$a, param$b, param$c, mv$max_age)
  VAFs <- tibble(
    region = mv$mutation_vafs %>%
      filter(mutation_id == mut_id) %>%
      arrange(desc(VAF)) %>%
      pull(region),
    VAF = generate_VAF_distribution(param$a, param$b, param$c, mv$n_regions),
    time = t,
    column = str_sub(region, start = 1, end = 1) %>%
      parse_factor(levels = mv$grid$columns),
    row = str_sub(region, start = 2) %>%
      parse_factor(levels = mv$grid$rows)
  )
  attr(VAFs, "mutation_id") <- mut_id
  attr(VAFs, "params") <- param
  attr(VAFs, "time") <- t
  VAFs
}


get_params_at_time_t <- function(t, a, b, c, age_max) {
  mutation_born_time <- age_max - b
  mutation_age_at_t <- t - mutation_born_time
  scaling_factor <- if (mutation_age_at_t > 0) mutation_age_at_t / b else 0L
  list(
    a = a * scaling_factor,
    b = b * scaling_factor,
    c = c
  )
}


generate_VAF_distribution <- function(a, b, c, length) {

  if (near(a, 0) && near(b, 0))
    return(rep(0, length))

  vafs <- vector(mode = "double", length = length)
  vafs[[1]] <- exp(a) / c
  for (i in 1:(length - 1)) {
    vafs[[i + 1]] <- exp(a) * (b / (b + a))^i * gammainc(b + a, i)[["reginc"]] / c
  }
  vafs
}


#' @export
#' @importFrom  cli cat_line
print.mapvis <- function(x, ...) {
  cat_line("Object of rnaseq_analysis class\n", col = "gray")

  cat_line("Mutations: ", nrow(x$mutation_params))
  cat_line("Regions: ", x$n_regions)
  cat_line("Tumor age: ", x$max_age)

  invisible(x)
}
