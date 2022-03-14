
#############################################
######         1-D plot
#############################################


#' @export
plot_VAFs_1D.mapvis <- function(mv, mut_id, t = last(mv$times)) {
  mv$plot_data[[mut_id]] %>%
    filter(near(time, t)) %>%
    mutate(region = parse_factor(region, levels = region)) %>%
    ggplot(aes(region, VAF)) +
    geom_point() +
    labs(title = mut_id, subtitle = str_c("time: ", format(t, digits = 1)))
}


#############################################
######         2-D tile plots
#############################################


#' @export
plot_VAFs_2D_tile.mapvis <- function(mv, mut_id, t = last(mv$times),
                                     fill_limits = c(0, 1)) {
  mv$plot_data[[mut_id]] %>%
    filter(near(time, t)) %>%
    ggplot_VAFs_2D_tile(fill_limits) +
    labs(title = mut_id, subtitle = t)
}


#' @export
animate_VAFs_2D_tile.mapvis <- function(mv, mut_id, fill_limits = NULL) {
  mv$plot_data[[mut_id]] %>%
    ggplot_VAFs_2D_tile(fill_limits) +
    transition_time(time) +
    labs(
      title = mut_id,
      subtitle = "Time: {format(frame_time, digits = 1)}"
    )
}


ggplot_VAFs_2D_tile <- function(data, fill_limits) {
  ggplot(data, aes(column, row, fill = VAF)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red", limits = fill_limits) +
    theme(
      panel.background = element_rect(fill = "gray93", colour = "gray93")
    )
}


#############################################
######         2-D point plots
#############################################


#' @export
plot_VAFs_2D_points_interactive.mapvis <- function(mv, mut_ids, t = last(mv$times),
                                              VAF_treshold = 0.01) {
  mv %>%
    plot_VAFs_2D_points(mut_ids, t, VAF_treshold) %>%
    plotly::ggplotly() %>%
    plotly::layout(showlegend = FALSE)
}


#' @export
plot_VAFs_2D_points.mapvis <- function(mv, mut_ids, t = last(mv$times),
                                       VAF_treshold = 0.01) {
  mv %>%
    prepare_VAF_point_data_2D(mut_ids, VAF_treshold) %>%
    filter(near(time, t)) %>%
    ggplot_mut_points_2D(
      map_data = mv$all_regions,
      size_var = "VAF",
      size_limits = c(0, 1)
    ) +
    labs(title = str_c("Time: ", format(t, digits = 1)))
}


#' @export
animate_VAFs_2D_points.mapvis <- function(mv, mut_ids,
                                          start = first(mv$times),
                                          end = last(mv$times),
                                          VAF_treshold = 0.01) {
  mv %>%
    prepare_VAF_point_data_2D(mut_ids, VAF_treshold) %>%
    filter(time >= start, time <= end) %>%
    ggplot_mut_points_2D(
      map_data = mv$all_regions,
      size_var = "VAF",
      size_limits = c(0, 1)
    ) +
    transition_time(time) +
    theme(legend.position = "none") +
    labs(title = "Time: {format(frame_time, digits = 1)}")
}


prepare_VAF_point_data_2D <- function(mv, mut_ids, VAF_treshold) {
  mut_offsets <- mv$offsets[mut_ids] %>%
    bind_rows(.id = "mutation_id")
  mut_data <- mv$plot_data[mut_ids] %>%
    bind_rows(.id = "mutation_id") %>%
    filter(VAF > VAF_treshold) %>%
    left_join(mut_offsets, by = "mutation_id") %>%
    mutate(
      mutation_id = parse_factor(mutation_id, levels = mut_ids),
      new_x = as.integer(column) + x,
      new_y = as.integer(row) + y
    )
  mut_data
}


ggplot_mut_points_2D <- function(mut_data, map_data, size_var, size_limits) {
  map_data <- map_data %>%
    mutate(sequenced = "yes") %>%
    complete(column, row) %>%
    mutate(sequenced = replace_na(sequenced, "no"))
  ggplot(map_data) +
    geom_tile(aes(column, row, fill = sequenced)) +
    scale_fill_manual(values = c(yes = "white", no = "gray93")) +
    geom_point(
      aes(new_x, new_y, size = !!sym(size_var), color = mutation_id),
      alpha = 0.75,
      data = mut_data
    ) +
    scale_size_area(limits = size_limits) +
    scale_color_discrete(drop = FALSE) +
    theme(
      panel.background = element_rect(fill = "gray93", colour = "gray93")
    )
}


#############################################
######         2-D a/b plots
#############################################


#' @export
plot_aob_2D_points_interactive.mapvis <- function(mv, mut_ids, t = last(mv$times),
                                              VAF_treshold = 0.01) {
  mv %>%
    plot_aob_2D_points(mut_ids, t, VAF_treshold) %>%
    plotly::ggplotly() %>%
    plotly::layout(showlegend = FALSE)
}


#' @export
plot_aob_2D_points.mapvis <- function(mv, mut_ids, t = last(mv$times),
                                      VAF_treshold = 0.01) {
  mv %>%
    prepare_aob_2D_data(mut_ids, VAF_treshold) %>%
    filter(near(time, t)) %>%
    ggplot_mut_points_2D(
      map_data = mv$all_regions,
      size_var = "aob",
      size_limits = mv$aob_range
    ) +
    labs(title = str_c("Time: ", format(t, digits = 1)))
}


#' @export
animate_aob_2D_points.mapvis <- function(mv, mut_ids,
                                         start = first(mv$times),
                                         end = last(mv$times),
                                         VAF_treshold = 0.01) {
  mv %>%
    prepare_aob_2D_data(mut_ids, VAF_treshold) %>%
    filter(time >= start, time <= end) %>%
    ggplot_mut_points_2D(
      map_data = mv$all_regions,
      size_var = "aob",
      size_limits = mv$aob_range
    ) +
    transition_time(time) +
    theme(legend.position = "none") +
    labs(title = "Time: {format(frame_time, digits = 1)}")
}


prepare_aob_2D_data <- function(mv, mut_ids, VAF_treshold) {
  mv %>%
    prepare_VAF_point_data_2D(mut_ids, VAF_treshold) %>%
    left_join(mv$mutation_params, by = "mutation_id") %>%
    mutate(aob = a/b)
}


#############################################
######         2-D a/b histogram
#############################################


#' @export
plot_aob_2D_histogram.mapvis <- function(mv, mut_ids, t = last(mv$times),
                                         VAF_treshold = 0.01) {
  mv %>%
    prepare_aob_2D_data(mut_ids, VAF_treshold) %>%
    filter(near(time, t)) %>%
    ggplot_aob_histograms_2D(aob_range = mv$aob_range) +
    labs(title = str_c("Time: ", format(t, digits = 1)))
}


#' @export
animate_aob_2D_histogram.mapvis <- function(mv, mut_ids,
                                            start = first(mv$times),
                                            end = last(mv$times),
                                            VAF_treshold = 0.01) {
  fake_rows <- mv$all_regions %>%
    mutate(aob = 1, time = start)
  mv %>%
    prepare_aob_2D_data(mut_ids, VAF_treshold) %>%
    filter(time >= start, time <= end) %>%
    # transition_time() fails if there is no? too few? points in some regions
    # addind 1 'fake' row per region solves the problem
    bind_rows(fake_rows) %>%
    ggplot_aob_histograms_2D(aob_range = mv$aob_range) +
    transition_time(time) +
    labs(title = "Time: {format(frame_time, digits = 1)}")
}


ggplot_aob_histograms_2D <- function(dt, aob_range) {
  dt %>%
    mutate(row = fct_rev(row)) %>%
    ggplot(aes(aob)) +
    geom_histogram() +
    facet_grid(row ~ column, switch = "y") +
    theme(axis.text.y = element_blank()) +
    scale_x_continuous(limits = aob_range, trans = "log10")
}
