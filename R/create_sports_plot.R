utils::globalVariables(".data")


#' Create a Win Ratio Bar Plot
#'
#' This function creates a bar plot displaying the win ratios of sports teams.
#'
#' @param data A data.frame containing at least the columns wins, losses, and team.
#' @return A ggplot object representing the win ratio plot.
#' @export
create_sports_plot <- function(data) {
    if (!"win_ratio" %in% names(data)) {
        data$win_ratio <- with(data, ifelse(wins + losses == 0, NA, wins / (wins + losses)))
    }

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$team, y = .data$win_ratio, fill = .data$team)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Win Ratio of Sports Teams",
            x = "Team",
            y = "Win Ratio"
        ) +
        ggplot2::scale_fill_brewer(palette = "Pastel1") +
        ggplot2::theme(legend.position = "none")

    return(p)
}
