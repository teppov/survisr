

svr_plot_histogram <- function(
    df.singlevar,
    binwidth = NULL
) {

    df.singlevar %>%

        pivot_longer( cols = everything() ) %>%

        # Create plot by mapping value to the x axis
        ggplot( mapping = aes( x = value ) ) +

        # Plot the histogram
        # (pick a color for the bars from the qualitative palette)
        geom_histogram(
            binwidth = binwidth,
            fill = col_qua15['c5'],
            color = 'black'
        ) +

        # Remove visual clutter from the plot
        theme_minimal()
}


svr_plot_histogram_facet <- function(
    df,
    varnames,
    binwidth = NULL
) {

    df %>%

        select( all_of( varnames ) ) %>%

        svr_plot_histogram( binwidth = binwidth ) +

            facet_wrap(
                ~name,
                scales = 'free',
                ncol = 1,
                strip.position = 'bottom'
            ) +

            theme(
                # Remove the x axis labels
                axis.title.x = element_blank()
            )
}

