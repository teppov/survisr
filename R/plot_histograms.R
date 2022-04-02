

svr_plot_histograms <- function(
    df,
    varnames,
    binwidth = NULL
) {

    df %>%

        select( all_of( varnames ) ) %>%

        pivot_longer( cols = everything() ) %>%

        # Create plot by mapping height to the x axis
        ggplot( mapping = aes( x = value ) ) +

        # Plot the histogram
        # (pick a color for the bars from the qualitative palette)
        geom_histogram(
            binwidth = binwidth,
            fill = col_qua15['c5'],
            color = 'black'
        ) +

        facet_wrap(
            ~name,
            scales = 'free',
            ncol = 1,
            strip.position = 'bottom'
        ) +

        # Remove visual clutter from the plot
        theme_minimal() +

        theme(
            # Remove the x axis labels
            axis.title.x = element_blank()
        )
}

