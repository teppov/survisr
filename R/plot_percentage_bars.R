# Internal function for getting labels for the
# percentage bar plot in the right language
svr_get_percentagelabel <- function(
    lang = 'en'
) {
    lbl = list(
        'en' = 'Percentage',
        'fi' = 'Prosenttiosuus'
    )
    lbl[[lang]]
}

svr_plot_percentagebars <- function(
    df,
    category_colors = NULL,
    brewer_palette = 'RdBu',
    plot_title = '',
    legend_title = '',
    reverse_legend = TRUE,
    bar_width = 0.75,
    lang = 'en'
) {

    p <- df %>%

        # Initialize ggplot by mapping variables to the x axis
        ggplot( mapping = aes( x = name ) ) +

        # Set the plot title
        ggtitle( plot_title ) +

        # Plot as bars
        geom_bar(
            # Map the values to the fill colour of the bars
            aes( fill = value ),
            # Scale the bars to 1.0, i.e. show proportions
            position = 'fill',
            # Change the width of the bars
            width = bar_width
        ) +

        # Adjust x axis (variable names)
        scale_x_discrete(
            # No need for label
            name = '',
            # Expand the bars to the whole plot area
            expand = c( 0, 0 )
        ) +

        # Adjust y axis
        scale_y_continuous(
            # Change label
            name = svr_get_percentagelabel( lang ),
            # Set the tick marks to 0.1 spacing
            breaks = seq( from = 0, to = 1, by = 0.2 ),
            minor_breaks = seq( from = 0, to = 1, by = 0.1 ),
            # Set the tick marks to correspond percentage
            labels = seq( from = 0, to = 100, by = 20 ),
            # Expand the bars to the whole plot area
            expand = c( 0, 0 )
        ) +

        # Modify legend
        guides(
            fill = guide_legend(
                title = legend_title,
                # Reverse the legend to get same order as in the plot
                reverse = reverse_legend
            )
        ) +

        # Remove visual clutter from the plot
        # theme_minimal() +
        svr_theme_v_varnames() +

        # Flip the coordinates to plot horizontal bars
        coord_flip()

    if( !is.null( category_colors ) ) {
        # Set the colours for the categories manually
        p + scale_fill_manual(
            values = category_colors,
            drop = FALSE
        )

    } else if( !is.null( brewer_palette ) ) {
        # Set a brewer color palette
        # https://ggplot2.tidyverse.org/reference/scale_brewer.html
        p + scale_fill_brewer(
            palette = brewer_palette,
            drop = FALSE
        )

    } else {
        p
    }
}
