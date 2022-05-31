
#'
#' @param df A data frame with two columns, name and value
#'
svr_plot_bars_percentage <- function(
    df,
    category_colors = NULL,
    brewer_palette = 'RdBu',
    bar_border_col = 'black',
    plot_title = '',
    legend_title = '',
    reverse_legend = TRUE,
    bar_width = 0.75,
    lang = 'en',
    namelabel_max_nof_rows = 3,
    namelabel_max_row_length = 20,
    namelabel_ellipsis = '..',
    valuelabel_max_nof_rows = 2,
    valuelabel_max_row_length = 15,
    valuelabel_ellipsis = '..'
) {

    # Add line breaks if necessary
    namelabels <- sapply(
        as.vector( df$name ),
        svr_add_linebreaks,
        max_nof_rows = namelabel_max_nof_rows,
        max_row_length = namelabel_max_row_length,
        ellipsis = namelabel_ellipsis
    )
    # valuelabels <- sapply(
    #     as.vector( df$value ),
    #     svr_add_linebreaks,
    #     max_nof_rows = valuelabel_max_nof_rows,
    #     max_row_length = valuelabel_max_row_length,
    #     ellipsis = valuelabel_ellipsis
    # )

    p <- df %>%

        # Use the new labels (with the possible line breaks)
        mutate( name = recode( name, !!!namelabels ) ) %>%

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
            width = bar_width,
            color = bar_border_col
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

        # # Adjust fill (values)
        # scale_fill_manual(
        #     # Use the new labels (with the possible line breaks)
        #     labels = valuelabels
        # ) +

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
        svr_theme_vertical() +

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
