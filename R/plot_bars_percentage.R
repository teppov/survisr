

#' Plots percentage bars for each variable in the "name" column
#' using the values in the "value" column of the given data frame.
#'
#' @param df A data frame with two columns: name and value
#' @param category_colors
#' @param brewer_palette
#' @param bar_border_col
#' @param plot_title
#' @param legend_title
#' @param percentage_label
#' @param percentage_suffix
#' @param reverse_legend
#' @param bar_width
#' @param namelabel_max_nof_rows
#' @param namelabel_max_row_length
#' @param namelabel_ellipsis
#'
#' @return
#' @export
#'
#' @examples
svr_plot_bars_percentage <- function(
        df,
        category_colors = NULL,
        brewer_palette = 'RdBu',
        bar_border_col = 'black',
        plot_title = element_blank(),
        legend_title = element_blank(),
        percentage_label = 'Percentage',
        percentage_suffix = '%',
        reverse_legend = TRUE,
        bar_width = 0.75,
        namelabel_max_nof_rows = 3,
        namelabel_max_row_length = 20,
        namelabel_ellipsis = '..'
        # valuelabel_max_nof_rows = 2,
        # valuelabel_max_row_length = 15,
        # valuelabel_ellipsis = '..'
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

        # Initialize ggplot by mapping variables to the y axis
        # and the values to the fill colour of the bars
        ggplot( mapping = aes( y = name, fill = value ) ) +

        # Set the plot title
        ggtitle( plot_title ) +

        # Plot as bars
        geom_bar(
            # Scale the bars to 1.0, i.e. show proportions
            position = 'fill',
            width = bar_width,
            color = bar_border_col
        ) +

        # Adjust x axis
        scale_x_continuous(
            name = percentage_label,
            labels = scales::label_percent( suffix = percentage_suffix ),
            # Expand the bars to the whole plot area
            expand = c( 0, 0 )
        ) +

        # Adjust y axis (variable names)
        scale_y_discrete(
            # No need for label
            name = '',
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

        # Apply custom theme
        svr_theme_vertical()

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

