

svr_plot_bars_divergingpercentage <- function(
    df,
    neg_categories,
    pos_categories,
    select_vars = everything(),
    keep_categories = NULL,
    var_labels = NULL,
    category_labels = NULL,
    category_colors = NULL,
    brewer_palette = 'RdBu',
    hline_color = 'gray95',
    plot_title = '',
    legend_title = '',
    reverse_legend = FALSE,
    bar_width = 0.75,
    lang = 'en'
) {

    # category_levels <- c( pos_categories, neg_categories )

    # Should categories be recoded?
    if( !is.null( category_labels ) ) {

        neg_categories <- category_labels[neg_categories]
        pos_categories <- category_labels[pos_categories]

        # Ensure that the keys of the colours match data
        if( !is.null( category_colors ) ) {
            names( category_colors ) <- category_labels
        }
    }

    p <- df %>%

        svr_pivot_longer(
            select_vars = {{ select_vars }},
            keep_categories = keep_categories,
            dropna = TRUE,
            var_labels = var_labels,
            category_labels = category_labels
        ) %>%

        # Use only the defined categories
        # mutate( value = factor( value, levels = category_levels ) ) %>%

        # drop_na() %>%

        # Count the responses in a category for each variable
        count( name, value ) %>%

        group_by( name ) %>%

        # Calculate the percentages (grouped by the variables),
        # add positive percentages for "agree" categories,
        # sum them into one variable, and
        # add negative percentages for "disagree" categories
        mutate(
            pr = ( n / sum(n) ) * 100,
            pr_plus = if_else(
                value %in% pos_categories,
                pr, NA_real_
            ),
            pr_plus_sum = sum( pr_plus, na.rm = TRUE ),
            pr_minus = if_else(
                value %in% neg_categories,
                -pr, NA_real_
            )
        ) %>%

        arrange( pr_plus_sum ) %>%

        ungroup() %>%

        mutate(
            name = factor(
                name, levels = unique( name ), ordered = TRUE
            )
            # ),
            # variable = recode( variable, !!!varlabels ),
            # value = recode( value, !!!category_labels )
        ) %>%

        ggplot(
            mapping = aes(
                x = name,
                fill = value
            )
        ) +

        ggtitle( plot_title ) +

        # Plot stacked bars
        geom_bar(
            mapping = aes( y = pr_plus ),
            stat = 'identity'
        ) +
        geom_bar(
            mapping = aes( y = pr_minus ),
            stat = 'identity',
            # Reverse the order of the stack
            # to get the categories in the right order in the plot
            position = position_stack( reverse = TRUE )
        ) +

        # Add a horizontal line to separate the diverging bars
        geom_hline( yintercept = 0, color = c( hline_color ) ) +

        # Remove label from the variable axis (x)
        scale_x_discrete( name = NULL ) +

        # Adjust y axis
        scale_y_continuous(
            # Change label
            name = svr_get_percentagelabel( lang ),
            # Set the tick marks to 0.1 spacing
            limits = c( -100, 100 ),
            breaks = seq( from = -100, to = 100, by = 20 ),
            minor_breaks = seq( from = -100, to = 100, by = 10 ),
            # Set the tick mark labels (left to right)
            # first from 100 to 0 and then from 20 to 100
            labels = c(
                seq( from = 100, to = 0, by = -20 ),
                seq( from = 20, to = 100, by = 20 )
            ),
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
        theme_minimal() +

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

