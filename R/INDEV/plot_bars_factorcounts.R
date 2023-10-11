

svr_plot_bars_factorcount <- function(
    df.singlevar
) {

    df.singlevar %>%

        # Handle everything as integers to get all to the same scale
        mutate( across( everything(), as.integer ) ) %>%

        pivot_longer( everything() ) %>%

        # Treat the values as factors to get discrete x axis in plot
        mutate( value = factor( value ) ) %>%

        # Initialize ggplot with the values in x axis
        ggplot( mapping = aes( x = value ) ) +

        # Plot bars with the "light2" colour from the qualitative palette
        geom_bar( fill = col_qua15['light2'] ) +

        # Show the distribution of responses in rows
        facet_grid(
            rows = 'name',
            # Switch the y tick labels to the right side
            switch = 'y'
        ) +

        # Remove unnecessary breaks from the y axis
        scale_y_continuous( breaks = NULL ) +

        labs( x = 'Category level' ) +

        # Remove visual clutter from the plot
        theme_minimal() +

        theme(
            # Remove the y axis labels
            axis.title.y = element_blank(),
            # Rotate the y axis tick labels horizontally
            strip.text.y.left = element_text( angle = 0 )
        )
}


svr_plot_bars_factorcount_facet <- function(
    df,
    varnames
) {

    df %>%

        # Select all categorical variables
        select( all_of( varnames ) ) %>%

        svr_plot_bars_factorcount() +

        # Show the distribution of responses in rows
        facet_grid(
            rows = 'name',
            # Switch the y tick labels to the right side
            switch = 'y'
        )
}

