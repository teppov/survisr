

svr_plot_violin <- function(
    df,
    varnames
) {

    df %>%

        select( all_of( varnames ) ) %>%

        # Drop all rows with even one NA value
        # (remove this if you want see also NA values)
        # drop_na() %>%

        # Create a plot with `year` in the x axis and `age` in the y axis
        ggplot(
            mapping = aes( x = year, y = age )
        ) +

        # Remove visual clutter from the plot
        theme_minimal() +

        scale_x_continuous(
            # Set the major grid lines to the year values
            breaks = unique( sort( df$year ) ),
            # Remove minor grid lines
            minor_breaks = NULL
        )


    p +
        geom_violin( mapping = aes( group = year ) )

}


