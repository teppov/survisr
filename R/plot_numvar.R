

svr_plot_numvar <- function(
    df.singlevar,
    box_color = 'darkgrey'
) {

    df.singlevar %>%

        pivot_longer( cols = everything() ) %>%

        ggplot( mapping = aes( x = value, y = name ) ) +

        geom_violin( fill = col_qua15['c5'] ) +

        geom_jitter( alpha = 0.1 ) +

        geom_boxplot(
            color = box_color,
            # Make the box transparent
            alpha = 0,
            # Use width to fit the box inside the violin
            width = 0.5
        ) +

        stat_summary(
            fun = 'mean',
            geom = 'point',
            color = 'red'
        ) +

        svr_theme_vertical() +

        theme(
            # Remove the axis labels
            axis.title = element_blank(),
            strip.text = element_blank()
        )
}


svr_plot_numvar_facet <- function(
    df,
    varnames,
    box_color = 'darkgrey'
) {

    df %>%

        select( all_of( varnames ) ) %>%

        svr_plot_numvar() +

        facet_wrap(
            ~name,
            scales = 'free',
            ncol = 1,
            strip.position = 'bottom'
        )
}
