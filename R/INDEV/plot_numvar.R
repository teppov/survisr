

svr_plot_numvar <- function(
    df.singlevar,
    box_color = 'darkgrey',
    violin_color = '#6db6ff',
    mean_color = '#db6d00',
    point_alpha = 0.1,
    box_width = 0.5
) {

    df.singlevar %>%

        pivot_longer( cols = everything() ) %>%

        ggplot( mapping = aes( x = value, y = name ) ) +

        geom_violin( fill = violin_color ) +

        geom_jitter( alpha = point_alpha ) +

        geom_boxplot(
            color = box_color,
            # Make the box transparent
            alpha = 0,
            # Fit the box inside the violin
            width = box_width
        ) +

        stat_summary(
            fun = 'mean',
            geom = 'point',
            color = mean_color
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
    box_color = 'darkgrey',
    violin_color = '#6db6ff',
    mean_color = '#db6d00',
    point_alpha = 0.1,
    box_width = 0.5
) {

    df %>%

        select( all_of( varnames ) ) %>%

        svr_plot_numvar(
            box_color = box_color,
            violin_color = violin_color,
            mean_color = mean_color,
            point_alpha = point_alpha,
            box_width = box_width
        ) +

        facet_wrap(
            ~name,
            scales = 'free',
            ncol = 1,
            strip.position = 'bottom'
        )
}
