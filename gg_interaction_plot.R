#' ---
#' title: gg_interaction()
#' description: inspired by 'Linear Models with R' by Julian Faraway (see page 243)
#' author: nathancday@gmail.com
#' ---

library(tidyverse)

data(warpbreaks)
plot(breaks ~ wool, warpbreaks)
with(warpbreaks, interaction.plot(wool, tension, breaks))

gg_interaction_plot <- function(data, formula) {
    
    formula <- as.formula(formula)
    
    y_var <- as.character(formula[2])
    
    x_vars <- as.character(formula[3]) %>%
        str_split(" \\+ ") %>% unlist()
    
    data <- mutate_at(data, x_vars, as.factor)
    
    shp_vars <- rev(x_vars)

    map2(x_vars, shp_vars,
         ~ ggplot(data, aes_(y = as.name(y_var), x = as.name(..1), shape = as.name(..2))) +
             geom_point(position = position_jitter(width = .1)) +
             stat_summary(fun.y = "mean", geom = "line",
                          aes_(group = as.name(..2), linetype = as.name(..2))) +
             scale_shape_manual(values = 15:25) +
             theme(legend.position = "top", legend.direction = "horizontal")) %>%
        cowplot::plot_grid(plotlist = ., ncol = 2)
        
}

gg_interaction_plot(warpbreaks, breaks ~ wool + tension )

data(pvc, package("faraway"))

gg_interaction_plot(pvc, psize ~ operator + resin)
