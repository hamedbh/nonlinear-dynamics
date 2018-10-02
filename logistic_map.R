logistic_map <- function(x, # starting condition, 
                         r, # rate parameter, 
                         N  # number of iterations
) {
    results <- numeric(length = N + 1)
    results[1] <- x
    for (i in seq_len(N)) {
        results[i + 1] <- r * results[i] * (1 - results[i])
    }
    
    data.frame(i = c(0, seq_len(N)), 
               x = results)
}

plot_logistic_map <- function(x, # starting condition, 
                              r, # rate parameter, 
                              N  # number of iterations
){
    plot_title <- paste0("Logistic Map: x0 = ", 
                         x, 
                         "; r = ", 
                         r)
    
    results <- numeric(length = N + 1)
    results[1] <- x
    for (i in seq_len(N)) {
        results[i + 1] <- r * results[i] * (1 - results[i])
    }
    
    d <- data.frame(i = c(0, seq_len(N)), 
                    x = results)
    
    ggplot2::ggplot(d, ggplot2::aes(i, x)) + 
        ggplot2::geom_point() + 
        ggplot2::theme_minimal() + 
        ggplot2::scale_y_continuous(limits = c(0, 1), 
                                    breaks = seq(0, 1, by = 0.2)) + 
        ggplot2::ggtitle(plot_title)
}
