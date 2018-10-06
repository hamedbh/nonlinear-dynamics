library(Rcpp)
cppFunction('NumericVector log_map_cpp (NumericVector x, double r) {
            int n = x.size(), i = 0; n--;
            for (; i < n; i++) x[i + 1] = r * x[i] * (1 - x[i]);
            return x;
            }')

logistic_map <- function(x, # starting condition, 
                         r, # rate parameter, 
                         N  # number of iterations
) {
    results <- numeric(length = N + 1)
    results[1] <- x
    log_map_cpp(results, r)
    data.frame(i = c(0, seq_len(N)), 
               x = results)
}

plot_logistic_map <- function(x, # starting condition, 
                              r, # rate parameter, 
                              N  # number of iterations
){
    plot_subtitle <- paste0("x0 = ", 
                            x, 
                            "; r = ", 
                            r)
    d <- logistic_map(x = x, r = r, N = N)
    
    ggplot2::ggplot(d, ggplot2::aes(i, x)) + 
        ggplot2::geom_point() + 
        ggplot2::theme_minimal() + 
        ggplot2::scale_y_continuous(limits = c(0, 1), 
                                    breaks = seq(0, 1, by = 0.2)) + 
        ggplot2::ggtitle(label = "Logistic Map", subtitle = plot_subtitle)
}
