calculate_summary_stats <- function(data, variables) {
    result <- data.frame()
    for (variable in variables) {
        stats <- data.frame(
            Variable = variable,
            Number = length(data[[variable]]),
            Mean = mean(data[[variable]]) %>% round(3),
            Median = median(data[[variable]] %>% round(3)),
            Q1 = quantile(data[[variable]], probs = 0.25) %>% round(3),
            Q3 = quantile(data[[variable]], probs = 0.75)%>% round(3),
            SD = sd(data[[variable]]) %>% round(3),
            Min = min(data[[variable]]) %>% round(3),
            Max = max(data[[variable]]) %>% round(3)
        )
        result <- rbind(result, stats)
        rownames(result) <- NULL
    }
    return(result)
}


calculate_summary_stats2 <- function(data, variables) {
    result <- data.frame()
    for (variable in variables) {
        stats <- data.frame(
            Variable = variable,
            Number = length(data[[variable]]),
            Categories_number <- unique(data[[variable]]) %>% length,
            Most_frequent_category <- names(table(data[[variable]]))[which.max(table(data[[variable]]))],
            Most_frequent_count <- max(table(data[[variable]]))
            #Categories <- unique(data[[variable]])
        )
        names(stats) <- c("Variable","Number", "Categories_number", "Most_frequent_category", "Most_frequent_count")
        result <- rbind(result, stats)
        rownames(result) <- NULL
    }
    return(result)
}