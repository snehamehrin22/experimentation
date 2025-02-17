# ===== Visual Style Constants =====
league_fill <- "#d9b554"
bar_plot_width <- 0.7
density_plot_alpha <- 0.5

# ===== Data Quality Functions =====
initial_data_check <- function(data) {
   
    
    # View the data and check the structure
    cat("\nData Head:\n")
    print(head(data))
    
    # Summary statistics
    cat("\nData Summary:\n")
    print(summary(data))
    
    
    # Check for missing values
    cat("\nMissing Values:\n")
    print(colSums(is.na(data)))
    
    # Check for duplicate rows
    cat("\nDuplicate Rows:\n")
    print(any(duplicated(data)))

    
    
    cat("\nData Quality Checks Completed.\n")
}

# ===== Univariate Analysis Functions =====

#' Performs univariate exploratory data analysis
#' @param data The dataset to analyze
#' @param colname The column name to analyze
#' @return For numeric variables: list of histogram, boxplot, and quantile table
#'         For categorical variables: bar plot
univariate_eda <- function(data, colname) {
    # Convert colname to a symbol for tidy evaluation
    col_sym <- sym(colname)
    
    # Get the column data properly
    col_data <- data[[colname]]
    
    # Clean and format the column name for display
    col_cleaned <- str_to_title(str_replace_all(colname, "_", " "))
    title <- glue("Distribution Of Users By {col_cleaned}")
    
    # Add count and facet_label to the data
    data <- data %>%
        mutate(
            total_users = n_distinct(user_id),
            facet_label = glue("Unique Users ({total_users})"))
    
    # Check if the data type is character or factor
    if (is.character(col_data) || is.factor(col_data)) {
        data %>%
            group_by({{col_sym}}, total_users, facet_label) %>%
            summarize(unique_users = n_distinct(user_id)) %>%
            ungroup() %>%
            mutate(
                pct_users = unique_users / total_users,
                # Only reorder if it's not already an ordered factor
                col_grouped = if (is.ordered(col_data)) {
                    !!col_sym
                } else {
                    fct_reorder(!!col_sym, unique_users)
                }
            ) %>%
            ggplot(aes(x = col_grouped, y = unique_users)) +
            geom_col(fill = league_fill, width = bar_plot_width) +
            theme_tq() +
            geom_text(aes(label = glue("{scales::percent(round(pct_users, 2))} (n = {unique_users})")), 
                     vjust = -0.5, size = 2.5) +
            labs(title = title, x = col_cleaned) +
            theme(title = element_text(size = 8)) +
            facet_wrap(~facet_label)
        
        # Check if the data type is integer or numeric
    } else if (is.numeric(col_data)) {
        # Create histogram visualization
        hist_viz <- data %>%
            ggplot(aes(x = {{col_sym}})) +
            geom_density(fill = league_fill, alpha = density_plot_alpha) +
            theme_tq() +
            theme(title = element_text(size = 8)) +
            facet_wrap(~facet_label)
        
        # Create boxplot visualization
        box_viz <- data %>%
            ggplot(aes(y = {{col_sym}})) +
            geom_boxplot(fill = league_fill, alpha = density_plot_alpha) +
            theme_tq() +
            theme(title = element_text(size = 8)) +
            facet_wrap(~facet_label)
        
        # Create quantile table
        quant_tbl <- data %>%
            summarize(
                q25 = quantile({{col_sym}}, probs = 0.25, na.rm = TRUE),
                q50 = quantile({{col_sym}}, probs = 0.5, na.rm = TRUE),
                q75 = quantile({{col_sym}}, probs = 0.75, na.rm = TRUE)
            )
        
        # Check if the distribution is skewed and if so, plot on log scale
        skewness <- abs(skewness(data[[colname]]))
        if (skewness > 1) {
          hist_viz <- hist_viz + scale_x_log10() +
            labs(x = paste0(col_cleaned, " (log scale)"))
          box_viz <- box_viz + scale_y_log10() +
            labs(y = paste0(col_cleaned, " (log scale)"))
        }
        else {
          hist_viz <- hist_viz + labs(x = col_cleaned)
          box_viz <- box_viz + labs(y = col_cleaned)
          
        }
        
        # Return the list of visualizations and table
        return(list(hist_viz, box_viz, quant_tbl))
    }
}

#' Wrapper function that saves univariate analysis plots
#' @param data The dataset to analyze
#' @param variable The variable name to analyze
#' @return The plot results and saves the plot to disk
univariate_eda_with_save <- function(data, variable) {
    plot_result <- univariate_eda(data, variable)
    plot_name <- paste0("plots/univariate_analysis_by_", variable, ".png")
    ggsave(plot_name, plot_result[[1]], width = 10, height = 6)
    return(plot_result)
}

#' Performs univariate analysis on all columns in the dataset
#' @param data The dataset to analyze
#' @return List of EDA results for each column
eda_univariate_all_columns <- function(data) {
    results <- map(colnames(data), function(colname) {
        create_eda_univariate_func(data, colname)
    })
}

# ===== Bivariate Analysis Functions =====

#' Performs bivariate exploratory data analysis
#' @param data The dataset to analyze
#' @param explanatory_var The explanatory variable
#' @param response_var The response variable
#' @return Visualization based on variable types:
#'         - Categorical vs Categorical: Bar plot
#'         - Numeric vs Categorical: Density plot, boxplot, and quantile table
#'         - Numeric vs Numeric: Scatter plot
bivariate_eda <- function(data, explanatory_var, response_var) {
    # Convert column names to symbols
    explanatory_var_sym <- sym(explanatory_var)
    response_var_sym <- sym(response_var)
    
    # Determine the data type of response_var and handle ordered factors
    response_var_type <- class(rlang::eval_tidy(response_var_sym, data))[1]  # Take first class
    explanatory_var_type <- class(rlang::eval_tidy(explanatory_var_sym, data))[1]  # Take first class
    
    # Clean column name for the title label
    response_var_cleaned <- str_to_title(str_replace_all(response_var, "_", " "))
    title <- glue("Distribution Of Users By {response_var_cleaned}")
    
    # Modified type checking to include ordered factors
    is_categorical <- function(x) x %in% c("character", "factor", "ordered")
    is_numeric <- function(x) x %in% c("numeric", "integer")
    
    if (is_categorical(response_var_type) && is_categorical(explanatory_var_type)) {
        # Create bar plot for categorical response_var
        viz <- data %>%
            group_by({{response_var_sym}}) %>%
            mutate(total_users = n_distinct(user_id)) %>%
            ungroup() %>%
            group_by({{response_var_sym}}, {{explanatory_var_sym}}, total_users) %>%
            summarize(unique_users = n_distinct(user_id)) %>%
            ungroup() %>%
            mutate(pct_users = unique_users / total_users) %>%
            ggplot(aes(x = {{explanatory_var_sym}}, y = pct_users, fill = {{explanatory_var_sym}})) +
            geom_bar(stat = "identity") +
            facet_wrap({{response_var_sym}}) +
            scale_y_continuous(labels = percent) +
            theme_tq() +
            geom_text(aes(label = scales::percent(pct_users)),
                      position = position_stack(vjust = 0.5), color = "white", size = 2, check_overlap = TRUE) 
        
        
        return(viz)
        
    } else if (is_numeric(response_var_type) && is_categorical(explanatory_var_type)) {
        # Create box plot for numerical response_var
        box_viz <- data %>%
            ggplot(aes(y = {{response_var_sym}}, x = {{explanatory_var_sym}})) +
            geom_boxplot(aes(fill = {{explanatory_var_sym}}), alpha = 0.5) +
            theme_tq() 
        
        
        hist_viz <- data %>%
            ggplot(aes( x = {{response_var_sym}})) +
            geom_density(aes(fill = {{explanatory_var_sym}}), alpha = 0.5) +
            theme_tq() 
    
        
        # Create quantile table
        quant_tbl <- data %>%
            group_by({{explanatory_var_sym}}) %>% 
            summarize(
                q25 = quantile({{response_var_sym}}, probs = 0.25, na.rm = TRUE),
                q50 = quantile({{response_var_sym}}, probs = 0.5, na.rm = TRUE),
                q75 = quantile({{response_var_sym}}, probs = 0.75, na.rm = TRUE)
            )
        
        # Return the list of visualizations and table
        return(list(hist_viz, box_viz, quant_tbl))
        
    
    } else if (is_numeric(response_var_type) && is_numeric(explanatory_var_type)) {
        # Create Scatter plot for numerical response_var
        scatter_viz <- data %>%
            ggplot(aes(y = {{response_var_sym}}, x = {{explanatory_var_sym}})) +
            geom_point() +
            theme_tq() 
        return(scatter_viz)
        
        
    }
    
    # Add default return with warning if no conditions are met
    warning(glue("Unsupported variable types combination: 
                 response_var ({response_var_type}), 
                 explanatory_var ({explanatory_var_type})"))
    return(NULL)
}
