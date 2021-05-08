# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
}

<<<<<<< HEAD
mean_column <- function(data, colum_name) {
  data %>% 
    drop_na() %>% 
    summarise(m = round(mean(colum_name), digits = 0)) %>% 
    pull(m)
=======
shiny_df <- function(x,y,df){
  df <- df %>% 
    select(y, x)
  df <- df %>% rename(density=x, category=y)
  return(df)
}

model_logit <- function(data) {
  # try to do something to insert also the target attribute by using function names() that gives column names of a dataframe
  data <- data %>% # original data size (494x16)
    pivot_longer(!status, names_to = "measurements", values_to = "values") %>% # create a pivot_longer structure (7410x3) -> (status, measurements, values)
    group_by(measurements) %>% # as we have all rows now in the same column in the pivot_longer, it is grouped by measurements -> original columns
    nest %>% # this function puts together each value for each measurement with the respective status (measurements, data), where data is a tibble with the value of the measurement and status for each row
    ungroup %>%
    mutate(model = map(data, ~ glm(status ~ values, # we apply a General Liner Model for each combination of attributes with respect to status
                                   data = .,
                                   family = binomial(link = "logit")))) %>% # binomial and logit are used as status is binary
    mutate(model_tidy = map(model, ~tidy(., conf.int = TRUE))) %>% # tidy gives the results of the fitting in a nice form in a tibble
    unnest(model_tidy) %>% # the tibble we just created is unnested, showing all results
    filter(str_detect(term, "values")) %>% # get rid of the intercepts and just keep the linear part
    mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                     TRUE ~ "Non-significant")) %>% # mark each fitting depending on its performance with p value
    mutate(neg_log10_p = -log10(p.value)) # create a column required for the Manhattan plot
  return(data)
>>>>>>> c33a83a30b59aa3338b24f7395a8bd5bb8d85b52
}

PCA <- function(data) {
  pca_columns <- data %>%
    select(status, everything())
  
  pca_data <- pca_columns %>% 
    select(!status) %>%
    prcomp(scale = TRUE)
    
  plot <- pca_data %>% augment(pca_columns) %>%
    mutate(status = factor(status)) %>%
    ggplot(aes(x = .fittedPC1,
               y = .fittedPC2,
               color = status)) + 
    geom_point(size = 2) + 
    theme_classic(base_family = "Avenir", base_size = 8) +
    theme(legend.position = "bottom")
  return(plot)
}