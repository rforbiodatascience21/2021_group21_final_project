# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
}

model_logit <- function(data) {
  # try to do something to insert also the target attribute by using function names() that gives column names of a dataframe
  data <- data %>%
    pivot_longer(!status, names_to = "measurements", values_to = "values") %>%
    group_by(measurements) %>%
    nest %>%
    ungroup %>%
    mutate(model = map(data, ~ glm(status ~ values, 
                                   data = .,
                                   family = binomial(link = "logit")))) %>%
    mutate(model_tidy = map(model, ~tidy(., conf.int = TRUE))) %>%
    unnest(model_tidy) %>%
    filter(str_detect(term, "values")) %>%
    mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                     TRUE ~ "Non-significant")) %>%
    mutate(neg_log10_p = -log10(p.value))
  return(data)
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