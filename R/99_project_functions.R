# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
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