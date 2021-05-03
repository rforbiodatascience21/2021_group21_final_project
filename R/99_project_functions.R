# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
}

model_logit <- function(data) {
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