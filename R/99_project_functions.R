# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
}


mean_column <- function(data, colum_name) {
  data %>% 
    drop_na() %>% 
    summarise(m = round(mean(colum_name), digits = 0)) %>% 
    pull(m)
}

shiny_df <- function(x,y,df){
  df <- df %>% 
    select(y, x)
  df <- df %>% rename(density=x, category=y)
  return(df)
}
