# Define project functions ------------------------------------------------
foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
}

shiny_df <- function(x,y,df){
  df <- df %>% 
    select(y, x)
  df <- df %>% 
    rename(density=x, category=y)
  return(df)
}
