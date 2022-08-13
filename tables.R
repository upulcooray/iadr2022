set.seed(1851111)
a<- runif(15,0.1, 3.5) %>% round(2)　　 

a_1_25 <- d1(a)
a_1_2 <- d2(a)
a_2_25 <- d3(a)
a_2_2 <- d4(a)


rbind(observed=a, improve_x2= a_1_2 ) %>% knitr::kable()
rbind(observed=a, improve_25= a_2_25 ) %>% knitr::kable()
rbind(observed=a, improve_x2= a_2_2 ) %>% knitr::kable()



make_shift <- function(a, restrict, multi){
  
  out <- list()
  
  for (i in 1:length(a)) {
    if ((a[i] < restrict) &  (a[i]*multi <= restrict) ) {
      out[[i]] <- a[i]*multi} 
    else if ((a[i] < restrict) &  (a[i]*multi > restrict)) {
      out[[i]] <-  restrict
    }else{
      
      out[[i]] <-  a[i]
    }
  }
  unlist(out) %>% round(2)
  
} 




# increase income level by 25% (who are below poverty line)-----

d1 <- function(a) {
  
  make_shift(a,restrict = 1, multi = 1.25)
  
}

d2 <- function(a) {
  
  make_shift(a,restrict = 1, multi = 2)
  
}
d3 <- function(a) {
  
  make_shift(a,restrict = 1.7, multi = 1.25)
  
}

d4 <- function(a) {
  
  make_shift(a,restrict = 1.7, multi = 2)
  
}