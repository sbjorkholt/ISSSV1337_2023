parti <- c("R", "SV", "AP", "SP", "MDG", "KrF", "V", "H", "FrP")



# df<-df%>%mutate(MyNoisyCat= case_when( V1 < quantile(V1, 0.25) ~ sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.70, 0.1, 0.1, 0.1)),
#                                        V1<quantile(V1,0.5)~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.1, 0.7, 0.1, 0.1)),
#                                        V1<quantile(V1,0.75)~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.1, 0.1, 0.7, 0.1)),
#                                        TRUE~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.1, 0.1, 0.1, 0.7))))
# 

cor_sample <- function(data, pred, groups){
  pred = enquo(pred)
  data <- data %>%   
  mutate(x = case_when(!!pred < quantile(!!pred, 0.25) ~ sample(groups,n(), replace = TRUE, p = runif(n = length(groups))),
             !!pred < quantile(!!pred,0.5) ~ sample(groups,n(), replace = TRUE, p = runif(n = length(groups))),
             !!pred < quantile(!!pred,0.75) ~ sample(groups,n(), replace = TRUE, p = runif(n = length(groups))),
             TRUE ~ sample(groups,n(), replace = TRUE, p = runif(n = length(groups)))))

    return(data$x)
}

cor_sample(valgdata, age, parti)



venstre <- numeric(5000)
for (i in 1:5000) {
  venstre[i] <- valgdata %>% 
    select(vote) %>% 
    slice_head(n = i) %>% 
    count(vote) %>% 
    mutate(pr = n/sum(n)*100) %>% 
    filter(vote == "V") %>% 
    select(pr) %>% 
    as.numeric(.)
}


venstre
valgdata %>% 
  select(vote) %>% 
  slice_head(n = 30) %>% 
  count(vote) %>% 
  mutate(pr = n/sum(n)*100) %>% 
  filter(vote == "V") %>% 
  select(pr) %>% 
  as.numeric(.)


ggplot(.data, aes(!!x, fill = !!x)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual("legend", values = colours) +
  scale_y_continuous(labels = scales::percent_format()) +
  ggthemes::theme_excel_new()