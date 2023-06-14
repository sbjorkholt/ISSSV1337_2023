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

