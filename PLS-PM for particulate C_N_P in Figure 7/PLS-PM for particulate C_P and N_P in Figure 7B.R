library(dplyr)
library(psych)
rm(list=ls())
library(plspm)
library(ggplot2)
library(GGally)
library(lavaan)
data <- read.csv(file.choose(), header = TRUE)

latent_r<- list( 
  dam = c('HRT', 'RA', 'ND'), 
  human = c('CL', 'IS', 'HAI'),  
  WH = c('WT','EC','pH'), 
  C_P = c('POC_PP', 'PON_PP') )
latent_r

dam<-c(0,0,0,0)
human <-c(0,0,0,0)
WH<-c(1,1,0,0)
C_P <-c(1,1,1,0)

latent_path_r <- rbind(dam, human, WH,C_P)
colnames(latent_path_r) <- rownames(latent_path_r)
latent_path_r
innerplot(latent_path_r)

latent_path_r <- rbind(dam, human, WH,C_P)
colnames(latent_path_r) <- rownames(latent_path_r)
latent_path_r
innerplot(latent_path_r)

r_A <- rep('A', 4)

Sample_pls <- plspm(data, latent_path_r, latent_r, modes = r_A, boot.val = T,br = 1000
                    )
summary(Sample_pls )
Sample_pls$crossloadings

Sample_pls$gof
Sample_pls$boot

Sample_pls$path_coefs 

Sample_pls$effects


Sample_pls$inner_model 

innerplot(Sample_pls, colpos = 'blue', colneg = 'red', show.values =FALSE, lcol = 'black', box.lwd = 1,curve = 0)

Sample_pls$outer_model
inner_model <- Sample_pls$inner_model
path_coefficients <- bind_rows(lapply(names(inner_model), function(name) {  
  data.frame(   
    Predictor = rownames(inner_model[[name]]),    
    Response = name,    
    Estimate = inner_model[[name]][, "Estimate"],   
    StdError = inner_model[[name]][, "Std. Error"],   
    tValue = inner_model[[name]][, "t value"],    
    pValue = inner_model[[name]][, "Pr(>|t|)"]  )})) %>%  
  filter(Predictor != "Intercept")  

r_squared <- as.data.frame(Sample_pls$inner_summary) %>% 
  select(Type, R2) %>%  
  filter(Type == "Endogenous") %>%  
  mutate(Response = rownames(.)) %>% 
  select(Response, R2)

path_coefficients <- path_coefficients %>%  
  left_join(r_squared, by = "Response") %>% 
  arrange(Response, Predictor) %>% 
  mutate(Significance = ifelse(pValue < 0.001, "***",                              
                               ifelse(pValue < 0.01, "**",                                     
                                      ifelse(pValue < 0.05, "*", "ns"))))

print(path_coefficients)


Sample_pls$gof

Sample_pls$effects