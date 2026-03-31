
library(simmr)

mix<-as.matrix(na.omit(read.csv("d:\\Ryuyan\\SIMMR\\mix_POC.csv",sep=",")))
s_names<-c("C3","C4","soil","plankton")
s_means<-as.matrix(na.omit(read.csv("d:\\Ryuyan\\SIMMR\\s_means_POC.csv",sep=",")))
s_sds<-as.matrix(na.omit(read.csv("d:\\Ryuyan\\SIMMR\\s_sds_POC.csv",sep=",")))


simmr_in = simmr_load(mixtures = mix, source_names = s_names, source_means = s_means, source_sds = s_sds)
plot(simmr_in,trace = c(1,2))


simmr_out = simmr_mcmc(simmr_in)

post_pred = posterior_predictive(simmr_out)
                                        
print(post_pred)
prior_viz(simmr_out)

proportion_means = c(0.3,0.3,0.2,0.1,0.1)
proportion_sds = c(0.05,0.03,0.02,0.02,0.05)
prior = simmr_elicit(5, proportion_means, proportion_sds)

simmr_out_informative = simmr_mcmc(simmr_in, prior_control =
                                     list(means = prior$mean, sd = prior$sd, sigma_shape = c(3,5), 
                                          sigma_rate = c(3/50,3/50)))

summary(simmr_out,type='diagnostics')
summary(simmr_out,type='quantiles')
summary(simmr_out,type='statistics')

plot(simmr_out,type='matrix')
plot(simmr_out,type='density')
plot(simmr_out,type='box')


myfun<-function(n){
  C3mean<-c();C3sd<-c();C35<-c();C325<-c();C350<-c();C375<-c();C395<-c();
  C4mean<-c();C4sd<-c();C45<-c();C425<-c();C450<-c();C475<-c();C495<-c();
  Soilmean<-c();Soilsd<-c();Soil5<-c();Soil25<-c();Soil50<-c();Soil75<-c();Soil95<-c();
  Planktonmean<-c();Planktonsd<-c();Plankton5<-c();Plankton25<-c();Plankton50<-c();Plankton75<-c();Plankton95<-c();
  sewagemean<-c();sewagesd<-c();sewage5<-c();sewage25<-c();sewage50<-c();sewage75<-c();sewage95<-c();
  mix<-as.matrix(na.omit(read.csv("d:\\Ryuyan\\SIMMR\\mix_POC.csv",sep=",")));
  s_names<-c("C3","C4","Soil","Plankton");
  s_means<-as.matrix(na.omit(read.csv("d:\\Ryuyan\\SIMMR\\s_means_POC.csv",sep=",")));  
  s_sds<-as.matrix(na.omit(read.csv("d:\\Ryuyan\\SIMMR\\s_sds_POC.csv",sep=",")));
  for (i in  c(1:n)){
    simmr_in = simmr_load(mixtures=matrix(mix[i,],nrow = 1),source_names=s_names,source_means=s_means,source_sds=s_sds);
    simmr_out = simmr_mcmc(simmr_in);
    C3<-simmr_out[["output"]][["1"]][["BUGSoutput"]][["sims.matrix"]][,2];
    C4<-simmr_out[["output"]][["1"]][["BUGSoutput"]][["sims.matrix"]][,3];
    Soil<-simmr_out[["output"]][["1"]][["BUGSoutput"]][["sims.matrix"]][,4];
    Plankton<-simmr_out[["output"]][["1"]][["BUGSoutput"]][["sims.matrix"]][,5];
    C3mean<-c(C3mean,mean(C3));C3sd<-c(C3sd,sd(C3));C35<-c(C35,quantile(C3,c(0.05)));C325<-c(C325,quantile(C3,c(0.25)));
    C350<-c(C350,quantile(C3,c(0.50)));C375<-c(C375,quantile(C3,c(0.75)));C395<-c(C395,quantile(C3,c(0.95)));
    C4mean<-c(C4mean,mean(C4));C4sd<-c(C4sd,sd(C4));C45<-c(C45,quantile(C4,c(0.05)));C425<-c(C425,quantile(C4,c(0.25)));
    C450<-c(C450,quantile(C4,c(0.50)));C475<-c(C475,quantile(C4,c(0.75)));C495<-c(C495,quantile(C4,c(0.95)));
    Soilmean<-c(Soilmean,mean(Soil));Soilsd<-c(Soilsd,sd(Soil));Soil5<-c(Soil5,quantile(Soil,c(0.05)));Soil25<-c(Soil25,quantile(Soil,c(0.25)));
    Soil50<-c(Soil50,quantile(Soil,c(0.50)));Soil75<-c(Soil75,quantile(Soil,c(0.75)));Soil95<-c(Soil95,quantile(Soil,c(0.95)));
    Planktonmean<-c(Planktonmean,mean(Plankton));Planktonsd<-c(Planktonsd,sd(Plankton));Plankton5<-c(Plankton5,quantile(Plankton,c(0.05)));
    Plankton25<-c(Plankton25,quantile(Plankton,c(0.25)));Plankton50<-c(Plankton50,quantile(Plankton,c(0.50)));
    Plankton75<-c(Plankton75,quantile(Plankton,c(0.75)));Plankton95<-c(Plankton95,quantile(Plankton,c(0.95)));
      };
  m<-data.frame(C3mean=C3mean,C3sd=C3sd,C35=C35,C325=C325,C350=C350,C375=C375,C395=C395,
                C4mean=C4mean,C4sd=C4sd,C45=C45,C425=C425,C450=C450,C475=C475,C495=C495,
                Soilmean=Soilmean,Soilsd=Soilsd,Soil5=Soil5,Soil25=Soil25,Soil50=Soil50,Soil75=Soil75,Soil95=Soil95,
                Planktonmean=Planktonmean,Planktonsd=Planktonsd,Plankton5=Plankton5,Plankton25=Plankton25,Plankton50=Plankton50,
                Plankton75=Plankton75,Plankton95=Plankton95
                );
  write.csv(m,"contribution.csv")
}

myfun(333)


