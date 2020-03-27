#
# Calculates the proportion of explained Varianse of all Variables that are includet in a GAM
#
# Sven Adler
#
# based on a personal correspondence / discussion with Gavin Simpson
# see also Lit. Wood et al 2006 / 2011


drop_cont<-function(form=form,data_use=dat_mod,fam="quasibinomial",method="REML")
{
  var_all<-as.character(attr(terms(form),"variables"))[-1]
  response<-var_all[1]
  predictors<-var_all[-1]
  form0<-as.formula(paste(response,1,sep="~"))
  fit0<-gam(form0,data=data_use,family=fam)#,method=method)
  fit_tot<-gam(form,data=data_use,family=fam)#,method=method)
  d0<-deviance(fit0)
  d_tot<-deviance(fit_tot)
  var_explain<-predictor_explain<-prop_var_explain<-NA
  for (i in 1:length(predictors))
  {
    predictor_sub<-paste(predictors[-i],collapse="+")
    form1<-as.formula(paste(response,predictor_sub,sep="~"))
    fit_red<-gam(form1,data=data_use,family=fam,sp=fit_tot$sp[-i])#,method=method)
    predictor_explain[i]<-predictors[i]
    var_explain[i]<-(deviance(fit_red)-d_tot)/d0
    
    
  }
  prop_var_explain<-round(var_explain/sum(var_explain),3)
  #prop_var_explain<-prop_var_explain[order(prop_var_explain[,3],decreasing = F),]
  res<-data.frame(predictor_explain,var_explain,prop_var_explain)
  #res_a<-res[order(res$prop_var_explain,decreasing=T),]
  res<-list(fit_tot,summary(fit_tot),res)
}
