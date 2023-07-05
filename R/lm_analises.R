

lm_analises<-function(f.model=Ozone~0+Solar.R+Temp,data_=airquality,
                      output.path=NULL,show.progress=TRUE,
                      plot.labels="",weigth_negative=NULL){
  pkgs<-c('dplyr','ggplot2','lmtest','car','xlsx2dfs','colf')
  pkgs<-pkgs[pkgs%in%installed.packages()==FALSE]
  
  if(length(pkgs)>0&show.progress){
    cat('Installing packages',paste0(pkgs,collapse = " and "))
    install.packages(pkgs,quiet = TRUE)
  }
  library(dplyr)
  library(ggplot2)
  library(stats)
  library(data.table)
  
  drop_two_spaces<-function(char){
    gsub(pattern = "  ",replacement = " ",char)
  }
  organize_path<-function(path){
    gsub(pattern = " ",replacement = "_",path)
  }
  
  model<-lm(formula = f.model,data = data_)
  if(!is.null(weigth_negative)){
    coefs<-model$coefficients[attr(terms(f.model),"term.labels")]
    min_negative<-attr(terms(f.model),"term.labels")[which.min(coefs)]
    model<-lm(formula = f.model,data = data_,
              weights = ifelse(
                data_[,min_negative]>quantile(data_[,min_negative],0.75),
                weigth_negative,1
              ))
  }
  
  cat("Model:",as.character(terms(f.model))[c(2,1,3)])
  print(summary(model))
  
  if(sum(model$model[,1]<0)==0&sum(model$fitted.values<0)>0){
    warning('This model is returning negative values of explained variable. Maybe is convenient change the f.model input or define a weight_negative different of NULL')
  }
  
  paste0(
    ifelse(shapiro.test(model$residuals)$p>0.01,"Passou no teste shapiro de normalidade","Não passou no teste shapiro de normalidade"),'\n',
    ifelse(lmtest::bgtest(model)$p.value>0.05,"Passou no teste de homocedasticidade de Breusch-Godfrey","Não passou no teste de homocedasticidade de Breusch-Godfrey"),'\n',
    ifelse(car::durbinWatsonTest(model)$p>0.05,"Passou no teste de independência de Durbin Watson","Não passou no teste de independência de Durbin Watson"),'\n'
  ) %>% cat()
  
  if(is.null(output.path)){
    output.path<-getwd()
    cat('\n\n\n','Saving outputs in ',getwd(),'\n\n\n')
  }else{
    if(dir.exists(output.path)){
      if(str_sub(output.path,start = -1)=="/"){
        output.path<-str_sub(output.path,end=str_length(output.path)-1)
      }
      cat('\n\n\n','Saving outputs in ',output.path,'\n\n\n')
    }else{
      message('The path ',output.path," don't exists")
      return(NULL)
    }
  }
  
  cat('\r','Saving output 1 of 4')
  write.csv(capture.output(print(summary(model))),file=paste0(output.path,"/summary.txt"),row.names = F)
  
  residuals<-data.frame(id=1:length(model$residuals),residuos=model$residuals)
  
  options(ragg.max_dim = 55000)
  cat('\r','Saving output 2 of 4')
  ggplot(residuals)+geom_point(aes(x=id,y=residuos))+ylim(min(residuals$residuos)*1.1,1.1*max(residuals$residuos))+
    geom_line(data=data.frame(x=c(0,nrow(residuals)),y=c(0,0)),aes(x=x,y=y),colour="red")+
    labs(title = paste0("Resíduos do modelo ",plot.labels) %>% drop_two_spaces(),
         subtitle = paste0("Média dos resíduos:",round(mean(model$residuals),3)))+
    xlab("")+ylab("Resíduos")
  ggsave(filename = paste0(output.path,'/',organize_path(drop_two_spaces(paste0('residuos do modelo ',plot.labels,'.png')))),plot = last_plot(),
         dpi = 400,width = 222,height = 155,units = "mm",limitsize = FALSE)
  cat('\r','Saving output 3 of 4')
  ggplot(residuals)+geom_point(aes(x=id,y=residuos))+ylim(min(residuals$residuos)*1.1,1.1*max(residuals$residuos))+
    geom_line(data=residuals,aes(x=id,y=residuos)) +
    geom_line(data=data.frame(x=c(0,nrow(residuals)),y=c(0,0)),aes(x=x,y=y),colour="red")+
    labs(title = paste0('Resíduos do modelo ',plot.labels,' ordenados')%>% drop_two_spaces(),
         subtitle = paste0("Valor-P do teste de independência: ",round(car::durbinWatsonTest(model)$p,2)))+
    xlab("")+ylab("Resíduos")
  ggsave(filename = paste0(output.path,'/',organize_path(drop_two_spaces(paste0('residuos do modelo ',plot.labels,' ordenados.png')))),
         plot = last_plot(),dpi = 400,width = 222,height = 155,units = "mm",limitsize = FALSE)
  cat('\r','Saving output 4 of 4\n')
  r2<-cor(qqnorm(model$residuals)$x,model$residuals)
  main<-paste0("Teste de normalidade shapiro-wilk\n R²: ",round(r2,4))
  png(filename = paste0(output.path,'/',organize_path(drop_two_spaces(paste0('normalidade Shapiro Wilk ',plot.labels,".png")))),width = 25.317,height = 14.548,units = "cm",res=400)
  qqnorm(model$residuals,main=main,xlab = "Quantis teóricos",ylab = "Quantis da amostra");qqline(model$residuals,col="red")
  dev.off()
  
  
  
}










