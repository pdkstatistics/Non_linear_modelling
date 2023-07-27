## BASICS
options(scipen = 9)
extrafont::loadfonts(device = "win", quiet = TRUE)


## PACKAGES
library(minpack.lm)
library(nls.multstart)
library(ggplot2)
library(caret) # RMSE, R2
library(dplyr) # %>% 



## Data
#DT = read.table("clipboard", header = TRUE)
DT


## NONLINEAR MODELS
NLModels = list(
    
    ## 1. Exponential Model
    "Exp(k)" = list(Model = formula(y~exp(-k*x)),
                    Lower=c(k=0),
                    Upper=c(k=Inf),
                    Start_lower = c(k=0),
                    Start_upper = c(k=100)),
    
    ## 2. General Exponential
    "Exp(a,b)" = list(Model = formula(y~a*exp(-b*x)),
                      Lower=c(a=0, b=-1000),
                      Upper=c(a=Inf, b=Inf),
                      Start_lower = c(a=0, b=-100),
                      Start_upper = c(a=1000, b=1000)),
    
    ## 3. Weibull Model
    "Weibull(a,b)" = list(Model = formula(y~exp(-((x/b)^a))),
                          Lower=c(a=0, b=-1000),
                          Upper=c(a=Inf, b=Inf),
                          Start_lower = c(a=0, b=-100),
                          Start_upper = c(a=1000, b=1000)),
    
    ## 4. Page Model
    "Page(k,n)"  = list(Model = formula(y~exp(-k*(x^n))),
                        Lower=c(k=0, n=-1000),
                        Upper=c(k=Inf, n=Inf),
                        Start_lower = c(k=0, n=-100),
                        Start_upper = c(k=1000, n=1000)),
    
    ## 5. Two-terms Model
    "Two_terms(a,b,m,n)"  = list(Model = formula(y~(a*exp(-(m*x)))+
                                                     (b*exp(-(n*x)))),
                                 Lower=c(a=0, b=0, m=0, n=0),
                                 Upper=c(a=Inf, b=Inf, m=Inf, n=Inf),
                                 Start_lower = c(a=0, b=0, m=0, n=0),
                                 Start_upper = c(a=10, b=10, m=10, n=10)),
    
    
    ## 6. Mod_HP Model
    "Mod_HP(a,b,c,m,n,o)"  = list(Model = formula(y~(a*exp(-(m*x)))+
                                                      (b*exp(-(n*x)))+
                                                      (c*exp(-(o*x)))),
                                  Lower=c(a=0, b=0, c=0,
                                          m=0, n=0, o=0),
                                  Upper=c(a=Inf, b=Inf, c=Inf,
                                          m=Inf, n=Inf, o=Inf),
                                  Start_lower = c(a=0, b=0, c=0,
                                                  m=0, n=0, o=0),
                                  Start_upper = c(a=10, b=10, c=10,
                                                  m=10, n=10, o=10)),
    
    ## 7. Midilli Model
    "Midilli(a,b,k,c)"  = list(Model = formula(y~a*exp(-k*(x^b))+(c*x)),
                               Lower=c(a=0, b=0, k=0, c=0),
                               Upper=c(a=Inf, b=Inf, k=Inf, c=Inf),
                               Start_lower = c(a=0, b=0, k=0, c=0),
                               Start_upper = c(a=2, b=2, k=2, c=2))
    )




for (i in 3) {
    
    ## VARIABLE
    cat("\n##################################\n")
    names(DT)[i+1] %>% print()
    
    
    
    ## Data Selection
    data=na.omit(DT[c(1,i+1)])
    colnames(data)=c("x", "y")
    
    
    for (j in 1:length(names(NLModels))) {
        
        ## MODEL FITTING
        rm(Model)
        Model = nls_multstart(NLModels[[j]]$Model,
                              data = data,
                              lower=NLModels[[j]]$Lower,
                              upper=NLModels[[j]]$Upper,
                              start_lower = NLModels[[j]]$Start_lower,
                              start_upper = NLModels[[j]]$Start_upper,
                              iter = 1000,
                              supp_errors = "Y", # suppress error - Yes
                              #trace = TRUE   # trace the iteration
        )       
        
        cat("\n ======================================\n")
        names(NLModels)[j] %>% print()
        
        
        
        ## SUMMARY
        Model_sum = summary(Model)
        cat("\n--------\nParameter\n--------\n")
        Model_sum$coefficients %>% print()
        
        
        
        ## MODEL SELECTION CRITERION
        # RSS
        RSS = sum(Model_sum$residuals^2)
        
        # RMSE
        RMSE = RMSE(predict(Model),data$y)
        
        # R squared
        R2 = R2(predict(Model),data$y)
        
        # Chi square
        CHI = sum(Model_sum$residuals^2)/Model_sum$df[2]
        
        cat("\n--------\nModel Selection Criterion\n--------\n")
        data.frame(Model_name = names(NLModels)[j], 
                   RSS, RMSE, R2, CHI) %>% 
            print()
        
        
        
        
        
        ## PREDICTED VALUES
        cat("\n--------\nPredicted values\n--------\n")
        data.frame(data,
                   Predicted = predict(Model)) %>% print()
        
    }

}


## RESULT EXPORT
sink("D://WORKS//OTHERS WORKS//KITS Asha ENG//Results_DC_4mm.txt")
sink()




## Visualize data with prediction
ggplot(data = data,
       aes(x, y))+
    geom_point()+
    geom_line(aes(x, predict(Model)))+
    labs(title = names(NLModels)[1])+
    theme_bw()+
    theme(
        text=element_text(size=12,
                          colour = 'black',
                          family="Times New Roman"))
    
