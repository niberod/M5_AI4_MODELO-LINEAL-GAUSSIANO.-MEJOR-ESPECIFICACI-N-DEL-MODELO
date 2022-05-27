data <-eval(modelo$call$data)




valor_Thail_actual <- k_Thail(modelo,k)       
modelo_sin_var <- list()
valor_Thail_nuevo <-c()


for(i in colnames(modelo$model[,-1])){
      #El nombre de cada modelo es el nombre de la variable que saco

      modelo_sin_var[[i]] <- lm(y~.,
                                data=data %>% 
                                      dplyr::select(-i) 
      )
      valor_Thail_nuevo[i] <- k_Thail(modelo_sin_var[[i]],k) 
      
      
}


variable_a_sacar <-  names(which.min(valor_Thail_nuevo))
variable_a_sacar
#Me quedo con este modelo y calculo nuevo Thail
modelo <- modelo_sin_var[[variable_a_sacar ]]
data <- data %>% 
      dplyr::select(-variable_a_sacar)

valor_Thail_nuevo <- k_Thail(modelo,k)
valor_Thail_nuevo
valor_Thail_actual
valor_Thail_nuevo<valor_Thail_actual





```{r step}
data <-eval(modelo$call$data)
#Calculo el k_thail con el modelo actual
valor_Thail_actual <- k_Thail(modelo,k)    
#Para empezar le resto 1 para asegurarme que sea menor y haga la primera iteración
valor_Thail_nuevo <- valor_Thail_actual-1

#Mientras el k Thail sea menor al del modelo anterior, y si hay por lo menos
#1 variable, entonces sigue haciendo el loop hasta encontrar un óptimo local
while (valor_Thail_nuevo<valor_Thail_actual & ncol(modelo$model)>=3) {
      valor_Thail_actual <- k_Thail(modelo,k)       
      modelo_sin_var <- list()
      valor_Thail_nuevo <-c()
      
      for(i in colnames(modelo$model[,-1])){
            #El nombre de cada modelo es el nombre de la variable que saco
            modelo_sin_var[[i]] <- lm(y~.,
                                      data=data %>% 
                                            dplyr::select(-i) 
            )
            valor_Thail_nuevo[i] <- k_Thail(modelo_sin_var[[i]],k) 
            
            
            
      }
      
      #Variable que saco
      variable_a_sacar <-  names(which.min(valor_Thail_nuevo))
      
      #Me quedo con este modelo y calculo nuevo Thail
      modelo <- modelo_sin_var[[variable_a_sacar ]]
      
      #Saco la variable del dataframe
      data <- data %>% 
            dplyr::select(-variable_a_sacar)
      
      
      
}






