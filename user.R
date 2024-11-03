## Esqueleto de aplicación con creación, edición, carga y descarga de un conjunto de datos (data.frame)
## Autor: Manuel Muñoz Márquez (manuel.munoz@uca.es)
## Licencia: GNU-GPL >= 3
## Proyecto: Proyecto R-UCA (http://knuth.uca.es/R)
##
## This file includes the definition of the user functions
## The user can defined here it's own funtion
## All required packages must be load here

### Definition of new data set, as data.frame, suitable for current model
new.data <- function() {
    data.frame()
}

### Main panel
main.title <- function(language = 'en') {
    switch(language,
           es = 'Análisis de frontera estocástica',
           'Stochastic frontier analysis'
           )
}

### Menu panel
other_url <- function(language = 'en') {
    switch (language,
            es = '../skeleton/',
            '../esqueleto/'
            )
}
other_language <- function(language = 'en') {
    switch (language,
            es = 'English version',
            'Versión española'
            )
}

### Information tab definitions
## Title of the information tab
information.title <- function(language = 'en') {
    switch(language,
           es = 'Información',
           'Information')
}
## Text of the information tab
information.text <- function(language = 'en') {
    switch(language,
           es = HTML(
               '<p align="justify">Aplicación que permite la creación, edición, carga y descarga de un fichero csv con datos. \
  Se proveen datos de ejemplo. Una vez cargue datos, puede escoger un modelo de frontera estocástica en la pestaña "Acciones AFS". </p> \
  Ha sido diseñada para calcular los coeficientes de la función frontera de los datos introducidos en la pestaña "Resultados". \
  El modelo transversal estimado es el introducido en <a href="https://doi.org/10.1016/0304-4076(77)90052-5">Aigner, Lovell, & Schmidt (1977)</a>. \
  Además, es capaz de estimar la (in)eficiencia de las observaciones en la pestaña "Eficiencias". Cada fila en la tabla de esta pestaña corresponde a la observación de dicha fila en la tabla de datos. \
  "u" y "m" son la estimación de la ineficiencia de la esperanza condicionada y la moda desarrolladas por <a href="http://dx.doi.org/10.1016/0304-4076(82)90004-5">Jondrow et al. (1982)</a>. \
  "teJLMS" es exp(-"u"), representando la eficiencia técnica según Jondrow et al. (1982). "teBC" es la eficiencia técnica propuesta por <a href="https://doi.org/10.1016/0304-4076(88)90053-X">Battese  Coelli (1988)</a>.</p>\
  En el caso de datos de panel se pueden emplear las distribuciones de ineficiencia seminormal y normal truncada. Es necesario introducir cuáles columnas definen la dimensión temporal y cuál la dimensión transversal. La ineficiencia media de una entidad como <a href="https://doi.org/10.1016/0304-4076(88)90053-X">Battese  Coelli (1988)</a> puede ser hallada en la pestaña de eficiencias. En el caso en el que se utilize un modelo con ineficiencia variante en el tiempo, se estima el modelo de <a href="http://dx.doi.org/10.1007/BF00158774">Battese  Coelli (1992)</a>.
  <p align="justify">Esta aplicación web usa el paquete de R "sfaR" desarrollado por Dakpo KH., Desjeux Y., Henningsen A., y Latruffe L. (2023) y sobre el esqueleto de aplicación web desarrollado por <a href="mailto:manuel.munoz(at)uca.es">M. Muñoz-Márquez</a>. Escrita por <a href="mailto:fidel.solisaguilar(at)uca.es">Fidel Solís Aguilar</a>.</p>'           ),
           HTML(
               '<p align="justify">This application allows create, edit, load and download a csv data file. \
  It has been designed to compute the coefficients of the frontier function of the inputted data on the "Results" tab. \
  The estimated transversal model is the one introduced by <a href="https://doi.org/10.1016/0304-4076(77)90052-5">Aigner, Lovell, & Schmidt (1977)</a>. \
  It is also able to estimate the (in)efficiency of the observations on the "Efficiencies" tab. Each row on the table corresponds to the observation in said row on the data table.
  "u" and "m" are the conditional mean and mode inefficiency estimates developed by <a href="http://dx.doi.org/10.1016/0304-4076(82)90004-5">Jondrow et al. (1982)</a>. \
  "teJLMS" is exp(-"u"), representing technical efficiency according to Jondrow et al. (1982). "teBC" is the technical efficiency proposed by <a href="https://doi.org/10.1016/0304-4076(88)90053-X">Battese and Coelli (1988)</a>.</p>\
  In the case of panel data half-normal and truncated normal distributions for the inefficiency term are supported. It will be needed to input which columns of define the temporal dimension and cross-sectional dimension. Average inefficiency of an entity will be shown on the efficiencies tab. It is possible to estimate the time-varying inefficiency model introduced by <a href="http://dx.doi.org/10.1007/BF00158774">Battese  Coelli (1992)</a>.
               <p align="justify">This web app uses the "sfaR" package developed by Dakpo KH., Desjeux Y., Henningsen A., y Latruffe L. (2023) and the web app skeleton developed by <a href="mailto:manuel.munoz(at)uca.es">M. Muñoz-Márquez</a>. Written by <a href="mailto:fidel.solisaguilar(at)uca.es">Fidel Solís Aguilar</a>.</p>'
           )
           )
}
# Dakpo KH., Desjeux Y., Henningsen A., and Latruffe L. (2023). sfaR: Stochastic Frontier Analysis Routines. R package version 1.0.0.


### Examples data set
examples.files <- function(language = 'en') {
    switch(language,
           es = c('elsalvador.csv', 'charnes1981.csv', 'pib.csv', 'rice_production_philippines.csv'),
           c('elsalvador.csv', 'charnes1981.csv', 'pib.csv', 'rice_production_philippines.csv')
           )
}

### Data panel
## Title of data tab
data.title <- function(language = 'en') {
    switch(language,
           es = 'Datos',
           'Data'
           )
}

### Results panel
## Title of result tab
results.title <- function(language = 'en') {
    switch(language,
           es = 'Resultados',
           'Results'
           )
}
## Function that computes the results
results <- function(data, input, values) {
    #if(nrow(data) > 0) summary(data)
    if(nrow(data) == 0) {
        return()
    }
    if(is.null(input$insumos)){
        return()
    }

    if(input$ineff_term == paste(sfa['hnormal'])){
        udist = "hnormal"
    }else if(input$ineff_term == paste(sfa['exponential'])){
        udist = "exponential"
    }else if(input$ineff_term == paste(sfa['tnormal'])){
        udist = "tnormal"
    }else if(input$ineff_term == paste(sfa['gamma'])){
        udist = "gamma"
    }else udist = "hnormal"
    
    out_col = input$out_column

    col_names <- names(data)
    
    # Use a panel dataframe if its chosen
    if(input$data_type == sfa["cross"]) {
        useddata = data
        }
    else {
        useddata = pdata.frame(data, index = c(input$var1_panel, input$var2_panel))
        }

    response_var <- out_col
    predictor_vars = input$insumos
    
    if(input$isLog){
        fo = reformulate(termlabels = paste(predictor_vars, collapse = "+"), response = response_var)
    }else{
        predictor_formula <- paste("log(", predictor_vars, ")", collapse = " + ")
        formula_str <- paste("log(", response_var, ") ~", predictor_formula)
        fo <- as.formula(formula_str)
    }
    
    # Add Determinants of Inefficiency
    #if(!(is.null(input$determinants))){
    #    print("test")
    #    fo_str = as.character(fo)
    #    fo_str[3] = paste(fo_str[3], '|', paste( input$determinants, collapse=' + ' ))
    #    fo = as.formula(paste(fo_str[2], fo_str[1], fo_str[3] ))
    #}

    #sfaResult <- sfa( fo, data = data)
    #coef( sfaResult )
    #sfaResult <<- sfacross(formula = fo, udist = udist, data = useddata, logDepVar = input$isLog) # TODO 

    if(input$data_type == sfa["cross"]){
        if(input$ineff_term == paste(sfa['tnormal'])){
            values$sfaResultFrontier <- sfa(formula = fo, data= useddata, truncNorm = TRUE)
            values$sfaResultSFAR <- sfacross(formula = fo, udist = udist, data = useddata)
            
        }else{
            values$sfaResultSFAR <- sfacross(formula = fo, udist = udist, data = useddata)
        }
        
    }
    else{
        values$sfaResultSFAR <- sfacross(formula = fo, udist = udist, data = useddata)
        values$sfaResultFrontier <- sfa(formula = fo, data= useddata, truncNorm = input$ineff_term == paste(sfa["tnormal"]), timeEffect = input$time_variant)
        }
    
    #efficiencies(sfaResult)
    #summary(values$sfaResultFrontier)
    
    return()
    }


effic <- function(data, input, values){
    if(nrow(data) == 0) {
        return()
    }
    if(is.null(input$insumos)){
        return()
    }
    if(input$data_type == sfa["cross"]){
        if(input$ineff_term == paste(sfa['gamma'])){
            return(efficiencies(values$sfaResultSFAR, level = input$confidence_level)[c("u", "teJLMS", "teBC")])
        }else{
            return(efficiencies(values$sfaResultSFAR, level = input$confidence_level)[c("u", "m", "teJLMS", "teBC", "teBCLB", "teBCUB")])
        }
        
    }else{
        return(summary(values$sfaResultFrontier)$effic)
    }
        
    
    
}

model_fitness <- function(data, input, values){
    if(nrow(data) == 0) {
        return()
    }
    if(is.null(input$insumos)){
        return()
    }
    if(input$data_type == paste(sfa["cross"])){
        object = values$sfaResultSFAR
        
        mlLoglik = object$mlLoglik
        nParm = object$nParm
        Nobs = object$Nobs
    }else{
        object = values$sfaResultFrontier
        
        mlLoglik = object$mleLogl
        nParm = length(object['mleParam'][[1]]) / 4
        Nobs = object$nob
    }
    
    AIC <- -2 * mlLoglik + 2 * nParm
    BIC <- -2 * mlLoglik + log(Nobs) * nParm
    HQIC <- -2 * mlLoglik + 2 * log(log(Nobs)) * nParm
    
    
    df = data.frame(fitness = c("AIC", "BIC", "HQIC"), value = c(AIC, BIC, HQIC), row.names=1)
    return(t(df))
    
}

coeff <- function(data, input, values){
    if(nrow(data) == 0) {
        return()
    }
    if(is.null(input$insumos)){
        return()
    }
    if(input$data_type == paste(sfa["cross"]) & input$ineff_term != paste(sfa['tnormal']) ){
        mlRes = summary(values$sfaResultSFAR)['mlRes'][[1]]
        nParams = summary(values$sfaResultSFAR)["nXvar"][[1]]
        extra_param = ifelse(input$ineff_term == paste(sfa['gamma']), 1, 0)
        
        col_names = names(values$sfaResultSFAR$mlParam)[1:nParams]
        
        frontier_params = mlRes[1:nParams]
        std_error = mlRes[(nParams+3 + extra_param):(2*nParams+2 + extra_param)]
        z_value = mlRes[(2*nParams+5 + 2*extra_param):(3*nParams+4 + 2*extra_param)]
        prob_absolute_value_of_z = mlRes[(3*nParams+7 + 3*extra_param):(4*nParams+6 + 3*extra_param)]
        
        significance = array(dim = nParams)
        for(i in 1:nParams){
            val = prob_absolute_value_of_z[i]
            if(val < 0.001){
                significance[i] = "****"
            }else if(val < 0.01){
                significance[i] = "***"
            }else if(val < 0.05){
                significance[i] = "**"
            }else if(val < 0.1){
                significance[i] = "*"
            }else{
                significance[i] = " "
            }
        }
        df = data.frame(row.names = col_names, frontier_params, std_error, z_value, prob_absolute_value_of_z, significance)
        colnames(df) = c(paste(sfa['par1']), paste(sfa['par2']), "z value", "Pr[Z > |z value|]", "")
        return(df)
    }else{
        mlRes = summary(values$sfaResultFrontier)['mleParam'][[1]]
        aux_df = data.frame(mlRes)
        
        aux_df = data.frame(rownames(aux_df), mlRes)
        colnames(aux_df) = c(paste(sfa['par1']), paste(sfa['par2']), "z value", "Pr[Z > |z value|]", "")
        I = which(match(aux_df[1][[1]], "sigmaSq") == 1)
        df = aux_df[1:(I-1),]
        
        significance = array(dim = 3)
        for(i in 1:length(df[5][[1]])){
            val = df[5][[1]][i]
            print(val)
            if(val < 0.001){
                significance[i] = "****"
            }else if(val < 0.01){
                significance[i] = "***"
            }else if(val < 0.05){
                significance[i] = "**"
            }else if(val < 0.1){
                significance[i] = "*"
            }else{
                significance[i] = " "
            }
        }
        
        df[" "] = significance
        return(df)
    }
    
}

ineff_coeff <- function(data, input, values){
    if(nrow(data) == 0) {
        return()
    }
    if(is.null(input$insumos)){
        return()
    }
    if(input$data_type == paste(sfa["cross"]) & input$ineff_term != paste(sfa['tnormal']) ){
        sigma_u_sq = summary(values$sfaResultSFAR)['sigmauSq'][[1]]
        sigma_v_sq = summary(values$sfaResultSFAR)['sigmavSq'][[1]]
        sigma = sqrt(sigma_u_sq + sigma_v_sq)
        lambda = sqrt(sigma_u_sq/sigma_v_sq)
        gamma = sigma_u_sq/sigma**2
        
        Eu  = summary(values$sfaResultSFAR)['Eu'][[1]]
        Expu  = summary(values$sfaResultSFAR)['Expu'][[1]]
        
        rownames = c("sigma_u^2", "sigma_v^2", "sigma", "lambda", "gamma", paste(sfa['par3']), paste(sfa['par4']))
        numbers = c(sigma_u_sq, sigma_v_sq, sigma, lambda, gamma, Eu, Expu)
        
        
        if(input$ineff_term == paste(sfa['gamma'])){
            #Extract Shape parameter from gamma distribution
            dff = data.frame(summary(values$sfaResultSFAR)$mlRes)
            names(summary(values$sfaResultSFAR))
            
            P = dff[dim(dff)[1],][[1]]
            rownames= c("P = m+1",rownames)
            numbers=c(P, numbers)
        }
        df = data.frame(rownames,numbers)
        colnames(df) = c(paste(sfa['par5']), paste(sfa['par6']))
        
        return(df)
    }else{
        object = summary(values$sfaResultFrontier)['mleParam'][[1]]
        df = data.frame(object)
        len = dim(df)[1]
        
        if(input$ineff_term == paste(sfa["tnormal"])){
            starting_point=len-2
        }else{
            starting_point=len-1
        }
        if(input$time_variant){
            starting_point = starting_point - 1
        }
            
        sigma_sq = df[starting_point, , drop=TRUE][[1]]
        gamma = df[starting_point+1, , drop=TRUE][[1]]
        sigma_u_sq = gamma*sigma_sq
        sigma_v_sq = sigma_sq - sigma_u_sq
        sigma=sqrt(sigma_sq)
        lambda = sqrt(sigma_u_sq/sigma_v_sq)
        
        if(input$ineff_term == paste(sfa["tnormal"])){
            mu = df[starting_point+2, , drop=TRUE][[1]]
            Eu = mu + sqrt(sigma_u_sq) * dnorm(mu/sqrt(sigma_u_sq))/pnorm(mu/sqrt(sigma_u_sq))
            Expu = exp(-mu + 1/2 * sigma_u_sq) * pnorm(mu/sqrt(sigma_u_sq) - sqrt(sigma_u_sq))/pnorm(mu/sqrt(sigma_u_sq))
            
            rownames = c("mu", "sigma_u^2", "sigma_v^2", "sigma", "lambda", "gamma", paste(sfa['par3']), paste(sfa['par4']))
            numbers = c(mu, sigma_u_sq, sigma_v_sq, sigma, lambda, gamma, Eu, Expu)
            
            
            
        }else{
            Eu = sqrt(sigma_u_sq) * sqrt(2/pi)
            Expu = 2 * (1 - pnorm(sqrt(sigma_u_sq))) * exp(sigma_u_sq/2)
            
            rownames = c("sigma_u^2", "sigma_v^2", "sigma", "lambda", "gamma", paste(sfa['par3']), paste(sfa['par4']))
            numbers = c(sigma_u_sq, sigma_v_sq, sigma, lambda, gamma, Eu, Expu)

        }
        
        
        if(input$time_variant){
            if(input$ineff_term == paste(sfa["tnormal"])){
                time_parameter = df[starting_point+3, , drop=TRUE][[1]]
            }else{
                time_parameter = df[starting_point+2, , drop=TRUE][[1]]
            }
            
            
            rownames = c(rownames, paste(sfa['par7']))
            numbers = c(numbers, time_parameter)
        }
        dff = data.frame(rownames, numbers)
        colnames(dff) = c(paste(sfa['par5']), paste(sfa['par6']))
        return(dff)
    }
}


### Efficiencies panel
## Title of result tab
effic.title <- function(language = 'en') {
    switch(language,
           es = 'Eficiencias',
           'Efficiencies'
    )
}



### User Friendly Features

warnings <- function(data, input, values){
    
    # No data loaded:
    if(nrow(data) == 0) {
        return(paste(sfa['error1']))
    }
    
    # Frontier function without inputs:
    if(is.null(input$insumos)){
        return(paste(sfa['error2']))
    }
    
    # No errors!
    return()
}


significance <- function(data, input, values){
    if(nrow(data) == 0 | is.null(input$insumos)) return()
    
    return(paste(sfa["signific"]))
}



### Plot panel
## Title of plot tab
graphic.title <- function(language = 'en') {
    switch(language,
           es = 'Gráfico',
           'Graphic'
           )
}
## Function that plots the results
## This function must call results if it need it
graphic.plot <- function(data) {
    if (ncol(data) > 0) {
        columns <- sapply(data, function(x) {is.numeric(x) | is.factor(x)})
        if (sum(columns) > 1) {
            plot(data[, columns], main = '')
        } else plot.new()
    }
}

