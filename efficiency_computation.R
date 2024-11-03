library(stats)

chalfnormeff <- function(object, level) {
    beta <- object$mlParam[1:(object$nXvar)]
    delta <- object$mlParam[(object$nXvar + 1):(object$nXvar +
                                                    object$nuZUvar)]
    phi <- object$mlParam[(object$nXvar + object$nuZUvar + 1):(object$nXvar +
                                                                   object$nuZUvar + object$nvZVvar)]
    Xvar <- model.matrix(object$formula, data = object$dataTable,
                         rhs = 1)
    #--- Had to make this change so in a weird case where panel data dimensions are two columns it removes a column of 1s
    #cols_to_remove <- apply(Xvar, 2, function(col) all(col == 1))
    #Xvar = Xvar[, !cols_to_remove]
    #---
    uHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 2)
    vHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 3)
    Wu <- as.numeric(crossprod(matrix(delta), t(uHvar)))
    Wv <- as.numeric(crossprod(matrix(phi), t(vHvar)))
    epsilon <- model.response(model.frame(object$formula, data = object$dataTable)) -
        as.numeric(crossprod(matrix(beta), t(Xvar)))
    mustar <- -exp(Wu) * object$S * epsilon/(exp(Wu) + exp(Wv))
    sigmastar <- sqrt(exp(Wu) * exp(Wv)/(exp(Wu) + exp(Wv)))
    u <- mustar + sigmastar * dnorm(mustar/sigmastar)/pnorm(mustar/sigmastar)
    uLB <- mustar + qnorm(1 - (1 - (1 - level)/2) * (1 - pnorm(-mustar/sigmastar))) *
        sigmastar
    uUB <- mustar + qnorm(1 - (1 - level)/2 * (1 - pnorm(-mustar/sigmastar))) *
        sigmastar
    m <- ifelse(mustar > 0, mustar, 0)
    if (object$logDepVar == TRUE) {
        teJLMS <- exp(-u)  ## for cost it is Farrell equivalent
        teMO <- exp(-m)
        teBC <- exp(-mustar + 1/2 * sigmastar^2) * pnorm(mustar/sigmastar -
                                                             sigmastar)/pnorm(mustar/sigmastar)
        teBCLB <- exp(-uUB)
        teBCUB <- exp(-uLB)
        teBC_reciprocal <- exp(mustar + 1/2 * sigmastar^2) *
            pnorm(mustar/sigmastar + sigmastar)/pnorm(mustar/sigmastar)
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, teJLMS = teJLMS,
                          m = m, teMO = teMO, teBC = teBC, teBCLB = teBCLB,
                          teBCUB = teBCUB, teBC_reciprocal = teBC_reciprocal)
    } else {
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, m = m)
    }
    return(res)
}

cexponormeff <- function(object, level) {
    beta <- object$mlParam[1:(object$nXvar)]
    delta <- object$mlParam[(object$nXvar + 1):(object$nXvar +
                                                    object$nuZUvar)]
    phi <- object$mlParam[(object$nXvar + object$nuZUvar + 1):(object$nXvar +
                                                                   object$nuZUvar + object$nvZVvar)]
    Xvar <- model.matrix(object$formula, data = object$dataTable,
                         rhs = 1)
    uHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 2)
    vHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 3)
    Wu <- as.numeric(crossprod(matrix(delta), t(uHvar)))
    Wv <- as.numeric(crossprod(matrix(phi), t(vHvar)))
    epsilon <- model.response(model.frame(object$formula, data = object$dataTable)) -
        as.numeric(crossprod(matrix(beta), t(Xvar)))
    mustar <- -object$S * epsilon - exp(Wv)/sqrt(exp(Wu))
    u <- mustar + sqrt(exp(Wv)) * dnorm(mustar/sqrt(exp(Wv)))/pnorm(mustar/sqrt(exp(Wv)))
    uLB <- mustar + qnorm(1 - (1 - (1 - level)/2) * (1 - pnorm(-mustar/sqrt(exp(Wv))))) *
        sqrt(exp(Wv))
    uUB <- mustar + qnorm(1 - (1 - level)/2 * (1 - pnorm(-mustar/sqrt(exp(Wv))))) *
        sqrt(exp(Wv))
    m <- ifelse(mustar > 0, mustar, 0)
    if (object$logDepVar == TRUE) {
        teJLMS <- exp(-u)
        teMO <- exp(-m)
        teBC <- exp(-mustar + 1/2 * exp(Wv)) * pnorm(mustar/sqrt(exp(Wv)) -
                                                         sqrt(exp(Wv)))/pnorm(mustar/sqrt(exp(Wv)))
        teBCLB <- exp(-uUB)
        teBCUB <- exp(-uLB)
        teBC_reciprocal <- exp(mustar + 1/2 * exp(Wv)) * pnorm(mustar/sqrt(exp(Wv)) +
                                                                   sqrt(exp(Wv)))/pnorm(mustar/sqrt(exp(Wv)))
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, teJLMS = teJLMS,
                          m = m, teMO = teMO, teBC = teBC, teBCLB = teBCLB,
                          teBCUB = teBCUB, teBC_reciprocal = teBC_reciprocal)
    } else {
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, m = m)
    }
    return(res)
}

cgammanormeff <- function(object, level) {
    beta <- object$mlParam[1:(object$nXvar)]
    delta <- object$mlParam[(object$nXvar + 1):(object$nXvar +
                                                    object$nuZUvar)]
    phi <- object$mlParam[(object$nXvar + object$nuZUvar + 1):(object$nXvar +
                                                                   object$nuZUvar + object$nvZVvar)]
    P <- object$mlParam[object$nXvar + object$nuZUvar + object$nvZVvar +
                            1]
    Xvar <- model.matrix(object$formula, data = object$dataTable,
                         rhs = 1)
    uHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 2)
    vHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 3)
    Wu <- as.numeric(crossprod(matrix(delta), t(uHvar)))
    Wv <- as.numeric(crossprod(matrix(phi), t(vHvar)))
    epsilon <- model.response(model.frame(object$formula, data = object$dataTable)) -
        as.numeric(crossprod(matrix(beta), t(Xvar)))
    mui <- -object$S * epsilon - exp(Wv)/sqrt(exp(Wu))
    Hi1 <- numeric(object$Nobs)
    Hi2 <- numeric(object$Nobs)
    for (i in seq_len(object$Nobs)) {
        Hi1[i] <- mean((mui[i] + sqrt(exp(Wv[i])) * qnorm(object$FiMat[i,
        ] + (1 - object$FiMat[i, ]) * pnorm(-mui[i]/sqrt(exp(Wv[i])))))^(P))
        Hi2[i] <- mean((mui[i] + sqrt(exp(Wv[i])) * qnorm(object$FiMat[i,
        ] + (1 - object$FiMat[i, ]) * pnorm(-mui[i]/sqrt(exp(Wv[i])))))^(P -
                                                                             1))
    }
    u <- Hi1/Hi2
    if (object$logDepVar == TRUE) {
        teJLMS <- exp(-u)
        mui_Gi <- -object$S * epsilon - exp(Wv)/sqrt(exp(Wu)) -
            exp(Wv)
        mui_Ki <- -object$S * epsilon - exp(Wv)/sqrt(exp(Wu)) +
            exp(Wv)
        Gi <- numeric(object$Nobs)
        Ki <- numeric(object$Nobs)
        for (i in seq_len(object$Nobs)) {
            Gi[i] <- mean((mui_Gi[i] + sqrt(exp(Wv[i])) * qnorm(object$FiMat[i,
            ] + (1 - object$FiMat[i, ]) * pnorm(-mui_Gi[i]/sqrt(exp(Wv[i])))))^(P -
                                                                                    1))
            Ki[i] <- mean((mui_Ki[i] + sqrt(exp(Wv[i])) * qnorm(object$FiMat[i,
            ] + (1 - object$FiMat[i, ]) * pnorm(-mui_Ki[i]/sqrt(exp(Wv[i])))))^(P -
                                                                                    1))
        }
        teBC <- exp(exp(Wv)/exp(Wu/2) + object$S * epsilon +
                        exp(Wv)/2) * pnorm(-exp(Wv/2 - Wu/2) - object$S *
                                               epsilon/exp(Wv/2) - exp(Wv/2)) * Gi/(pnorm(-exp(Wv/2 -
                                                                                                   Wu/2) - object$S * epsilon/exp(Wv/2)) * Hi2)
        teBC_reciprocal <- exp(-exp(Wv)/exp(Wu/2) - object$S *
                                   epsilon + exp(Wv)/2) * pnorm(-exp(Wv/2 - Wu/2) -
                                                                    object$S * epsilon/exp(Wv/2) + exp(Wv/2)) * Ki/(pnorm(-exp(Wv/2 -
                                                                                                                                   Wu/2) - object$S * epsilon/exp(Wv/2)) * Hi2)
        res <- data.frame(u = u, teJLMS = teJLMS, teBC = teBC,
                          teBC_reciprocal = teBC_reciprocal)
    } else {
        res <- data.frame(u = u)
    }
    return(res)
}

cuninormeff <- function(object, level) {
    beta <- object$mlParam[1:(object$nXvar)]
    delta <- object$mlParam[(object$nXvar + 1):(object$nXvar +
                                                    object$nuZUvar)]
    phi <- object$mlParam[(object$nXvar + object$nuZUvar + 1):(object$nXvar +
                                                                   object$nuZUvar + object$nvZVvar)]
    Xvar <- model.matrix(object$formula, data = object$dataTable,
                         rhs = 1)
    uHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 2)
    vHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 3)
    Wu <- as.numeric(crossprod(matrix(delta), t(uHvar)))
    Wv <- as.numeric(crossprod(matrix(phi), t(vHvar)))
    epsilon <- model.response(model.frame(object$formula, data = object$dataTable)) -
        as.numeric(crossprod(matrix(beta), t(Xvar)))
    theta <- sqrt(12) * exp(Wu/2)
    u1 <- -exp(Wv/2) * ((dnorm((theta + object$S * epsilon)/exp(Wv/2)) -
                             dnorm(object$S * epsilon/exp(Wv/2)))/(pnorm((theta +
                                                                              object$S * epsilon)/exp(Wv/2)) - pnorm(object$S * epsilon/exp(Wv/2)))) -
        object$S * epsilon
    u2 <- exp(Wv/2) * (dnorm(object$S * epsilon/exp(Wv/2))/(1 -
                                                                pnorm(object$S * epsilon/exp(Wv/2))) - object$S * epsilon/exp(Wv/2))  # when theta/sigmav ---> Infty
    uLB <- exp(Wv/2) * qnorm((1 - level)/2 * pnorm((theta + object$S *
                                                        epsilon)/exp(Wv/2)) + (1 - (1 - level)/2) * pnorm(object$S *
                                                                                                              epsilon/exp(Wv/2))) - object$S * epsilon
    uUB <- exp(Wv/2) * qnorm((1 - (1 - level)/2) * pnorm((theta +
                                                              object$S * epsilon)/exp(Wv/2)) + (1 - level)/2 * pnorm(object$S *
                                                                                                                         epsilon/exp(Wv/2))) - object$S * epsilon
    m <- ifelse(-theta < object$S * epsilon & object$S * epsilon <
                    0, -object$S * epsilon, ifelse(object$S * epsilon >=
                                                       0, 0, theta))
    if (object$logDepVar == TRUE) {
        teJLMS1 <- exp(-u1)
        teJLMS2 <- exp(-u2)
        teMO <- exp(-m)
        teBC1 <- exp(object$S * epsilon + exp(Wv)/2) * (pnorm((object$S *
                                                                   epsilon + theta)/exp(Wv/2) + exp(Wv/2)) - pnorm(object$S *
                                                                                                                       epsilon/exp(Wv/2) + exp(Wv/2)))/(pnorm((theta + object$S *
                                                                                                                                                                   epsilon)/exp(Wv/2)) - pnorm(object$S * epsilon/exp(Wv/2)))
        teBC2 <- exp(object$S * epsilon + exp(Wv)/2) * (1 - pnorm(object$S *
                                                                      epsilon/exp(Wv/2) + exp(Wv/2)))/(1 - pnorm(object$S *
                                                                                                                     epsilon/exp(Wv/2)))
        teBCLB <- exp(-uUB)
        teBCUB <- exp(-uLB)
        teBC1_reciprocal <- exp(-object$S * epsilon + exp(Wv)/2) *
            (pnorm((object$S * epsilon + theta)/exp(Wv/2) - exp(Wv/2)) -
                 pnorm(object$S * epsilon/exp(Wv/2) - exp(Wv/2)))/(pnorm((theta +
                                                                              object$S * epsilon)/exp(Wv/2)) - pnorm(object$S *
                                                                                                                         epsilon/exp(Wv/2)))
        teBC2_reciprocal <- exp(-object$S * epsilon + exp(Wv)/2) *
            (1 - pnorm(object$S * epsilon/exp(Wv/2) - exp(Wv/2)))/(1 -
                                                                       pnorm(object$S * epsilon/exp(Wv/2)))
        res <- data.frame(u1 = u1, u2 = u2, uLB = uLB, uUB = uUB,
                          teJLMS1 = teJLMS1, teJLMS2 = teJLMS2, m = m, teMO = teMO,
                          teBC1 = teBC1, teBC2 = teBC2, teBCLB = teBCLB, teBCUB = teBCUB,
                          teBC1_reciprocal = teBC1_reciprocal, teBC2_reciprocal = teBC2_reciprocal,
                          theta = theta)
    } else {
        res <- data.frame(u1 = u1, u2 = u2, uLB = uLB, uUB = uUB,
                          m = m, theta = theta)
    }
    return(res)
}

ctruncnormscaleff <- function(object, level) {
    beta <- object$mlParam[1:(object$nXvar)]
    Xvar <- model.matrix(object$formula, data = object$dataTable,
                         rhs = 1)
    uHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 3)
    vHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 4)
    delta <- object$mlParam[(object$nXvar + 1):(object$nXvar +
                                                    (object$nuZUvar - 1))]
    tau <- object$mlParam[object$nXvar + (object$nuZUvar - 1) +
                              1]
    cu <- object$mlParam[object$nXvar + (object$nuZUvar - 1) +
                             2]
    phi <- object$mlParam[(object$nXvar + (object$nuZUvar - 1) +
                               2 + 1):(object$nXvar + (object$nuZUvar - 1) + 2 + object$nvZVvar)]
    musca <- exp(as.numeric(crossprod(matrix(delta), t(uHvar[,
                                                             -1, drop = FALSE])))) * tau
    Wusca <- cu + 2 * as.numeric(crossprod(matrix(delta), t(uHvar[,
                                                                  -1, drop = FALSE])))
    Wvsca <- as.numeric(crossprod(matrix(phi), t(vHvar)))
    epsilon <- model.response(model.frame(object$formula, data = object$dataTable)) -
        as.numeric(crossprod(matrix(beta), t(Xvar)))
    mustar <- (musca * exp(Wvsca) - exp(Wusca) * object$S * epsilon)/(exp(Wusca) +
                                                                          exp(Wvsca))
    sigmastar <- sqrt(exp(Wusca) * exp(Wvsca)/(exp(Wusca) + exp(Wvsca)))
    u <- mustar + sigmastar * dnorm(mustar/sigmastar)/pnorm(mustar/sigmastar)
    uLB <- mustar + qnorm(1 - (1 - (1 - level)/2) * (1 - pnorm(-mustar/sigmastar))) *
        sigmastar
    uUB <- mustar + qnorm(1 - (1 - level)/2 * (1 - pnorm(-mustar/sigmastar))) *
        sigmastar
    if (object$logDepVar == TRUE) {
        teJLMS <- exp(-u)
        m <- ifelse(mustar > 0, mustar, 0)
        teMO <- exp(-m)
        teBC <- exp(-mustar + 1/2 * sigmastar^2) * pnorm(mustar/sigmastar -
                                                             sigmastar)/pnorm(mustar/sigmastar)
        teBCLB <- exp(-uUB)
        teBCUB <- exp(-uLB)
        teBC_reciprocal <- exp(mustar + 1/2 * sigmastar^2) *
            pnorm(mustar/sigmastar + sigmastar)/pnorm(mustar/sigmastar)
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, teJLMS = teJLMS,
                          m = m, teMO = teMO, teBC = teBC, teBCLB = teBCLB,
                          teBCUB = teBCUB, teBC_reciprocal = teBC_reciprocal)
    } else {
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, m = m)
    }
    return(res)
}

ctruncnormeff <- function(object, level) {
    beta <- object$mlParam[1:(object$nXvar)]
    omega <- object$mlParam[(object$nXvar + 1):(object$nXvar +
                                                    object$nmuZUvar)]
    delta <- object$mlParam[(object$nXvar + object$nmuZUvar +
                                 1):(object$nXvar + object$nmuZUvar + object$nuZUvar)]
    phi <- object$mlParam[(object$nXvar + object$nmuZUvar + object$nuZUvar +
                               1):(object$nXvar + object$nmuZUvar + object$nuZUvar +
                                       object$nvZVvar)]
    Xvar <- model.matrix(object$formula, data = object$dataTable,
                         rhs = 1)
    muHvar <- model.matrix(object$formula, data = object$dataTable,
                           rhs = 2)
    uHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 3)
    vHvar <- model.matrix(object$formula, data = object$dataTable,
                          rhs = 4)
    mu <- as.numeric(crossprod(matrix(omega), t(muHvar)))
    Wu <- as.numeric(crossprod(matrix(delta), t(uHvar)))
    Wv <- as.numeric(crossprod(matrix(phi), t(vHvar)))
    epsilon <- model.response(model.frame(object$formula, data = object$dataTable)) -
        as.numeric(crossprod(matrix(beta), t(Xvar)))
    mustar <- (mu * exp(Wv) - exp(Wu) * object$S * epsilon)/(exp(Wu) +
                                                                 exp(Wv))
    sigmastar <- sqrt(exp(Wu) * exp(Wv)/(exp(Wu) + exp(Wv)))
    u <- mustar + sigmastar * dnorm(mustar/sigmastar)/pnorm(mustar/sigmastar)
    uLB <- mustar + qnorm(1 - (1 - (1 - level)/2) * (1 - pnorm(-mustar/sigmastar))) *
        sigmastar
    uUB <- mustar + qnorm(1 - (1 - level)/2 * (1 - pnorm(-mustar/sigmastar))) *
        sigmastar
    if (object$logDepVar == TRUE) {
        teJLMS <- exp(-u)
        m <- ifelse(mustar > 0, mustar, 0)
        teMO <- exp(-m)
        teBC <- exp(-mustar + 1/2 * sigmastar^2) * pnorm(mustar/sigmastar -
                                                             sigmastar)/pnorm(mustar/sigmastar)
        teBCLB <- exp(-uUB)
        teBCUB <- exp(-uLB)
        teBC_reciprocal <- exp(mustar + 1/2 * sigmastar^2) *
            pnorm(mustar/sigmastar + sigmastar)/pnorm(mustar/sigmastar)
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, teJLMS = teJLMS,
                          m = m, teMO = teMO, teBC = teBC, teBCLB = teBCLB,
                          teBCUB = teBCUB, teBC_reciprocal = teBC_reciprocal)
    } else {
        res <- data.frame(u = u, uLB = uLB, uUB = uUB, m = m)
    }
    return(res)
}

efficiencies.sfacross <- function(object, level = 0.95, newData = NULL,...) {
    if (level < 0 || level > 0.9999) {
        stop("'level' must be between 0 and 0.9999", call. = FALSE)
    }
    if (!is.null(newData)) {
        if (!is.data.frame(newData)) {
            stop("argument 'newData' must be of class data.frame")
        }
        object$dataTable <- newData
        object$Nobs <- dim(newData)[1]
    }
    if (object$udist == "hnormal") {
        EffRes <- chalfnormeff(object = object, level = level)
    } else {
        if (object$udist == "exponential") {
            EffRes <- cexponormeff(object = object, level = level)
        } else {
            if (object$udist == "gamma") {
                EffRes <- cgammanormeff(object = object, level = level)
            } else {
                if (object$udist == "uniform") {
                    EffRes <- cuninormeff(object = object, level = level)
                } else {
                    if (object$udist == "tnormal") {
                        if (object$scaling) {
                            EffRes <- ctruncnormscaleff(object = object,
                                                        level = level)
                        } else {
                            EffRes <- ctruncnormeff(object = object,
                                                    level = level)
                        }
                    }
                }
            }
        }
    }

    return(EffRes)
}
