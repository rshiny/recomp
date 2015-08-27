#' Calculate relative efficiency
#'
#' @param data Input data frame
#' @param method Method or estimator
#' @param rho Correlation coefficient between visits
#' @return Relative efficiency (RE) of the specified estimator, relative to the endpoint contrast (M_K)
#' @export calRE
#' @examples
#'
#' dat <- formData(visits=1:4, times=visits, trtProfile=c(-3,rep(-4.5, length(visits)-1)),
#' plaProfile=c(-2,rep(-3, length(visits)-1)))
#' dat2 <- padzero(dat)
#' calcRE(dat2, method="AUC")



calcRE <- function(data, rho=0.5, method="M_KK") {

  attach(data)
  if(rho<0|rho>1)
    stop('Rho needs to be within [0,1]')

  if(length(visits)!=length(times))
    stop('Visits and Times differ in length, please check')

  if(!method%in%c("M_K", "M_KK", "Main", "AUC"))
    stop('Unrecognized method, should be one of the following: "M_K", "M_KK", "Main", or "AUC"')

  K <- length(visits)
  LY <- data.frame(Visit=c(0, visits), Time=c(0, times), Arm="LY", ChgLY=c(0, trtProfile))
  PL <- data.frame(Arm="PL", ChgPL=c(0, plaProfile))
  d <- cbind(LY, PL)

  LY <- as.numeric(d[-1,4])
  PLA <- as.numeric(d[-1,6])
  re.d <- LY-PLA
  gamma <- re.d/re.d[K]

  if (method=="M_K"){
    RE <- 1
  } else if (method=="M_KK") {
    RE <- (1+gamma[K-1])/sqrt(2*(1+rho))
  } else if (method=="Main") {
    RE <- sum(gamma)/sqrt(K+K*(K-1)*rho)
  } else if (method=="AUC") {
    time <- d$Time[-1]
    w <- numeric(K)
    w[1] <- time[2]/2
    for(i in 2:(K-1))
      w[i] <- time[i+1]-time[i-1]
    w[K] <- time[K] - time[K-1]

    RE4_num <- w[K]/2 + head(w,-1)%*%head(gamma,-1)
    tmp <- 0
    for(i in 1:(K-2)) {
      for(j in (i+1):(K-1)) {
        tmp <- tmp + w[i]*w[j]
      }
    }
    RE4_den <- sqrt(w[K]^2/4 + sum((head(w,-1))^2) + 2*rho*tmp +  rho*w[K]*sum(head(w,-1)))
    RE <- RE4_num/RE4_den
  }
  detach(data)
  return(RE)
}





