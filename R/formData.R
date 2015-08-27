#' Form a data frame based on user specified information
#'
#' @param visits Vector of visit id variable
#' @param times Vector of time points corresponding to visits
#' @param trtProfile Vector of longitudinal change from baseline in Treatment arm (excluding baseline)
#' @param plaProfile Vector of longitudinal change from baseline in Placebo arm (excluding baseline)
#' @return A data frame with 4 columns: visits, times, y, and trt.
#' @export padzero
#' @examples
#' formData(visits=1:4, times=visits, trtProfile=c(-3,rep(-4.5, length(visits)-1)),
#' plaProfile=c(-2,rep(-3, length(visits)-1)))


formData <- function(visits=1:6, times=visits, trtProfile=c(-3,rep(-4.5, length(visits)-1)),
                     plaProfile=c(-2,rep(-3, length(visits)-1))){
  print(visits);
  trt <- data.frame(visits, times, y=trtProfile, trt="TRT")
  pla <- data.frame(visits, times, y=plaProfile, trt="PLA")
  data <- rbind(trt, pla)
  return(data)
}