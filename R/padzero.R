#' Pad zero (at baseline) to dataset
#'
#' @param data Input data frame, without baseline visit
#' @return Data frame with zero value padded at top
#' @export padzero
#' @examples
#' dat <- formData(visits=1:4, times=visits, trtProfile=c(-3,rep(-4.5, length(visits)-1)),
#' plaProfile=c(-2,rep(-3, length(visits)-1)))
#' padzero(dat)

padzero<- function(data) {
  zero.trt <- data.frame(visits=0, times=0, y=0, trt="TRT")
  zero.pla <- data.frame(visits=0, times=0, y=0, trt="PLA")
  rbind(zero.trt, subset(data, trt=="TRT"), zero.pla, subset(data,trt=="PLA"))
}





