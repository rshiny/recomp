#' Pad zero (at baseline) to dataset
#'
#' @param data Input data frame, without baseline visit
#' @return Data frame with zero value padded at top
#' @export padzero
#' @examples
#'
#' padzero(sampleData)

padzero<- function(data) {
  zero.trt <- data.frame(visits=0, times=0, y=0, trt="TRT")
  zero.pla <- data.frame(visits=0, times=0, y=0, trt="PLA")
  rbind(zero.trt, subset(data, trt=="TRT"), zero.pla, subset(data,trt=="PLA"))
}





