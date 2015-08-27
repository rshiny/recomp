#' Plot longitudinal profiles
#'
#' @param data  Input data created by formData
#' @return Longitudinal profile plot
#' @export plotprofiles
#' @import ggplot2
#' @import tidyr
#' @examples
#' dat2 <- padzero(sampleData)
#' plotprofiles(dat2)

plotprofiles <- function(dat) {
  wide <- tidyr::spread(data=dat, key=trt, value=y)
  ggplot(data=dat, aes(x=times, y=y, color=trt))+geom_line() +geom_point()+
    geom_segment(data=wide, aes(x=times, xend=times, y=TRT, yend=PLA), size=1, color=gray(.7))
}
