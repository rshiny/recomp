#' Plot longitudinal profiles
#'
#' @param data  Input data created by formData
#' @return Longitudinal profile plot
#' @export plotprofiles
#' @examples
#' dat <- formData(visits=1:4, times=visits, trtProfile=c(-3,rep(-4.5, length(visits)-1)),
#' plaProfile=c(-2,rep(-3, length(visits)-1)))
#' dat2 <- padzero(dat)
#' plotprofiles(dat2)


plotprofiles <- function(data) {
  wide <- tidyr::spread(data=data, key=trt, value=y)
  ggplot2::ggplot(data=data, ggplot2::aes(x=times, y=y, group=trt, color=trt))+ggplot2::geom_line() + ggplot2::geom_point()+
    ggplot2::geom_segment(data=wide, ggplot2::aes(x=times, xend=times, y=TRT, yend=PLA), size=1, color=gray(.7))
}
