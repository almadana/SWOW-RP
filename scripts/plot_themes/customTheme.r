theme_Publication <- function(base_size=14, base_family="Helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            plot.tag = element_text(face="bold", size=rel(1.5)),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_color_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#00A965","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}


scale_color_currBio <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#1982C4","#09324b","#ED3658","#91142c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

scale_color_currBio2 <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#ED3658","#1982C4","#09324b","#188949","#62B259","#C15726","#E87A30","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}


scale_fill_currBio <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#1982C4","#09324b","#ED3658","#91142c","#188949","#62B259","#C15726","#E87A30","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

scale_fill_currBio2 <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#ED3658","#1982C4","#09324b","#188949","#62B259","#C15726","#E87A30","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}


scale_shape_Publication <- function(...) {
  library(scales)
  discrete_scale("shape","Publication",manual_pal(values = c(19,24,22,23,25)),...)
}


