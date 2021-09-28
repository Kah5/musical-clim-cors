# this is adapted from DPLR
library(gganimate)
plotRings2<- function (year, trwN, trwS = NA, trwE = NA, trwW = NA, animation = FALSE, 
          sys.sleep = 0.2, year.labels = TRUE, d2pith = NA, col.rings = "grey", 
          col.outring = "black", x.rings = "none", col.x.rings = "red", 
          species.name = NA, saveGIF = TRUE, fpath ="growth_gifs",  fname = "GIF_plotRings", species, treeid) 
{
  # this assigns a dataframe with year as row.names, and with columsn as the different cores sampled from a single tree:
  TRW <- data.frame(row.names = year, trwN = trwN, trwS = if (exists("trwS") == 
                                                              TRUE) 
    trwS
    else NA, trwE = if (exists("trwE") == TRUE) 
      trwE
    else NA, trwW = if (exists("trwW") == TRUE) 
      trwW
    else NA)
  
  # get the rows that are non-NA for the tree
  TRW <- TRW[as.logical((rowSums(is.na(TRW)) - length(TRW))), ]
  TRW$trw.means <- rowMeans(TRW, na.rm = T) # get mean growth from each core
  
  # if their is a value for d2pith:
  if (!is.na(mean(d2pith, na.rm = T))) {
    TRW.d2pith <- TRW[, 1:4]
    if (!is.na(d2pith[1])) {
      TRW.d2pith$trwN[which.min(is.na(TRW.d2pith$trwN))] <- TRW.d2pith$trwN[which.min(is.na(TRW.d2pith$trwN))] + 
        d2pith[1]
    }
    if (!is.na(d2pith[2])) {
      TRW.d2pith$trwS[which.min(is.na(TRW.d2pith$trwS))] <- TRW.d2pith$trwS[which.min(is.na(TRW.d2pith$trwS))] + 
        d2pith[2]
    }
    if (!is.na(d2pith[3])) {
      TRW.d2pith$trwE[which.min(is.na(TRW.d2pith$trwE))] <- TRW.d2pith$trwE[which.min(is.na(TRW.d2pith$trwE))] + 
        d2pith[3]
    }
    if (!is.na(d2pith[4])) {
      TRW.d2pith$trwW[which.min(is.na(TRW.d2pith$trwW))] <- TRW.d2pith$trwW[which.min(is.na(TRW.d2pith$trwW))] + 
        d2pith[4]
    }
    TRW$trw.means[1] <- rowMeans(TRW.d2pith[1, ], na.rm = T)
  }
  
  
  TRW$trw.acc <- cumsum(TRW$trw.means) # cumulative sum of the tree ring width means
  y <- TRW$trwN - TRW$trwS # difference between the N and southe cores
  y[is.na(y)] <- 0 # just assign this difference to 0 if the values are NA
  
  if (exists("y") == TRUE) 
    TRW$N_S <- y
  
  x <- TRW$trwE - TRW$trwW # difference bewteen E and W cores
  x[is.na(x)] <- 0
  if (exists("x") == TRUE) 
    TRW$E_W <- x
  
  z <- TRW$trw.acc # create the cumulative sum of all RWI means
  
  # calculate quantiles for narrow and wide rings
  q2 <- as.numeric(quantile(TRW[, 5])[2])
  col.narrow.rings <- ifelse(TRW[, 5] <= q2, col.x.rings, col.rings)
  q4 <- as.numeric(quantile(TRW[, 5])[4])
  col.wider.rings <- ifelse(TRW[, 5] >= q4, col.x.rings, col.rings)
  TRW$bai.acc <- pi * (TRW$trw.acc)^2
  TRW$bai.ind <- c(TRW$bai.acc[1], TRW$bai.acc[2:nrow(TRW)] - 
                     TRW$bai.acc[1:nrow(TRW) - 1])
  
  
  circleFun <- function(center = c(0,0), diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  circle.data <- list()
  for(i in 1:length(TRW$bai.acc)){
  circle.data [[i]] <- circleFun(center = c(0,0), diameter = TRW$bai.acc[i], npoints = 100)
  circle.data [[i]]$year <- year[i]
  }
  
  circle.data.df <- do.call(rbind, circle.data)
  #geom_path will do open circles, geom_polygon will do filled circles
  # ggplot(circle.data.df,aes(x,y, col = year)) + geom_path()+ 
  #   transition_time(year)+ ease_aes('linear')
  # 
  
  p <- ggplot(circle.data.df, aes(x,y, group = year)) + geom_path(color = "saddlebrown")+coord_equal()+
    theme_bw(base_size = 16)+   theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())+
    #transition_layers(year, layer_length = 1, transition_length = 2, keep_layers = TRUE, from_blank = TRUE) 
    transition_reveal(along = year)+
    enter_appear()+labs(title = 'Year: {frame_along}', x = paste(species, treeid ,"\n width in (millimeters)"))
  
    ##nter_fade()  enter_grow()
  
  p
  
  animate(p, fps = 10, width = 450, height = 460, )
  anim_save(paste0(fpath,"/", fname,".gif"))
 
  
}
 
  
 