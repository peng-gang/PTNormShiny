library(preprocessCore)
library(edgeR)
library(ggplot2)
library(ggpubr)


norm.functions <- list(
  # sample loading normalization
  sl.norm = function(x){
    target <- mean(colSums(x))
    norm.facs <- target/colSums(x)
    sweep(x, 2, norm.facs, FUN = "*")
  },
  
  # median normalization
  median.norm = function(x){
    median.all <- apply(x, 2, median)
    md <- median(median.all)
    norm.facs <- md/median.all
    sweep(x, 2, norm.facs, FUN = "*")
  },
  
  quantile.norm = function(x){
    cname <- colnames(x)
    rname <- rownames(x)
    rlt <- normalize.quantiles(as.matrix(x))
    rlt <- data.frame(rlt, stringsAsFactors = FALSE)
    colnames(rlt) <- cname
    rownames(rlt) <- rname
    rlt
  }#,
  
  # tmm.norm = function(x){
  #   raw_tmm <- calcNormFactors(x)
  #   sweep(x, 2, raw_tmm, FUN = "/")
  # }
)

data.clean <- function(x){
  idx.na <- rowSums(is.na(x))
  dt.no.na <- x[!idx.na, ]
  idx.zero <- rowSums(dt.no.na == 0)>0
  dt.clean <- dt.no.na[!idx.zero,]
  rlt <- dt.clean[,2:ncol(dt.clean)]
  rownames(rlt) <- dt.clean[,1]
  rlt
}



box.compare.single <- function(raw.data.clean, norm.data, idx.norm.single){
  gps <- list()
  dplot.raw <- melt(raw.data.clean, id.vars = NULL)
  gp.raw <- ggplot(dplot.raw) + geom_boxplot(aes(x=variable, y=log2(value))) + 
    theme_light() + 
    labs(y="Abundance (Log2)", x="") + 
    ggtitle("Raw") +
    theme(plot.title = element_text(hjust = 0.5))
  
  gps[[1]] <- gp.raw
  
  for(i in 1:length(norm.data)){
    dplot <- melt(norm.data[[i]], id.vars = NULL)
    gp<- ggplot(dplot) + geom_boxplot(aes(x=variable, y=log2(value))) + 
      theme_light() + 
      labs(y="Abundance (Log2)", x="") + 
      ggtitle(names(norm.method)[as.integer(idx.norm.single[i])]) +
      theme(plot.title = element_text(hjust = 0.5))
    gps[[i+1]] <- gp
  }
  
  gp <- ggarrange(
    plotlist = gps,
    ncol = 1,
    nrow = length(gps)
  )
  
  gp
}

cv <- function(df){
  ave <- rowMeans(df)
  sd <- apply(df, 1, sd)
  cv <- 100* sd /ave
  cv
}

cv.compare.single <- function(raw.data.clean, norm.data, idx.norm.single, sample.info){
  tb <- table(sample.info$sample.id)
  
  if(length(tb)<length(sample.info$sample.id)){
    sp.dup <- names(tb)[tb>1]
    
    cv.raw <- NULL
    sp <- NULL
    for(i in 1:length(sp.dup)){
      idx <- sample.info$sample.id==sp.dup[i]
      cv.raw <- c(cv.raw, cv(raw.data.clean[,idx]))
      sp <- c(sp, rep(sp.dup[i], nrow(raw.data.clean)))
    }
    
    cv.norm <- NULL
    for(i in 1:length(norm.data)){
      cv.norm.tmp <- NULL
      for(j in 1:length(sp.dup)){
        idx <- sample.info$sample.id == sp.dup[j]
        cv.norm.tmp <- c(cv.norm.tmp, cv(norm.data[[i]][,idx]))
      }
      cv.norm <- c(cv.norm, cv.norm.tmp)
    }
    
    dplot <- data.frame(
      cv = c(cv.raw, cv.norm),
      sample = rep(sp, 1+length(norm.data)),
      Method = factor(rep(c("Raw", names(norm.method)[as.integer(idx.norm.single)]), each = length(cv.raw)),
                      levels = c("Raw", names(norm.method)[as.integer(idx.norm.single)])),
      stringsAsFactors = FALSE
    )
    
    gp <- ggplot(dplot) + 
      geom_boxplot(aes(x=sample, y=cv, color=Method)) + 
      theme_light() +
      labs(x="", y = "Coefficient of Variation (%)")
    
    return(gp)
    
  } else {
    cv.raw <- cv(raw.data.clean)
    
    cv.norm <- NULL
    for(i in 1:length(norm.data)){
      cv.norm <- c(cv.norm, cv(norm.data[[i]]))
    }
    
    dplot <- data.frame(
      cv = c(cv.raw, cv.norm),
      Method = factor(rep(c("Raw", names(norm.method)[as.integer(idx.norm.single)]), each = length(cv.raw)),
                      levels = c("Raw", names(norm.method)[as.integer(idx.norm.single)]))
    )
    
    gp <- ggplot(dplot) + 
      geom_boxplot(aes(x=Method, y=cv)) + 
      theme_light() +
      labs(x="", y = "Coefficient of Variation (%)")
    
    return(gp)
  }
}


cluster.compare.single <- function(raw.data.clean, norm.data, idx.norm.single){
  op <- par(mfrow = c(ceiling((1+length(norm.data))/2),2))
  plotMDS(log2(raw.data.clean), main = "Raw")
  for(i in 1:length(norm.data)){
    plotMDS(log2(norm.data[[i]]), main = names(norm.method)[as.integer(idx.norm.single[i])])
  }
  par(op)
  recordPlot()
}
