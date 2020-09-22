
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
numericcharacters <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(x)))) & is.character(x)
}

scale1 <- function(x) scale(x)[,1]

construct_coef_df = function(dvs,df){
  fhab_mds = lapply(dvs, function(x) {
    lm(substitute(i ~ fhab + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  fhab_sums = lapply(fhab_mds, summary)
  
  femo_mds = lapply(dvs, function(x) {
    lm(substitute(i ~ femo + study_t + scanner_t , list(i = as.name(x))), data = df)
  })
  femo_sums = lapply(femo_mds, summary)
  
  fconnected_mds = lapply(dvs, function(x) {
    lm(substitute(i ~ fconnected + study_t + scanner_t , list(i = as.name(x))), data = df)
  })
  fconnected_sums = lapply(fconnected_mds, summary)
  
  fhab_betas = c()
  femo_betas = c()
  fconnected_betas = c()
  fhab_se = c()
  femo_se = c()
  fconnected_se = c()
  for (i in c(1:length(dvs))){
    fhab_betas = c(fhab_betas,fhab_sums[[i]]$coefficients[2,1])
    femo_betas = c(femo_betas,femo_sums[[i]]$coefficients[2,1] )
    fconnected_betas = c(fconnected_betas,fconnected_sums[[i]]$coefficients[2,1] )
    fhab_se = c(fhab_se, fhab_sums[[i]]$coefficients[2,2])
    femo_se = c(femo_se, femo_sums[[i]]$coefficients[2,2])
    fconnected_se = c(fconnected_se, fconnected_sums[[i]]$coefficients[2,2])
  }
  
  coef_df = data.frame("IV" = c(rep("Habitual FB use", length(dvs)), rep("Emotional FB use", length(dvs)), 
                                rep("FB connectedness", length(dvs))),
                       "DV" = c(dvs,dvs, dvs),
                       "beta" = c(fhab_betas, femo_betas, fconnected_betas),
                       "se" = c(fhab_se, femo_se,fconnected_se))
  coef_df = coef_df %>%
    mutate(IV = factor(coef_df$IV, levels=c("Habitual FB use", "Emotional FB use", "FB connectedness")))
  return(coef_df)
}

plot_coef_df = function(coef_dv){
  g1 = ggplot(coef_df, aes(x=DV, y=beta, colour=IV)) + 
    geom_errorbar(aes(ymin= beta-1.96*se, ymax= beta+1.96*se), width=.1, position=pd) +
    geom_point(position=pd) + theme_pubr()  +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "grey", size=1) + 
    rotate_x_text(45)
  # g1 = ggplot(coef_df, aes(x=DV, y=beta, colour=IV)) + 
  #   geom_errorbar(aes(ymin= beta-1.96*se, ymax= beta+1.96*se), width=.1, position=pd) +
  #   geom_point(position=pd) + theme_pubr() + #+ 
  # scale_color_manual(values = c("Habitual FB use" = "dodgerblue", "Emotional FB use" = "forestgreen")) 
  #  geom_hline(yintercept=0, linetype="dashed", 
  #               color = "grey", size=1) + 
  # rotate_x_text(45)
  return(g1)
}

construct_coef_df_iri = function(dvs,df){
  mds1 = lapply(dvs, function(x) {
    lm(substitute(i ~ IRI_Perspective_Taking + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums1 = lapply(mds1, summary)
  
  mds2 = lapply(dvs, function(x) {
    lm(substitute(i ~ IRI_Fantasy + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums2 = lapply(mds2, summary)
  
  mds3 = lapply(dvs, function(x) {
    lm(substitute(i ~ IRI_Empathic_Concern + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums3 = lapply(mds3, summary)
  
  mds4 = lapply(dvs, function(x) {
    lm(substitute(i ~ IRI_Personal_Distress + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums4 = lapply(mds4, summary)
  
  betas1 = c()
  betas2 = c()
  betas3 = c()
  betas4 = c()
  se1 = c()
  se2 = c()
  se3 = c()
  se4 = c()
  for (i in c(1:length(dvs))){
    betas1 = c(betas1,sums1[[i]]$coefficients[2,1])
    betas2 = c(betas2,sums2[[i]]$coefficients[2,1])
    betas3 = c(betas3,sums3[[i]]$coefficients[2,1])
    betas4 = c(betas4,sums4[[i]]$coefficients[2,1])
    se1 = c(se1, sums1[[i]]$coefficients[2,2])
    se2 = c(se2, sums2[[i]]$coefficients[2,2])
    se3 = c(se3, sums3[[i]]$coefficients[2,2])
    se4 = c(se4, sums4[[i]]$coefficients[2,2])
  }
  
  coef_df = data.frame("IV" = c(rep("Perspective taking", length(dvs)),
                                rep("Fantasy", length(dvs)),
                                rep("Empathic concern", length(dvs)),
                                rep("Personal distress", length(dvs))),
                       "DV" = c(dvs,dvs,dvs,dvs),
                       "beta" = c(betas1, betas2,betas3,betas4),
                       "se" = c(se1, se2,se3,se4))
  coef_df = coef_df
  return(coef_df)
}



plot_coef_iri = function(coef_dv){
  g1 = ggplot(coef_df, aes(x=DV, y=beta, colour=IV)) + 
    geom_errorbar(aes(ymin= beta-1.96*se, ymax= beta+1.96*se), width=.1, position=pd) +
    geom_point(position=pd) + theme_pubr()  +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "grey", size=1) + 
    rotate_x_text(45)
  return(g1)
}

corstar <- function(x, y = NULL, use = "pairwise", method = "pearson", round = 3, row.labels, col.labels, ...) {
  
  require(psych)
  
  ct <- corr.test(x, y, use, method)    # calculate correlation
  r <- ct$r                             # get correlation coefs
  p <- ct$p                             # get p-values
  
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))) # generate significance stars
  
  m <- matrix(NA, nrow = nrow(r) * 2, ncol = ncol(r) + 1) # create empty matrix
  
  rlab <- if(missing(row.labels)) rownames(r) else row.labels # add row labels
  clab <- if(missing(col.labels)) {
    if(is.null(colnames(r)))
      deparse(substitute(y))
    else
      colnames(r)
  } else {
    col.labels # add column labels
  }
  
  rows <- 1:nrow(m)                     # row indices
  cols <- 2:ncol(m)                     # column indices
  
  odd <- rows %% 2 == 1                 # odd rows
  even <- rows %% 2 == 0                # even rows
  m[odd, 1] <- rlab                     # add variable names
  m[even, 1] <- rep("", sum(even))      # add blank
  
  m[odd, cols] <- paste(format(round(r, round), nsmall = round, ...), stars, sep = "")     # add r coefs
  m[even, cols] <- paste("(", format(round(p, round), nsmall = round, ...), ")", sep = "") # add p-values
  
  colnames(m) <- c(" ", clab)           # add colnames
  m                                     # return matrix
}




# Skewness and kurtosis and their standard errors as implement by SPSS
#
# Reference: pp 451-452 of
# http://support.spss.com/ProductsExt/SPSS/Documentation/Manuals/16.0/SPSS 16.0 Algorithms.pdf
# 
# See also: Suggestion for Using Powerful and Informative Tests of Normality,
# Ralph B. D'Agostino, Albert Belanger, Ralph B. D'Agostino, Jr.,
# The American Statistician, Vol. 44, No. 4 (Nov., 1990), pp. 316-321

spssSkewKurtosis=function(x) {
  w=length(x)
  m1=mean(x)
  m2=sum((x-m1)^2)
  m3=sum((x-m1)^3)
  m4=sum((x-m1)^4)
  s1=sd(x)
  skew=w*m3/(w-1)/(w-2)/s1^3
  sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
             dimnames=list(c("skew","kurtosis"), c("estimate","se")))
  return(mat)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


construct_coef_df_nts = function(dvs,df){
  mds1 = lapply(dvs, function(x) {
    lm(substitute(i ~ NTS_Belongingness + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums1 = lapply(mds1, summary)
  
  mds2 = lapply(dvs, function(x) {
    lm(substitute(i ~ NTS_SelfEsteem + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums2 = lapply(mds2, summary)
  
  mds3 = lapply(dvs, function(x) {
    lm(substitute(i ~ NTS_Control + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums3 = lapply(mds3, summary)
  
  mds4 = lapply(dvs, function(x) {
    lm(substitute(i ~ NTS_MeaningfulExistence + study_t + scanner_t, list(i = as.name(x))), data = df)
  })
  sums4 = lapply(mds4, summary)
  
  betas1 = c()
  betas2 = c()
  betas3 = c()
  betas4 = c()
  se1 = c()
  se2 = c()
  se3 = c()
  se4 = c()
  for (i in c(1:length(dvs))){
    betas1 = c(betas1,sums1[[i]]$coefficients[2,1])
    betas2 = c(betas2,sums2[[i]]$coefficients[2,1])
    betas3 = c(betas3,sums3[[i]]$coefficients[2,1])
    betas4 = c(betas4,sums4[[i]]$coefficients[2,1])
    se1 = c(se1, sums1[[i]]$coefficients[2,2])
    se2 = c(se2, sums2[[i]]$coefficients[2,2])
    se3 = c(se3, sums3[[i]]$coefficients[2,2])
    se4 = c(se4, sums4[[i]]$coefficients[2,2])
  }
  
  coef_df = data.frame("IV" = c(rep("Belongingness", length(dvs)),
                                rep("SelfEsteem", length(dvs)),
                                rep("Control", length(dvs)),
                                rep("MeaningfulExistence", length(dvs))),
                       "DV" = c(dvs,dvs,dvs,dvs),
                       "beta" = c(betas1, betas2,betas3,betas4),
                       "se" = c(se1, se2,se3,se4))
  coef_df = coef_df
  return(coef_df)
}


getWC_mat = function(text, w){
  stopwords = c("just","usually","sometimes","something",
                "also","every")
  
  docs <- Corpus(VectorSource(text))
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, stopwords) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  ## Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v*w)
  return(d)
  
}



