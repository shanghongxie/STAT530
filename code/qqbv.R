qq2<-function(x,nx=0,maxy=0){
  #In case it wasn't one already
  # it needs to be a dataframe to
  # to use the sapply function right 
  x<-as.data.frame(x)
  #Determine layout of screen 
  nvars<-ncol(x)
  if (nx==0){
    nx<-ceiling(sqrt(nvars))}
  ny<-ceiling(nvars/nx) 
  if (maxy>0){
    if (ny>maxy){par(ask=TRUE)}
    ny<-maxy}
  
  par(mfrow=c(ny,nx))
  #Do the plotting 
  sapply(colnames(x),function(y){
    qqnorm(x[[y]],main=y)
    qqline(x[[y]])}) 
  
  par(mfrow=c(1,1)) 
  par(ask=FALSE)
}

bv2<-function(x,nx=0,maxy=0,jit=F){
  if (jit==T){x<-apply(x,2,jitter)}
  x<-as.data.frame(x)
  nvars<-ncol(x)
  ngraphs<-choose(nvars,2)
  if (nx==0){
    nx<-ceiling(sqrt(ngraphs))}
  ny<-ceiling(ngraphs/nx)
  if (maxy>0){
    if (ny>maxy){par(ask=TRUE)}
    ny<-maxy}
  par(mfrow=c(ny,nx))
  for (i in 1:(nvars-1)){
    for (j in (i+1):nvars){
      bvbox(cbind(x[,i],x[,j]),xlab=colnames(x)[i],ylab=colnames(x)[j])}}
  par(mfrow=c(1,1))
  par(ask=FALSE)
}