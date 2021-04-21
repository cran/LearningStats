#' Frequency Table
#'
#' The function \code{freq.table} computes a frequency table with absolute and relative frequencies (for non-ordered variables); and with those as well as their cumulative counterparts (for ordered variables).
#'
#' @param x a vector containing the sample provided to compute the frequency table
#' @param cont logical; if TRUE, the sample comes from a continuous variable; if FALSE the sample is treated as coming from a discrete or categorical variable.
#' @param ord if needed, character vector containing the ordered categories' names of the ordinal variable.
#'
#' @details The procedure used to define the intervals for the frequency table in the continuous case is the same as used for the histogram (see ?Histogram).
#'
#' @return A list containing the following components:
#' \item{ni}{a numeric vector containing the absolute frequencies.}
#' \item{fi}{a numeric vector containing the relative frequencies.}
#' \item{Ni}{a numeric vector containing the absolute cumulative frequencies.}
#' \item{Fi}{a numeric vector containing the relative cumulative frequencies.}
#' \item{di}{if cont=TRUE, a vector containing the frequency density.}
#' \item{tab}{the frequency table.}
#' The values of the cumulative frequencies (Ni and Fi) are only computed and provided when the variable of interest is ordered.
#' If the user does not save those values, the function provides the list on the console.
#'
#'
#' @examples
#' #Nominal variable
#' x=sample(c("yellow","red","blue","green"),size=20,replace=TRUE)
#' freq.table(x,cont=FALSE)
#'
#' #Ordinal variable
#' x=sample(c("high","small","medium"),size=20,replace=TRUE)
#' freq.table(x,cont=FALSE,ord=c("small","medium","high"))
#'
#' #Discrete variable
#' x=sample(1:5,size=20,replace=TRUE)
#' freq.table(x,cont=FALSE)
#'
#' #Continuous variable
#' x=rnorm(20)
#' freq.table(x,cont=TRUE)
#'
#' @export
freq.table<-function(x,cont,ord=NULL){
  if(!is.null(dim(x))){stop("'x' must be a vector")}
  if (length(cont)!=1){stop("'cont' must be a single logical value")}
  if (!is.logical(cont)){stop("'cont' must be a single logical value")}
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
  if(!length(x)>1){stop("'x' must be a sample of size bigger than one")}
  n=length(x)
  if(n==1){stop("'x' mus be at least of length two")}

  if(cont==TRUE){#continuous
    if(!is.null(ord)){warning("As the sample is continuous, 'ord' is not needed")}
    if(any(!is.finite(x))){stop("If 'x' comes from a continuous variable, it must be numeric")}
    ran=max(x)-min(x)
    n_int=round(sqrt(n),0)
    amp=ran/n_int; amp=ceiling(amp*100)/100
    breaks=min(x) + 0:n_int*amp;breaks=round(breaks,2);
    if(min(x)<breaks[1]){breaks[1]=breaks[1]-0.01}
    if(max(x)>=breaks[length(breaks)]){breaks[length(breaks)]=breaks[length(breaks)]+0.01}
    ni=table(cut(x,breaks, right=FALSE))
    fi=ni/n
    Ni=cumsum(ni)
    Fi=cumsum(fi)
    di=fi/amp
    tab=round(cbind(ni,fi,Ni,Fi,di),4)
  }else if(cont==FALSE){
    if(any(!is.finite(x))){#categorical
      if(is.null(ord)){#categorical nominal
        ni=table(x)
        fi=ni/n
        tab=round(cbind(ni,fi),4)
      }else{#categorical ordinal
        if(!is.character(ord)){stop("'ord' must be a character vector")}
        if(is.character(x)){x=as.factor(x)}
        if(sum(table(ord)==1)!=length(ord)){stop("'ord' has repeated categories")}
        if(length(levels(x))!=length(ord)){stop("The number of categories in 'ord' must be the same as the number of categories in 'x'")}
        if(sum(ord%in%levels(x))!=length(ord)){stop("The names of the categories in 'ord' and the ones in 'x' must agree")}
        x=factor(x,levels=ord)
        ni=table(x)
        fi=ni/n
        Ni=cumsum(ni)
        Fi=cumsum(fi)
        tab=round(cbind(ni,fi,Ni,Fi),4)
      }
    }else{#discrete
      if(!is.null(ord)){warning("As the sample is discrete, 'ord' is not needed")}
      ni=table(x)
      if(sum(ni==1)==sum(ni)){warning("There are no ties in the sample of a discrete variable")}
      fi=ni/n
      Ni=cumsum(ni)
      Fi=cumsum(fi)
      tab=round(cbind(ni,fi,Ni,Fi),4)

    }
  }

#  cat("\n          Frequency table\n \n")
 # print(tab)
  if(ncol(tab)==5){return(list(ni=ni, fi=fi, Ni=Ni, Fi=Fi, di=di, tab=tab))}
  if(ncol(tab)==4){return(list(ni=ni, fi=fi, Ni=Ni, Fi=Fi, tab=tab))}
  if(ncol(tab)==2){return(list(ni=ni, fi=fi, tab=tab))}
}
