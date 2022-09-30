KoladaAPI=function(){
  #rm(list=ls())
  library(httr)
  library(readxl)
  
  #library(jsonlite) #just for test comments
  url1="https://raw.githubusercontent.com/muhis097/Lab5/main/data/List_kommun.xlsx"
  GET(url1, write_disk(tfile1 <- tempfile(fileext = ".xlsx")))
  List_kommun = read_excel(tfile1,col_types = "text")
  rich=c(28,121,235,252,26,151,129,251,199,32,261,125)
  richnames=List_kommun$Kommun[c(29,122,236,253,27,152,130,252,200,33,262,126)-1]
  richcode=List_kommun$Kod[c(29,122,236,253,27,152,130,252,200,33,262,126)-1]
  url=list()
  for (i in rich){
    url[[i]] =gsub("[\r\n]", "", paste("http://api.kolada.se/v2/data/kpi/N03101,N03006,N03105,N03120,N03104,N03132,N03100,N03048,N03001,N03003,N03103,N03016,N03016,N03106,N03144,N03079,N03102/municipality/",List_kommun$Kod[i],"/year/2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021",sep = ""))
  }
  url[sapply(url, is.null)] <- NULL
  resp_test=lapply(url,GET)
  data_cont=lapply(resp_test,content)
  
  ##Here we create a big list out of values in data_cont(and its sub_list)
  list_adder=function(n){
    return(data_cont[[n]]$values)
  }
  list_extract=function(n,m){
    dot=my_data[[n]][[m]][1:3]
    dot=append(dot,my_data[[n]][[m]][[4]][[1]][4])
    return(dot)
  }
  ## after making the big list we create sub list out of that for preparation 
  #of dataframe
  
  my_data=(lapply(1:length(data_cont),list_adder))  #list of raw data
  
  ##following function creates final list
  f_list=list()
  for (i in 1:length(my_data)){
    for (j in 1:160){
      f_list=append(f_list,list(list_extract(i,j)))
    }
  }
  return(f_list)
}