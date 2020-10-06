#' Make analysis data
#' 2020/06/12
#' Dr.Ideguchi


dir.subroutine <- "./src/sub"

list.files.sub <- list.files(path = dir.subroutine)

for(i in 1:length(list.files.sub)){
  source(
    sprintf("%s/%s",dir.subroutine,list.files.sub[i])
  )
}

# Read data ---------------------------------------------------------------

df.col_info <-
  readxl::read_excel(
    path = sprintf(
      "%s/%s", 
      dir.receiveddata, 
      fn.receiveddata
      ),
    sheet = 2
    )

data <-
  readxl::read_excel(
    path = sprintf(
      "%s/%s", 
      dir.receiveddata, 
      fn.receiveddata
      ),
    skip=1,
    col_names = df.col_info$col_names,
    col_types = df.col_info$col_types
    ) %>%
  data.frame()


# imputation --------------------------------------------------------------

data$meas.val.ab_Asp[data$meas.val.ab_Asp=='<0.5'] <- 0

imp.data <- apply(
  data[,-3],
  1,
  function(vec){
    vec.num <- 
      as.numeric(vec)
    return(vec.num)
    }
  ) %>%
  t() %>%
  data.frame()

imp.data <- 
  cbind(
    imp.data[,c(1,2)],
    data[,3],
    imp.data[,3:ncol(imp.data)]
    )

colnames(imp.data) <- df.col_info$col_names

save(
  imp.data, df.col_info,
  file = sprintf(
    '%s/%s', dir.data, fn.ADS
    )
  )



