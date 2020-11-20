#' Make analysis data
#' 2020/10/07

dir.subroutine <- "./src/sub"

list.files.sub <- list.files(
  path = dir.subroutine
  )

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
      dir.receiveddata.02, 
      fn.receiveddata
      ),
    sheet = "col_info"
    )

data <-
  readxl::read_excel(
    path = sprintf(
      "%s/%s", 
      dir.receiveddata.02, 
      fn.receiveddata
      ),
    skip=2,
    col_names = df.col_info$col_names,
    col_types = df.col_info$col_types
    ) %>%
  data.frame()

data.02 <-
  readxl::read_excel(
    path = sprintf(
      "%s/%s", 
      dir.receiveddata.02, 
      fn.receiveddata
      ),
    skip=2,
    col_names = df.col_info$col_names,
    col_types = df.col_info$col_types.02
    ) %>%
  data.frame()



# imputation --------------------------------------------------------------

imp.data <- data
#   apply(
#   data[,-3],
#   1,
#   function(vec){
#     vec.num <- 
#       as.numeric(vec)
#     return(vec.num)
#     }
#   ) %>%
#   t() %>%
#   data.frame()
# 
# imp.data <- 
#   cbind(
#     imp.data[,c(1,2)],
#     data[,3],
#     imp.data[,3:ncol(imp.data)]
#     )
# 
# colnames(imp.data) <-
#   df.col_info[
#     df.col_info$col_types!="skip",
#     "col_names"
#     ]

save(
  imp.data, data.02, df.col_info,
  file = sprintf(
    '%s/%s', dir.data, fn.ADS.02
    )
  )



