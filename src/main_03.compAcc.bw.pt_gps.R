#' Compare accuracy of the test bw subjects' subgroups. 
#' Note: 2020/10/13

dir.subroutine <- "./src/sub"

list.files.sub <- list.files(path = dir.subroutine)

for(i in 1:length(list.files.sub)){
  source(
    sprintf("%s/%s",dir.subroutine,list.files.sub[i])
    )
  }

# load data ---------------------------------------------------------------

load(
  file = sprintf(
    '%s/%s', dir.data,
    fn.res.main_02.diagAcc_rev.R.RData
    )
  )

ADS.03  <- bind_cols(
  imp.data.pred, data.02
  )[
    imp.data.pred[,df.col_info$var.subgroup_01==1]=="1",
    ]

# Association between prediction error. -----------------------------------------------------

err.table <-
  table(
      Error = as.numeric(ADS.03[,"predict.var.test_dicho"]) !=
        as.numeric(as.character(ADS.03[,df.col_info$var.subgroup_02==1])),
      `risk factor` = ADS.03[,df.col_info$var.subgroup_02==1]=="1"
      )

res.fisher.test <-
  fisher.test(
    err.table
    )
    
print(
  list(
    res.fisher.test,
    err.table
    )
  )


# End ---------------------------------------------------------------------
