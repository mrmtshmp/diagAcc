#' Improve accuracy of the test in true negative subjects.
#' 2020/06/16
#' m.2020/07/10
#' m.2020/10/07

dir.subroutine <- "./src/sub"

list.files.sub <- list.files(path = dir.subroutine)

for(i in 1:length(list.files.sub)){
  source(
    sprintf("%s/%s",dir.subroutine,list.files.sub[i])
    )
  }

# load data ---------------------------------------------------------------

load(
  file = sprintf('%s/%s', dir.data, fn.ADS.02)
  )

var.y <- df.col_info[
  df.col_info$var.y==1, 
  "col_names"
  ] %>%
  unlist() %>%
  unname()

var.test <-  df.col_info[
  df.col_info$var.test==1, 
  "col_names"
  ] %>%
  unlist() %>%
  unname()
  

imp.data[,var.y] <- 
  as.factor(
    imp.data[,var.y]
    )

# recursive partition -----------------------------------------------------

fml.string_var.test <-
  sprintf(
    '%s ~ %s',
    var.y, var.test
    )

fml.string_var.test_commed <-
  sprintf(
    '%s ~ %s',
    var.y,
    paste(
      var.test,
      paste(
        colnames(imp.data[grep('(^com\\.|^med\\.).+', colnames(imp.data))]),
        collapse = "+"
        ),
      sep = "+"
      )
    )
fml.string_full <- sprintf('%s ~ %s', var.y, '.')

res.rpart.var.test <- 
  rpart(
    imp.data, # c.m.p 2020/10/09 cancers were merged in the com.malignancies
    formula = as.formula(fml.string_var.test)
    )

res.rpart.var.test_commed <- 
  rpart(
    imp.data, # c.m.p 2020/10/09 cancers were merged in the com.malignancies
    formula = as.formula(fml.string_var.test_commed)
  )

res.rpart.full <- 
  rpart(
    imp.data,  # c.m.p 2020/10/09 cancers were merged in the com.malignancies
    formula = as.formula(fml.string_full)
    )

res.rpart.cancer_all.var.test <- 
  rpart(
    imp.data %>% # c.m.p 2020/10/09 cancers were merged in the com.malignancies
      dplyr::filter(com.malignancies==1),
    formula = as.formula(fml.string_var.test)
    )
res.rpart.cancer_all.var.test_commed <- 
  rpart(
    imp.data %>% # c.m.p 2020/10/09 cancers were merged in the com.malignancies
      dplyr::filter(com.malignancies==1),
    formula = as.formula(fml.string_var.test_commed)
  )
res.rpart.cancer_all.full <- 
  rpart(
    imp.data %>%
      dplyr::filter(com.malignancies==1),
    formula = as.formula(fml.string_full)
  )
res.rpart.auto_imn_all.var.test <- 
  rpart(
    imp.data %>% # c.m.p 2020/10/09 cancers were merged in the com.malignancies
      dplyr::filter(com.autoimm==1),
    formula = as.formula(fml.string_var.test)
    )
res.rpart.auto_imn_all.var.test_commed <- 
  rpart(
    imp.data %>% # c.m.p 2020/10/09 cancers were merged in the com.malignancies
      dplyr::filter(com.autoimm==1),
    formula = as.formula(fml.string_var.test_commed)
    )
res.rpart.auto_imn_all.full <- 
  rpart(
    imp.data %>%
      dplyr::filter(com.autoimm==1),
    formula = as.formula(fml.string_full)
  )

pdf(sprintf('%s/%s',dir.output.02,'rpart.total_subj.pdf'))
plot(partykit::as.party(res.rpart.var.test))
plot(partykit::as.party(res.rpart.var.test_commed))
plot(partykit::as.party(res.rpart.full))
dev.off()

pdf(sprintf('%s/%s',dir.output.02,'rpart.cancer_all.pdf'))
plot(partykit::as.party(res.rpart.cancer_all.var.test))
plot(partykit::as.party(res.rpart.cancer_all.var.test_commed))
plot(partykit::as.party(res.rpart.cancer_all.full))
dev.off()

pdf(sprintf('%s/%s',dir.output.02,'rpart.auto_imn_all.pdf'))
plot(partykit::as.party(res.rpart.auto_imn_all.var.test))
plot(partykit::as.party(res.rpart.auto_imn_all.var.test_commed))
plot(partykit::as.party(res.rpart.auto_imn_all.full))
dev.off()


# Assessment on predictions -----------------------------------------------

imp.data$predict.var.test <- 
  predict(res.rpart.var.test)[,"1"]
imp.data$predict.var.test_dicho <- 
  predict(res.rpart.var.test)[,"1"] > 0.5

imp.data$predict.full <- 
  predict(res.rpart.full)[,"1"]

imp.data$predict.full_dicho <- 
  predict(res.rpart.full)[,"1"] > 0.5

imp.data_cancer <- 
  imp.data %>% # c.m.p 2020/10/09 cancers were merged in the com.malignancies
  dplyr::filter(com.malignancies==1)

imp.data_cancer$predict.var.test <- 
  predict(res.rpart.cancer_all.var.test)[,"1"]

imp.data_cancer$predict.var.test_dicho <- 
  predict(res.rpart.cancer_all.var.test)[,"1"] > 0.5

imp.data_cancer$predict.var.test_commed <- 
  predict(res.rpart.cancer_all.var.test_commed)[,"1"]

imp.data_cancer$predict.var.test_commed_dicho <- 
  predict(res.rpart.cancer_all.var.test_commed)[,"1"] > 0.5

imp.data_auto_imn <- 
  imp.data %>%
  dplyr::filter(com.autoimm==1)

imp.data_auto_imn$predict.full <- 
  predict(res.rpart.auto_imn_all.full)[,"1"]

imp.data_auto_imn$predict.full_dicho <- 
  predict(res.rpart.auto_imn_all.full)[,"1"] > 0.5

table(round(imp.data$predict.var.test,2), imp.data[,var.y])
table(round(imp.data$predict.full,2), imp.data[,var.y])
table(round(imp.data_cancer$predict.var.test_commed,2), imp.data_cancer[,var.y])
table(round(imp.data_auto_imn$predict.full,2), imp.data_auto_imn[,var.y])

df.accuracy_predict(round(imp.data$predict.var.test,2), imp.data[,var.y]) %>% round (3)
df.accuracy_predict(round(imp.data$predict.full_dicho,2), imp.data[,var.y]) %>% round (3)

# Searching for clinical factors which are associated with errors ----------------------------------------------

#' Ratio of odds ratio between "correct" data to "error" data, that is selection bias from selecting "error" data
#' in evaluation of association between test results and a clinical factor, is test statistics.
#' The significance of observed data is evaluated by permutationt test of the clinical factor.
#'  

factor.med  <- 
  colnames(imp.data)[grep("^med",colnames(imp.data))]

factor.com  <- 
  colnames(imp.data)[grep("^com",colnames(imp.data))]

factor.med_com <-
  c(factor.med,factor.com)

# simulation --------------------------------------------------------------

test <- 
  mf.find_select_bias(
    obj.predict = res.rpart.var.test, 
    Data = imp.data, 
    colnames.targ = factor.med_com
    )

n.itt <- 2000

df.itt.simul_mf.find_select_bias <-
  data.frame(
    id=1:n.itt, itt=n.itt
    )

res.itt.simul_mf.find_select_bias <-
  df.itt.simul_mf.find_select_bias %>%
  ddply(.progress = "text",
    .variables = .(id),
    .fun = function(D){
      Data <- imp.data
      if(D$id!=1){
        Data.perm <- Data[sample(rownames(Data)),]
        Data[,factor.med_com] <- Data.perm[,factor.med_com]
        }
      test <- 
        mf.find_select_bias(
          obj.predict = res.rpart.var.test, 
          Data = Data, 
          colnames.targ = factor.med_com
          )
      return(test)
    }
  )

res_imp_maxBias.itt.simul_mf.find_select_bias <- 
  res.itt.simul_mf.find_select_bias %>% 
  ddply(
    .variables = .(factor),
    .fun = function(D){
      D.perm <- D[2:nrow(D),]
      D[D$Bias[1]==Inf,"Bias"] <- max(D.perm[!is.infinite(D.perm$Bias),"Bias"])
      D[D$Bias[1]==-Inf,"Bias"] <- min(D.perm[!is.infinite(D.perm$Bias),"Bias"])
      D.perm[D.perm$Bias==Inf,"Bias"] <- max(D.perm[!is.infinite(D.perm$Bias),"Bias"]) + 1
      D.perm[D.perm$Bias==-Inf,"Bias"] <- min(D.perm[!is.infinite(D.perm$Bias),"Bias"]) - 1
      
      D <- bind_rows(D[1,], D.perm)
      
      return(D)
      }
    )

ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias <- 
  res_imp_maxBias.itt.simul_mf.find_select_bias %>%
  ddply(
    .variables = .(factor),
    .fun = function(D){
      fun.ecdf <- ecdf(D$Bias)
      D$ecdf <- fun.ecdf(D$Bias)
      D$orig <- ifelse(D$id==1,1,0)
      return(D)
    } 
  ) %>%
  left_join(
    df.col_info[
      ,
      c("col_names","col_labels")
    ],
    by = c(
      "factor"="col_names"
    )
  ) %>%
  mutate(
    col_labels =
      factor(
        col_labels, 
        levels = df.col_info$col_labels
        )
    )

ggdata.ecdf <- 
  ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias %>% 
  ggplot(
    aes(x = log(Bias), y = ecdf)
    )

quartz(type = "pdf", width = 42,height = 21,
  file =
    sprintf(
      "%s/%s", dir.output.02,"ecdf.ratio_of_OR.pdf"
      )
  )
plot(
  ggdata.ecdf +
    geom_point() + 
    geom_text(
      aes(x=log(Bias),y=ecdf,label = sprintf("(%s, %s)",round(log(Bias),3),round(ecdf,3))),
      data=ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias[ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias$orig==1,],
      hjust = 0, vjust = 1.5, nudge_x = 0.05, color="royalblue"
      ) +
    geom_vline(aes(xintercept=log(Bias)),data = ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias[ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias$orig==1,]) +
    geom_hline(aes(yintercept=ecdf),data = ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias[ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias$orig==1,]) +
    facet_wrap(
      col_labels~.
      # factor ~ .
      ) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(color="black", size=16, family="Arial"),
      axis.text =  element_text(color="black", size=16, family="Arial")
    )
  )
dev.off()

write.csv(
  ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias[
    ecdf.res_imp_maxBias.itt.simul_mf.find_select_bias$id==1,
    ],
  file = sprintf(
    "%s/%s",
    dir.output.02,
    "ecdf.ratio_of_OR.csv"
    )
  )

# ROC ---------------------------------------------------------------------
#' c.p.m 2020/07/10: 
#'   subgroups PSL≥5mg and MTX users were added.
#'   correction for calculation of subjects numbers.

require(pROC)

roc.var.y.var.test <-
  pROC::roc(
    as.formula(sprintf("%s~%s", var.y,var.test)),
    data= imp.data,
    ci=TRUE,
    of='thresholds'
  )

roc.var.y.var.test_com.cancer_all <- # c.p.m 2020/07/10
  pROC::roc(
    as.formula(sprintf("%s~%s", var.y,var.test)),
    data= imp.data %>% # c.m.p 2020/10/09
      dplyr::filter(com.malignancies==1),
    ci=TRUE,
    of='thresholds'
  )

roc.var.y.var.test_com.auto_imn <-  # c.p.m 2020/07/10
  pROC::roc(
    as.formula(sprintf("%s~%s", var.y,var.test)),
    data= imp.data  %>% # c.p.m 2020/10/09
      dplyr::filter(com.autoimm==1),
    ci=TRUE,
    of='thresholds'
  )

roc.var.y.var.test_med.PSLgte.5 <-  # c.p.m 2020/07/10
  pROC::roc(
    as.formula(sprintf("%s~%s", var.y,var.test)),
    data= imp.data[imp.data$med.PSLgte.5==1,],
    ci=TRUE,
    of='thresholds'
    )

roc.var.y.var.test_med.MTX <-  # c.p.m 2020/07/10
  pROC::roc(
    as.formula(sprintf("%s~%s", var.y,var.test)),
    data= imp.data[imp.data$med.MTX==1,],
    ci=TRUE,
    of='thresholds'
  )


quartz(
  type = 'pdf',
  file = sprintf(
    '%s/%s.pdf', dir.output.02, 
    sprintf('roc.%s.pdf', var.y)
  ),
  pointsize = 20,
  family = "Arial"
)

# Total subjects

    plot(
      main=sprintf(
        "%s
        (test posi=%s; nega=%s)",
        "Total subjects", 
        length(roc.var.y.var.test$cases),
        length(roc.var.y.var.test$controls)
        ),
      roc.var.y.var.test,
      ci=TRUE,
      ci.type='bars',
      ci.col= "black",
      thre.col="blue",
      print.thres.best.method="youden",
      print.thres=TRUE,
      print.thres.col="royalblue",
      print.thres.cex=0.7,
      print.thres.adj=c(-0.4,4),
      segments.lwd= 0.5,
      lwd=4,
      identity.col="black",
      cex.axix = 2.5,
      cex.lab = 1.5, cex.main=0.7
    )
    
    legend(
      x = 0.6, y=0.5,cex = 0.7, 
      # lwd = c(2,2,0), lty = 1:2,
      legend = c(
        sprintf(
          "AUC = %s",
          round(auc(roc.var.y.var.test),3)
        )
      ),
      bty = "n"
    )
    
    # subgroup: cancer
    
    plot(
      main=sprintf(
        "%s
        (test posi=%s; nega=%s)",
        "Subgroup: Cancer", 
        length(roc.var.y.var.test_com.cancer_all$cases),
        length(roc.var.y.var.test_com.cancer_all$controls)
      ),
      roc.var.y.var.test_com.cancer_all,
      ci=TRUE,
      ci.type='bars',
      ci.col= "black",
      print.thres.best.method="youden",
      print.thres=TRUE,
      print.thres.col="royalblue",
      print.thres.cex=0.7,
      print.thres.adj=c(-0.4,4),
      segments.lwd= 0.5,
      lwd=4,
      identity.col="black",
      cex.axix = 2.5,
      cex.lab = 1.5,cex.main=0.7
    )
    
    legend(
      x = 0.6, y=0.5,cex = 0.7, 
      # lwd = c(2,2,0), lty = 1:2,
      legend = c(
        sprintf(
          "AUC = %s",
          round(auc(roc.var.y.var.test_com.cancer_all),3)
        )
      ),
      bty = "n"
    )
    
    # subgroup: auto-immn dis   # c.p.m 2020/07/10
    
    plot(
      main=sprintf(
        "%s
        (test posi=%s; nega=%s)",
        "subgroup: auto-immn. dis.", 
        length(roc.var.y.var.test_com.auto_imn$cases),
        length(roc.var.y.var.test_com.auto_imn$controls)
        ),
      roc.var.y.var.test_com.auto_imn,
      ci=TRUE,
      ci.type='bars',
      ci.col= "black",
      print.thres.best.method="youden",
      print.thres=TRUE,
      print.thres.col="royalblue",
      print.thres.cex=0.7,
      print.thres.adj=c(-0.4,4),
      segments.lwd= 0.5,
      lwd=4,
      identity.col="black",
      cex.axix = 2.5,
      cex.lab = 1.5, cex.main=0.7
    )
    
    legend(
      x = 0.6, y=0.5,cex = 0.7, 
      # lwd = c(2,2,0), lty = 1:2,
      legend = c(
        sprintf(
          "AUC = %s",
          round(auc(roc.var.y.var.test_com.auto_imn),3)
        )
      ),
      bty = "n"
    )
    
    
    # subgroup: PSL ≥ 5mg   # c.p.m 2020/07/10
    
    plot(
      main=sprintf(
        "%s
        (test posi=%s; nega=%s)",
        "subgroup: PSL ≥ 5mg", 
        length(roc.var.y.var.test_med.PSLgte.5$cases),
        length(roc.var.y.var.test_med.PSLgte.5$controls)
      ),
      roc.var.y.var.test_med.PSLgte.5,
      ci=TRUE,
      ci.type='bars',
      ci.col= "black",
      print.thres.best.method="youden",
      print.thres=TRUE,
      print.thres.col="royalblue",
      print.thres.cex=0.7,
      print.thres.adj=c(-0.4,4),
      segments.lwd= 0.5,
      lwd=4,
      identity.col="black",
      cex.axix = 2.5,
      cex.lab = 1.5, cex.main=0.7
    )
    
    legend(
      x = 0.6, y=0.5,cex = 0.7, 
      # lwd = c(2,2,0), lty = 1:2,
      legend = c(
        sprintf(
          "AUC = %s",
          round(auc(roc.var.y.var.test_med.PSLgte.5),3)
        )
      ),
      bty = "n"
    )
    
    
    # subgroup: MTX user    # c.p.m 2020/07/10
    
    plot(
      main=sprintf(
        "%s
        (test posi=%s; nega=%s)",
        "subgroup: MTX user", 
        length(roc.var.y.var.test_med.MTX$cases),
        length(roc.var.y.var.test_med.MTX$controls)
      ),
      roc.var.y.var.test_med.MTX,
      ci=TRUE,
      ci.type='bars',
      ci.col= "black",
      print.thres.best.method="youden",
      print.thres=TRUE,
      print.thres.col="royalblue",
      print.thres.cex=0.7,
      print.thres.adj=c(-0.4,4),
      segments.lwd= 0.5,
      lwd=4,
      identity.col="black",
      cex.axix = 2.5,
      cex.lab = 1.5, cex.main=0.7
    )
    
    legend(
      x = 0.6, y=0.5,cex = 0.7, 
      # lwd = c(2,2,0), lty = 1:2,
      legend = c(
        sprintf(
          "AUC = %s",
          round(auc(roc.var.y.var.test_med.MTX),3)
        )
      ),
      bty = "n"
    )
    
dev.off()

# Save data file for the next step of analysis. ---------------------------

imp.data.pred <- imp.data

save(
  imp.data.pred,　data.02,
  res.rpart.var.test,
  df.col_info,
  file = sprintf("%s/%s",dir.data,"res.main_02.diagAcc_rev.R.RData"))

# End ---------------------------------------------------------------------


