
shinyServer(function(input, output) {
    
  fetch_store = eventReactive(input$button, {
    validate(
      need(input$url, 'Add valid url'))
    parsedat(input$url) 
    return ("A streamlined version of your dataset has been saved locally!")
    #return (tdat)
  })
  
  output$dummy = renderText({fetch_store()})
  
  para_sel = eventReactive(input$genedis, { 
    para2 = para[1:input$genedis,]
    para2$effect = ifelse(para2$parameter.values > 0, "-", "+")
    return (para2)
  })
  
  out_dat = eventReactive(input$button, {
  a = readdat()
  return (a)
  })
  
  output$summary_tab = DT::renderDataTable({out_dat()})
  
  nfeat = function(lambda_val){
    model3a = glmnet(xdat, y, family = "binomial", alpha = 1, lambda =reg$lambda.1se)
    print("AA")
    return(model3a$df)
  }
  
  
  
  output$demographic0 = renderText({paste0("Overarching summary:")})
  output$demographic1 = renderText({paste0("There are currently ",dim(para)[1], " genes selected in the optimal model.")})
  output$demographic2 = renderText({paste0("Also, there are ", nfeat(reg$lambda.1se) ," genes selected in the 'next best' effecient model.")})
  output$demographic3 = renderText({paste0("You have selected ", nfeat(input$lambda) ," genes with current lambda =", round(input$lambda,4))})
  
  gene_chosen = eventReactive(input$select, {
    xtograph = xdat_all %>% select("reject_stat", input$select)
    xtograph$reject_stat = as.factor(ifelse(xtograph$reject_stat == 1, "Acute Renal Rejection", "Healthy Graft"))
    return (xtograph)
  })
  
  
  full_dat = eventReactive(input$button, {
    validate(
      need(input$url, 'Add valid url'))
    a = parsefull()
    return (a)
  })

  cvtab = eventReactive(input$button3, {
    return(cvtab_lambda(input$lambda))})
  
  cvtab_lambda = function(val) {
    valdf = cv_eval %>% group_by(Classifier) %>% 
    summarise("mean Accuracy" =mean(Accuracy), "mean F1.Score" =mean(F1.Score)) %>% 
    arrange(desc(`mean Accuracy`)) 
    #update with user input
    update = cv_run(val)
    valdf$`mean Accuracy`[valdf$Classifier == "User-Defined Lasso Regression"] = update[1:30]
    valdf$`mean F1.Score`[valdf$Classifier == "User-Defined Lasso Regression"] = update[31:60]
    valdf$`mean Accuracy`= round(valdf$`mean Accuracy`,3)
    valdf$`mean F1.Score`= round(valdf$`mean F1.Score`,3)
    return(valdf)
  }
  
  output$cv_table = DT::renderDataTable({cvtab()})
  
  prep_cvdat = eventReactive(input$button3, {prep_cvdat_lambda(input$lambda)})
  
  prep_cvdat_lambda = function(val) {
    update = cv_run(val)
    cv_eval$Accuracy[cv_eval$Classifier == "User-Defined Lasso Regression"] = update[1:30]
    cv_eval$F1.Score[cv_eval$Classifier == "User-Defined Lasso Regression"] = update[31:60]
    cvdat = reshape2::melt(cv_eval, id.vars = "Classifier")
    cvdat2 = rename(cvdat, "Evaluative.Metric"= variable)
    return(cvdat2)
  }
  
  output$cv_plot = renderPlot({
    ggplot(prep_cvdat(), aes(Classifier, value, fill = Evaluative.Metric)) + 
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE) + scale_fill_brewer(palette="RdBu") +
      ggtitle("Repeated CV of LASSO/Logisitic regression and Random Forest") +
      xlab("Evaluative Metric") +scale_y_continuous(name= "Score", breaks = c(3:10*0.10), limits=c(0.3, 1.0))
  })
  
  predict_fn = function(data) {
    #cbind(intercept = rep(1, nrow(data)), data)
    testx = model.matrix(~., data)[,-1]
    preds = ifelse(predict(modelf, testx, type="response") > 0.5, "Acute Renal Rejection", "Healthy Graft")
    sampli = paste0("Sample No ", c(1:length(preds)))
    fin = data.frame("Sample"=sampli, "Prediction_Results" = as.character(preds))
    #fin[, ] <- lapply(fin[, ], as.character)
    return(fin)}
  
  output$predictions = DT::renderDataTable({
    pred = full_dat() %>% predict_fn()
    return (pred)
    })
  
  
  cv_run = function(lambda_val) {
  k <- 3
  accLASSOB_50times = F1LASSOB_50times = accB = F1B = c()
  for (j in 1:30) {
    print(j)
    fold <- caret::createFolds(y, k)
    
    for(i in 1:length(fold)){
      model3a = glmnet(xdat[-fold[[i]],], y[-fold[[i]]], family = "binomial", alpha = 1, lambda =lambda_val)
      #reg$lambda[reg$nzero == chosen_val][1]
      preds3a <- ifelse(predict(model3a, xdat[fold[[i]],], type ="response") > 0.5, 1, 0) 
      tab_a = table(preds3a,y[fold[[i]]])
      accB[i] = tab_a %>% diag %>% sum %>% `/`(nrow(xdat[fold[[i]],]))
      F1B[i] = F1(tab_a)}
    
    accLASSOB_50times <- append(accLASSOB_50times, mean(accB))
    F1LASSOB_50times <- append(F1LASSOB_50times, mean(F1B))
  }
    return(c(accLASSOB_50times,F1LASSOB_50times))
}
  

  
  
#[only reactive, no button!]
#  fetch_data = reactive({
#    weather = get_current_weather(input$location)
#    return(weather)
#  })
  
#for SHINY, you need to create a reactive function instead of just generally parsing data!
  #note PLOTLY for interactive plots!
  
  output$parameter_plot = renderPlot({
     para_sel() %>% ggplot(aes(x = reorder(X,parameter.values),
                              y = exp(parameter.values)-1 , fill = effect)) + geom_col()+ scale_fill_brewer(palette="Pastel1") + labs(x = "Gene Expresssed", title = "Odds ratio -1 of most influential genes") + theme(text = element_text(face = "bold", size = 15), legend.position = "none") + coord_flip() + scale_y_continuous(name= "Parameter value(transformed)", breaks = c(-7:12*0.10), limits=c(-0.7, 1.2))
  })
  
  output$gene_specific = renderPlot({
    xtograph = gene_chosen()
    mu_r =mean(xtograph[2][xtograph[1] == "Acute Renal Rejection"])
    mu_h =mean(xtograph[2][xtograph[1] == "Healthy Graft"])
    
    ggplot(xtograph, aes_string(x=input$select, fill="reject_stat")) +
      geom_density(alpha =0.4)+labs(fill="Rejection Status", color = "", title = "Gene Expression categorised by rejection status")+
      geom_vline(aes(xintercept=mu_r, color="ARR Median"),linetype="dashed")+
      geom_vline(aes(xintercept=mu_h, color="Healthy Graft Median"),linetype="dashed")+ theme_minimal() +theme(text = element_text(face = "bold", size = 15))
  })
  
  obs = observe({
    print(
      paste0(input$url, " is the link!"))
  })
  
})

#input inherited from ui.R, output is used as part of the function 
#(the renderplot is consequentially saved into output...)
