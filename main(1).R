#!/usr/bin/env Rscript

##################
# soccer_modeling.R
# 足球比赛建模分析
##################

# 设置日志文件
log_file <- file.path("logs", format(Sys.time(), "modeling_log_%Y%m%d_%H%M%S.txt"))
dir.create("logs", showWarnings = FALSE)

#' 日志函数
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(message, "\n", file = log_file, append = TRUE)
  if(level %in% c("ERROR", "WARNING")) {
    cat(message, "\n")
  }
}

#' 加载必要的包
load_packages <- function() {
  # 设置CRAN镜像
  options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
  
  packages <- c(
    "tidyverse", "caret", "glmnet", "brms", "lme4", "MASS", 
    "geepack", "performance", "MatchIt", "nnet"
  )
  
  for(pkg in packages) {
    tryCatch({
      if(!requireNamespace(pkg, quietly = TRUE)) {
        log_message(sprintf("Installing package: %s", pkg))
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
      log_message(sprintf("Successfully loaded package: %s", pkg))
    }, error = function(e) {
      log_message(sprintf("Failed to load package %s: %s", pkg, e$message), "ERROR")
      stop(sprintf("Failed to load package %s", pkg))
    })
  }
}

#' 加载和验证数据
load_and_validate <- function() {
  # 检查处理后的数据文件是否存在
  data_files <- list(
    all = "processed_data/all_leagues_processed.rds",
    bundesliga = "processed_data/bundesliga_processed.rds",
    laliga = "processed_data/laliga_processed.rds", 
    ligue1 = "processed_data/ligue1_processed.rds",
    premier = "processed_data/premier_processed.rds",
    seriea = "processed_data/seriea_processed.rds"
  )
  
  data_list <- list()
  
  for(name in names(data_files)) {
    file_path <- data_files[[name]]
    tryCatch({
      if(file.exists(file_path)) {
        data_list[[name]] <- readRDS(file_path)
        log_message(sprintf("Successfully loaded data for %s", name))
      } else {
        log_message(sprintf("File not found: %s", file_path), "WARNING")
      }
    }, error = function(e) {
      log_message(sprintf("Error loading %s: %s", name, e$message), "ERROR")
    })
  }
  
  if(length(data_list) == 0) {
    stop("No data files could be loaded")
  }
  
  return(data_list)
}

#' No Polling Models
fit_no_polling <- function(data, leagues) {
  models <- list()
  
  # 检查必需的变量是否存在
  required_vars <- c("League", "TotalGoals", "HomeResult", "GoalDiff",
                     "HS", "AS", "HST", "AST", "HF", "AF", "HC", "AC",
                     "HomeAttackEff", "AwayAttackEff", "HomeDominance")
  
  missing_vars <- setdiff(required_vars, names(data))
  if(length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", 
                 paste(missing_vars, collapse = ", ")))
  }
  
  for(league in leagues) {
    log_message(sprintf("Fitting models for league: %s", league))
    
    league_data <- data %>% 
      filter(League == league)
    
    if(nrow(league_data) == 0) {
      log_message(sprintf("No data available for league: %s", league), "WARNING")
      next
    }
    
    tryCatch({
      # 1. 总进球数预测（使用泊松回归）
      goals_formula <- TotalGoals ~ HS + AS + HST + AST + HF + AF + HC + AC +
        HomeAttackEff + AwayAttackEff + HomeDominance
      
      goals_model <- glm(goals_formula,
                         family = poisson(link = "log"),
                         data = league_data,
                         control = list(maxit = 100))
      
      # 2. 比赛结果预测（使用多项式逻辑回归）
      result_formula <- HomeResult ~ HS + AS + HST + AST + HF + AF + HC + AC +
        HomeAttackEff + AwayAttackEff + HomeDominance
      
      result_model <- multinom(result_formula,
                               data = league_data,
                               trace = FALSE,
                               maxit = 1000)
      
      # 3. 进球差预测（使用线性回归）
      diff_formula <- GoalDiff ~ HS + AS + HST + AST + HF + AF + HC + AC +
        HomeAttackEff + AwayAttackEff + HomeDominance
      
      diff_model <- lm(diff_formula, data = league_data)
      
      models[[league]] <- list(
        goals = goals_model,
        result = result_model,
        diff = diff_model
      )
      
      log_message(sprintf("Successfully fitted models for league: %s", league))
      
    }, error = function(e) {
      log_message(sprintf("Error in model fitting for league %s: %s", 
                          league, e$message), "ERROR")
      return(NULL)
    })
  }
  
  if(length(models) == 0) {
    log_message("No models could be fitted", "WARNING")
  }
  
  return(models)
}

#' Partial Polling Models
fit_partial_polling <- function(data) {
  log_message("Starting partial polling models fitting")
  
  # 数据预处理
  data <- data %>%
    group_by(League) %>%
    mutate(
      AttackingStyle = mean(TotalGoals, na.rm = TRUE) > median(TotalGoals, na.rm = TRUE),
      LeagueStrength = mean(TotalShots, na.rm = TRUE) > median(TotalShots, na.rm = TRUE),
      LeagueType = factor(paste0(
        ifelse(AttackingStyle, "High", "Low"), "_",
        ifelse(LeagueStrength, "Strong", "Weak")
      )),
      match_id = row_number(),
      time_index = match_id
    ) %>%
    ungroup()
  
  tryCatch({
    # 1. 多层泊松回归（进球数）
    log_message("Fitting multilevel Poisson regression...")
    mlm_goals <- glmer(
      TotalGoals ~ HS + AS + (1|League),
      family = poisson(link = "log"),
      data = data,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
    
    # 2. 多层多项式逻辑回归（比赛结果）
    log_message("Fitting multilevel multinomial regression...")
    mlm_result <- multinom(
      HomeResult ~ HS + AS + League,
      data = data,
      trace = FALSE,
      MaxNWts = 5000
    )
    
    # 3. GEE模型（考虑时间相关性）
    log_message("Fitting GEE model...")
    data$id <- as.numeric(factor(data$League))
    data$wave <- as.numeric(factor(data$match_id))
    
    gee_model <- geeglm(
      TotalGoals ~ HS + AS,
      id = id,
      wave = wave,
      data = data,
      family = poisson(link = "log"),
      corstr = "exchangeable"
    )
    
    log_message("Successfully fitted all partial polling models")
    
    return(list(
      mlm_goals = mlm_goals,
      mlm_result = mlm_result,
      gee = gee_model
    ))
    
  }, error = function(e) {
    log_message(sprintf("Error in partial polling models: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' Complete Polling Models
fit_complete_polling <- function(data) {
  log_message("Starting complete polling models fitting")
  
  tryCatch({
    # 1. 泊松回归（总进球数）
    log_message("Fitting Poisson regression...")
    complete_goals <- glm(
      TotalGoals ~ HS + AS + HST + AST + League,
      family = poisson(),
      data = data,
      control = list(maxit = 1000)
    )
    
    # 2. 多项式逻辑回归（比赛结果）
    log_message("Fitting multinomial regression...")
    complete_result <- multinom(
      HomeResult ~ HS + AS + HST + AST + League,
      data = data,
      trace = FALSE,
      MaxNWts = 5000
    )
    
    # 3. 贝叶斯多层模型
    log_message("Fitting Bayesian multilevel model...")
    bayes_model <- brm(
      formula = TotalGoals ~ HS + AS + HST + AST + (1|League),
      data = data,
      family = poisson(),
      prior = c(
        prior(normal(0, 5), class = "b"),
        prior(normal(0, 2), class = "Intercept"),
        prior(student_t(3, 0, 2), class = "sd")
      ),
      chains = 4,
      iter = 2000,
      warmup = 1000,
      cores = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    )
    
    log_message("Successfully fitted all complete polling models")
    
    return(list(
      goals = complete_goals,
      result = complete_result,
      bayes = bayes_model
    ))
    
  }, error = function(e) {
    log_message(sprintf("Error in complete polling models: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' 计算Kappa系数
calculate_kappa <- function(actual, predicted) {
  # 创建混淆矩阵
  conf_matrix <- table(actual, predicted)
  
  # 计算观察到的一致性
  observed_agreement <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # 计算期望的一致性
  row_sums <- rowSums(conf_matrix)
  col_sums <- colSums(conf_matrix)
  expected_agreement <- sum(row_sums * col_sums) / (sum(conf_matrix)^2)
  
  # 计算 Kappa
  kappa <- (observed_agreement - expected_agreement) / (1 - expected_agreement)
  
  return(kappa)
}

#' 模型评估
evaluate_models <- function(no_polling, partial_polling, complete_polling, data) {
  results <- list()
  
  # 1. 评估No Polling模型
  log_message("Evaluating No Polling models...")
  no_polling_metrics <- list()
  
  for(league in names(no_polling)) {
    if(!is.null(no_polling[[league]])) {
      metrics <- list()
      tryCatch({
        # 进球数预测评估
        pred_goals <- predict(no_polling[[league]]$goals, newdata = data, type = "response")
        metrics$goals <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_goals)^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_goals), na.rm = TRUE)
        )
        
        # 比赛结果预测评估
        pred_result <- predict(no_polling[[league]]$result, newdata = data)
        metrics$result <- c(
          Accuracy = mean(pred_result == data$HomeResult, na.rm = TRUE),
          Kappa = calculate_kappa(data$HomeResult, pred_result)
        )
        
        no_polling_metrics[[league]] <- metrics
        
      }, error = function(e) {
        log_message(sprintf("Error in evaluating no polling model for %s: %s", league, e$message), "WARNING")
      })
    }
  }
  
  results$no_polling <- no_polling_metrics
  
  # 2. 评估Partial Polling模型
  if(!is.null(partial_polling)) {
    partial_metrics <- list()
    
    tryCatch({
      if(!is.null(partial_polling$mlm_goals)) {
        pred_goals <- predict(partial_polling$mlm_goals, newdata = data, 
                              re.form = NULL)
        partial_metrics$goals <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_goals)^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_goals), na.rm = TRUE)
        )
      }
      
      if(!is.null(partial_polling$mlm_result)) {
        pred_result <- predict(partial_polling$mlm_result, newdata = data)
        partial_metrics$result <- c(
          Accuracy = mean(pred_result == data$HomeResult, na.rm = TRUE),
          Kappa = calculate_kappa(data$HomeResult, pred_result)
        )
      }
      
      results$partial_polling <- partial_metrics
      
    }, error = function(e) {
      log_message(sprintf("Error in partial polling evaluation: %s", e$message), "WARNING")
    })
  }
  
  # 3. 评估Complete Polling模型
  if(!is.null(complete_polling)) {
    complete_metrics <- list()
    
    tryCatch({
      # 评估泊松回归
      if(!is.null(complete_polling$goals)) {
        pred_goals <- predict(complete_polling$goals, newdata = data, 
                              type = "response")
        complete_metrics$goals <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_goals)^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_goals), na.rm = TRUE)
        )
      }
      
      # 评估多项式逻辑回归
      if(!is.null(complete_polling$result)) {
        pred_result <- predict(complete_polling$result, newdata = data)
        complete_metrics$result <- c(
          Accuracy = mean(pred_result == data$HomeResult, na.rm = TRUE),
          Kappa = calculate_kappa(data$HomeResult, pred_result)
        )
      }
      
      # 评估贝叶斯模型 - 修改这部分的处理方式
      if(!is.null(complete_polling$bayes)) {
        pred_bayes <- fitted(complete_polling$bayes)  # 使用fitted()而不是predict()
        complete_metrics$bayes <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_bayes[,1])^2, na.rm = TRUE)),  # 使用第一列（均值）
          MAE = mean(abs(data$TotalGoals - pred_bayes[,1]), na.rm = TRUE)
        )
      }
      
      results$complete_polling <- complete_metrics
      
    }, error = function(e) {
      log_message(sprintf("Error in complete polling evaluation: %s", e$message), "WARNING")
    })
  }
  
  return(results)
}

#' 创建结果摘要 - 修改以适应新的结果结构
create_summary <- function(eval_results) {
  # 创建空数据框
  summary_df <- data.frame(
    model_type = character(),
    goals_rmse = numeric(),
    goals_mae = numeric(),
    result_accuracy = numeric(),
    result_kappa = numeric(),
    bayes_rmse = numeric(),
    bayes_mae = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 处理No Polling结果
  if(!is.null(eval_results$no_polling)) {
    goals_metrics <- sapply(eval_results$no_polling, function(x) x$goals)
    result_metrics <- sapply(eval_results$no_polling, function(x) x$result)
    
    no_polling_summary <- data.frame(
      model_type = "no_polling",
      goals_rmse = mean(unlist(goals_metrics["RMSE",]), na.rm = TRUE),
      goals_mae = mean(unlist(goals_metrics["MAE",]), na.rm = TRUE),
      result_accuracy = mean(unlist(result_metrics["Accuracy",]), na.rm = TRUE),
      result_kappa = mean(unlist(result_metrics["Kappa",]), na.rm = TRUE),
      bayes_rmse = NA,
      bayes_mae = NA
    )
    summary_df <- rbind(summary_df, no_polling_summary)
  }
  
  # 处理Partial Polling结果
  if(!is.null(eval_results$partial_polling)) {
    partial_polling_summary <- data.frame(
      model_type = "partial_polling",
      goals_rmse = eval_results$partial_polling$goals["RMSE"],
      goals_mae = eval_results$partial_polling$goals["MAE"],
      result_accuracy = eval_results$partial_polling$result["Accuracy"],
      result_kappa = eval_results$partial_polling$result["Kappa"],
      bayes_rmse = NA,
      bayes_mae = NA
    )
    summary_df <- rbind(summary_df, partial_polling_summary)
  }
  
  # 处理Complete Polling结果
  if(!is.null(eval_results$complete_polling)) {
    complete_polling_summary <- data.frame(
      model_type = "complete_polling",
      goals_rmse = eval_results$complete_polling$goals["RMSE"],
      goals_mae = eval_results$complete_polling$goals["MAE"],
      result_accuracy = eval_results$complete_polling$result["Accuracy"],
      result_kappa = eval_results$complete_polling$result["Kappa"],
      bayes_rmse = eval_results$complete_polling$bayes["RMSE"],
      bayes_mae = eval_results$complete_polling$bayes["MAE"]
    )
    summary_df <- rbind(summary_df, complete_polling_summary)
  }
  
  rownames(summary_df) <- summary_df$model_type
  summary_df$model_type <- NULL
  
  return(summary_df)
}

#' 保存结果函数
save_results <- function(results, eval_results, summary_results, base_dir = "model_results") {
  tryCatch({
    # 创建结果目录
    if(!dir.exists(base_dir)) {
      dir.create(base_dir)
    }
    
    # 保存模型结果
    saveRDS(results, file.path(base_dir, "model_objects.rds"))
    
    # 保存评估结果
    saveRDS(eval_results, file.path(base_dir, "evaluation_results.rds"))
    
    # 保存摘要结果到CSV
    write.csv(summary_results, file.path(base_dir, "model_summary.csv"),
              row.names = TRUE)
    
    log_message("Successfully saved all results")
    
  }, error = function(e) {
    log_message(sprintf("Error saving results: %s", e$message), "ERROR")
    stop("Failed to save results")
  })
}

#' 主函数
main <- function() {
  # 设置随机数种子以确保可重复性
  set.seed(123)
  
  # 创建结果目录
  results_dir <- "model_results"
  if(!dir.exists(results_dir)) {
    dir.create(results_dir)
    log_message(sprintf("Created results directory: %s", results_dir))
  }
  
  # 加载必要的包
  load_packages()
  
  # 加载数据
  tryCatch({
    log_message("Loading and validating data...")
    data_list <- load_and_validate()
    
    # 检查数据是否成功加载
    if(is.null(data_list$all)) {
      stop("Failed to load all leagues data")
    }
    
    # 获取联赛列表
    leagues <- unique(data_list$all$League)
    
    if(length(leagues) == 0) {
      stop("No league data available")
    }
    
    # 1. No Polling Models
    log_message("Fitting No Polling models...")
    no_polling_models <- fit_no_polling(data_list$all, leagues)
    
    # 2. Partial Polling Models
    log_message("Fitting Partial Polling models...")
    partial_polling_models <- fit_partial_polling(data_list$all)
    
    # 3. Complete Polling Models
    log_message("Fitting Complete Polling models...")
    complete_polling_models <- fit_complete_polling(data_list$all)
    
    # 评估模型
    log_message("Evaluating models...")
    eval_results <- evaluate_models(
      no_polling_models,
      partial_polling_models,
      complete_polling_models,
      data_list$all
    )
    
    # 创建结果摘要
    log_message("Creating results summary...")
    summary_results <- create_summary(eval_results)
    
    # 打印评估结果
    cat("\nModel Evaluation Results:\n")
    print(summary_results)
    
    # 模型比较
    cat("\nModel Performance Comparison:\n")
    
    # 进球预测比较
    cat("\nGoals Prediction Performance:\n")
    goals_comparison <- summary_results[, c("goals_rmse", "goals_mae")]
    print(goals_comparison[order(goals_comparison$goals_rmse), ])
    
    # 结果预测比较
    cat("\nMatch Result Prediction Performance:\n")
    result_comparison <- summary_results[, c("result_accuracy", "result_kappa")]
    print(result_comparison[order(result_comparison$result_accuracy, decreasing = TRUE), ])
    
    # 贝叶斯模型结果
    if(any(!is.na(summary_results$bayes_rmse))) {
      cat("\nBayesian Model Performance:\n")
      bayes_comparison <- summary_results[!is.na(summary_results$bayes_rmse), 
                                          c("bayes_rmse", "bayes_mae")]
      print(bayes_comparison)
    }
    
    # 保存所有结果
    log_message("Saving results...")
    
    # 保存模型对象
    saveRDS(list(
      no_polling = no_polling_models,
      partial_polling = partial_polling_models,
      complete_polling = complete_polling_models
    ), file.path(results_dir, "model_objects.rds"))
    
    # 保存评估结果
    saveRDS(eval_results, file.path(results_dir, "evaluation_results.rds"))
    
    # 保存摘要结果
    write.csv(summary_results, file.path(results_dir, "model_summary.csv"))
    
    # 输出联赛统计信息
    log_message("\nLeague Statistics:")
    league_stats <- data_list$all %>%
      group_by(League) %>%
      summarize(
        Matches = n(),
        AvgGoals = mean(TotalGoals, na.rm = TRUE),
        HomeWinRate = mean(HomeResult == "Win", na.rm = TRUE),
        AvgShots = mean(TotalShots, na.rm = TRUE)
      )
    print(league_stats)
    
    log_message("Analysis completed successfully")
    
  }, error = function(e) {
    log_message(sprintf("Error in main execution: %s", e$message), "ERROR")
    stop("Analysis failed")
  })
}

# 运行主函数
if(!interactive()) {
  tryCatch({
    main()
  }, error = function(e) {
    log_message(sprintf("Critical error in main execution: %s", e$message), "ERROR")
    quit(status = 1)
  })
}