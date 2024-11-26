#!/usr/bin/env Rscript

##################
# soccer_data_processing.R
# 足球数据预处理
##################

# 加载必要的包
library(tidyverse)

# 数据预处理函数
process_league_data <- function(file_path, league_name) {
  # 读取CSV数据
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # 确保所有必需的列都存在
  required_cols <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR",
                     "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST",
                     "HF", "AF", "HC", "AC", "HY", "AY", "HR", "AR")
  
  # 检查并报告缺失的列
  missing_cols <- setdiff(required_cols, names(df))
  if(length(missing_cols) > 0) {
    stop(sprintf("Missing columns in %s: %s", 
                 league_name, 
                 paste(missing_cols, collapse = ", ")))
  }
  
  # 首先只选择必需的列
  df <- df[, required_cols]
  
  # 添加联赛标识
  df$League <- league_name
  
  # 确保数值列的类型正确
  numeric_cols <- c("FTHG", "FTAG", "HTHG", "HTAG", "HS", "AS", "HST", "AST",
                    "HF", "AF", "HC", "AC", "HY", "AY", "HR", "AR")
  
  for(col in numeric_cols) {
    df[[col]] <- as.numeric(df[[col]])
    # 将NA替换为0
    df[[col]][is.na(df[[col]])] <- 0
  }
  
  # 计算总射门数
  df$TotalShots <- df$HS + df$AS
  
  # 计算主场进攻效率，避免除以0
  df$HomeAttackEff <- ifelse(df$HS > 0, df$HST / df$HS, 0)
  
  # 计算客场进攻效率，避免除以0
  df$AwayAttackEff <- ifelse(df$AS > 0, df$AST / df$AS, 0)
  
  # 计算主场控制力
  total_actions <- df$HS + df$HC + df$AS + df$AC
  df$HomeDominance <- ifelse(total_actions > 0,
                             (df$HS + df$HC) / total_actions,
                             0.5)
  
  # 计算总进球数
  df$TotalGoals <- df$FTHG + df$FTAG
  
  # 计算进球差
  df$GoalDiff <- df$FTHG - df$FTAG
  
  # 转换比赛结果为因子
  df$HomeResult <- factor(ifelse(df$GoalDiff > 0, "Win",
                                 ifelse(df$GoalDiff < 0, "Loss", "Draw")),
                          levels = c("Win", "Draw", "Loss"))
  
  return(df)
}

# 主函数
main <- function() {
  # 设置数据文件路径
  data_files <- list(
    premier = list(
      path = "~/Desktop/premier-league1819.csv",
      name = "Premier League"
    ),
    laliga = list(
      path = "~/Desktop/la-ligaseason-1819.csv",
      name = "La Liga"
    ),
    bundesliga = list(
      path = "~/Desktop/bundesliga season-1819.csv",
      name = "Bundesliga"
    ),
    seriea = list(
      path = "~/Desktop/serie-a1819.csv",
      name = "Serie A"
    ),
    ligue1 = list(
      path = "~/Desktop/ligue-1-1819.csv",
      name = "Ligue 1"
    )
  )
  
  # 创建空列表存储处理后的数据
  processed_data <- list()
  
  # 处理每个联赛的数据
  for(league in names(data_files)) {
    tryCatch({
      cat(sprintf("Processing %s data...\n", data_files[[league]]$name))
      
      # 检查文件是否存在
      if(!file.exists(data_files[[league]]$path)) {
        stop(sprintf("File not found: %s", data_files[[league]]$path))
      }
      
      processed_data[[league]] <- process_league_data(
        data_files[[league]]$path,
        data_files[[league]]$name
      )
      
      # 验证处理后的数据结构
      if(ncol(processed_data[[league]]) != 29) {  # 检查列数
        stop(sprintf("Incorrect number of columns in processed data for %s", 
                     data_files[[league]]$name))
      }
      
    }, error = function(e) {
      cat(sprintf("Error processing %s: %s\n", data_files[[league]]$name, e$message))
      return(NULL)
    })
  }
  
  # 移除处理失败的数据
  processed_data <- processed_data[!sapply(processed_data, is.null)]
  
  if(length(processed_data) == 0) {
    stop("No data was successfully processed")
  }
  
  # 确保所有数据框具有相同的列
  first_cols <- names(processed_data[[1]])
  for(league in names(processed_data)) {
    if(!identical(names(processed_data[[league]]), first_cols)) {
      stop(sprintf("Column mismatch in %s", data_files[[league]]$name))
    }
  }
  
  # 合并所有联赛数据
  cat("Merging league data...\n")
  all_leagues <- do.call(rbind, processed_data)
  
  # 创建输出目录
  if(!dir.exists("processed_data")) {
    dir.create("processed_data")
  }
  
  # 保存处理后的数据
  cat("Saving processed data...\n")
  saveRDS(all_leagues, "processed_data/all_leagues_processed.rds")
  
  # 保存各联赛的单独数据
  for(league in names(processed_data)) {
    saveRDS(processed_data[[league]], 
            sprintf("processed_data/%s_processed.rds", tolower(league)))
  }
  
  # 输出数据基本信息
  cat("\nData Summary:\n")
  cat(sprintf("Total matches: %d\n", nrow(all_leagues)))
  cat("\nMatches by league:\n")
  print(table(all_leagues$League))
  
  # 简单的数据验证
  cat("\nColumns in processed data:\n")
  print(names(all_leagues))
  
  # 返回处理后的数据
  invisible(list(
    all = all_leagues,
    by_league = processed_data
  ))
}

# 运行主函数
if(!interactive()) {
  main()
}