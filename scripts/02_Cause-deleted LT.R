############################################################
# 02_Cause-deleted_LT.R
# Feminicide and Female Life Expectancy in Mexico
# Author: Itzel Díaz-Juárez; Víctor M. Garcia-Guerrero
# 
# Description:
# Build cause-deleted life tables.
#
# Methodological basis:
# - Selection of external causes of death (Grushka & Kohan, 2020; Kohan, 2018)
# - Integration with Demographic Conciliation (INEGI, CONAPO)
# - Cause-deleted life tables (Chiang, 1968)
# - Multiple decrement life tables (Preston et al., 2001)
############################################################

# -------- 0. Setup-------

rm(list=ls())
setwd("")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, foreign, ggplot2, readr, janitor, dplyr, haven, car, expss, janitor, tidyr, tidyverse, survey, lubridate, R.utils, minpack.lm, gridExtra, Hmisc, wesanderson, openxlsx)

##------1) Loading data ---------

load("Ri_fem.RData")
load("mx_st.RData")
#load("mxfem_st.RData")

##

Ri_fem
mx_st
#estado <- "Chihuahua"

# ----2)Associated single decrement life table ----
ASDLTfem.func <- function(estado) {
  mx_estado <- mx_st[[estado]] %>%
    # filter(g_age %in% gage_sel) %>%
    select(-c(1:2)) %>% 
    as.matrix()
  
  ri_estado <- Ri_fem[[estado]] %>% 
    select(-c(1:2)) %>% 
    as.matrix()
  
  
  mx <- as.matrix(mx_estado)
  Ri <- as.matrix(ri_estado)
  
  #Life table 
  n <- matrix(5,dim(mx)[1],dim(mx)[2])
  n[c(1:2),] <- c(1,4)
  edad <- matrix(c(0, 1, seq(5, 85, 5)),
                 dim(mx)[1],dim(mx)[2])
  
  ax <- as.data.frame(n/2)
  
  for(i in 1:dim(mx)[2]){
    if(mx[1, i] >= 0.107) {
      ax[1, i] <- 0.35
      ax[2, i] <- 1.361
    } else {
      ax[1, i] <- 0.053 + 2.8*mx[1, i]
      ax[2, i] <- 1.522 + 1.518*mx[1, i]
    }
  }
  
  ax[dim(mx)[1],] <- 1/mx[dim(mx)[1],]
  
  dif <- 1
  while (dif > 0.00000000000001) {
    
    qx <- n*mx/(1+(n-ax)*mx)
    
    px <- 1-qx
    
    lx <- as.data.frame(matrix(1, dim(mx)[1], dim(mx)[2]))
    
    for(i in 2:dim(mx)[1]){
      lx[i,] <- lx[i-1,]*px[i-1,]
    }
    
    dx <- qx*lx
    
    ax1 <- ax
    ax[3:(dim(mx)[1] - 1), ] <-
      ((-n[2:(dim(mx)[1] - 2), ]/24) * dx[2:(dim(mx)[1] - 2), ]+
         (n[3:(dim(mx)[1] - 1),]/2) * dx[3:(dim(mx)[1] - 1),]+
         (n[4:dim(mx)[1], ]/24) * dx[4:dim(mx)[1], ])/dx[3:(dim(mx)[1] -  1), ]
    
    dif <- sum(abs(ax-ax1))
  } 
  
  Lx <- dx/mx
  
  Tx <- data.frame(matrix(0, dim(mx)[1], dim(mx)[2]))
  for(i in 1:dim(mx)[2]){
    Tx[,i] <- rev(cumsum(rev(Lx[,i])))
  }
  
  ex <- Tx/lx
  
  #Associated Single Decrement Life Table
  
  px[px < 0] <- 0
  
  px_i <- px^Ri
  
  #matplot(dx, type = "l")
  
  qx_i <- 1 - px_i
  
  #matplot(qx_i, type = "l", log = "y")
  
  lx_i <- data.frame(matrix(1, dim(mx)[1], dim(mx)[2]))
  for(i in 2:dim(mx)[1]){
    lx_i[i, ] <- lx_i[(i-1), ]*px_i[(i-1), ]
  }
  
  dx_i <- qx_i*lx_i
  
  ax_i <- data.frame(matrix(0, dim(mx)[1], dim(mx)[2]))
  
  # dejamos 2 primeros grupos iguales 
  ax_i[c(1, 2), ] <- ax[c(1, 2), ]
  #test
  #ax_i[1, ] <- 1 + Ri_ags[1,] * (qx[1,]/qx_i[1,])*(ax[1,]-1)
  #ax_i[2, ] <- 4 + Ri_ags[2,] * (qx[2,]/qx_i[2,])*(ax[1,]-4)
  
  #for open-ended interval
  ax_i[dim(mx)[1], ] <- ex[dim(mx)[1], ]/Ri[dim(mx)[1], ]
  # para el resto de las edades
  ax_i[c(3:(dim(mx)[1]-1)), ] <- ((-5*dx_i[c(2:(dim(mx)[1]-2)), ]/24)+
                                    (5*dx_i[c(3:(dim(mx)[1]-1)), ]/2)+
                                    (5*dx_i[c(4:dim(mx)[1]), ]/24))/dx_i[c(3:(dim(mx)[1]-1)), ]
  
  Lx_i <- data.frame(matrix(0, dim(mx)[1], dim(mx)[2]))
  
  Lx_i[c(1:(dim(mx)[1]-1)), ] <- 5*lx_i[c(2:dim(mx)[1]), ] +
    ax_i[c(1:(dim(mx)[1]-1)), ]*dx_i[c(1:(dim(mx)[1]-1)), ]
  
  # Last lx (open-ended interval)
  # esta Lx queda sólo con los que murieron en el intervalo
  Lx_i[dim(mx)[1], ] <- ax_i[dim(mx)[1], ]*dx_i[dim(mx)[1], ]
  
  Tx_i <- data.frame(matrix(0, dim(mx)[1], dim(mx)[2]))
  for(i in 1:20){
    Tx_i[, i] <- rev(cumsum(rev(Lx_i[, i])))
  }
  ex_i <- Tx_i/lx_i
  
  
  
  gained_yrs <- ex_i-ex
  gained_month <- 12*gained_yrs
  

  asdlt <- list(
    #g_age = rep(g_age_sel, each = 20),
    #year = rep(2000:2019, times = length(g_age_sel)),
    ax = as.tibble(ax),
    qx = as.tibble(qx),
    px = as.tibble(px),
    lx = as.tibble(lx),
    dx = as.tibble(dx),
    Lx = as.tibble(Lx),
    Tx = as.tibble(Tx),
    ex = as.tibble(ex),
    px_i = as.tibble(px_i),
    qx_i = as.tibble(qx_i),
    lx_i = as.tibble(lx_i),
    dx_i = as.tibble(dx_i),
    ax_i = as.tibble(ax_i),
    Lx_i = as.tibble(Lx_i),
    Tx_i = as.tibble(Tx_i),
    ex_i = as.tibble(ex_i),
    gained_yrs = as.tibble(gained_yrs),
    gained_month = as.tibble(gained_month)
    
  )
  
  
  g_age <- c(0, 1, seq(5, 85, 5))
  
  names(asdlt) <- c("ax", "qx", "px", "lx", "dx", "Lx", "Tx", "ex", "px_i", "qx_i", "lx_i", "dx_i", "ax_i", "Lx_i", "Tx_i", "ex_i", "gained_yrs", "gained_month")
  
  for (tbl_name in names(asdlt)){  
    if (inherits(asdlt[[tbl_name]], "data.frame")) {
      asdlt[[tbl_name]] <- cbind(g_age, asdlt[[tbl_name]])
    }
  }
  
  years <- as.character(2000:2019)
  for (i in seq_along(asdlt)) {
    if (inherits(asdlt[[i]], "data.frame")) {
      colnames(asdlt[[i]]) <- c("g_age", years)
    }
  }
  
  return(asdlt)
  
  
}

#tablas_chih <- ASDLTfem.func("Chihuahua")


#States list
states_list <- names(mx_st)

asdlt_st <- lapply(states_list, function(estado) {
  resultados <- ASDLTfem.func(estado)
  assign(paste0("asdlt_", tolower(gsub(" ", "", estado))), resultados, envir = .GlobalEnv)
  return(resultados)
})


#-------3)Saving all life tables from all states in a combined tibble (long format----------

list_lt <- list()

for (i in seq_along(asdlt_st)) {
  
  estado <- states_list[i]
  
  # Extracting data from each life table
  ax <- asdlt_st[[i]]$ax
  qx <- asdlt_st[[i]]$qx
  px <- asdlt_st[[i]]$px
  lx <- asdlt_st[[i]]$lx
  dx <- asdlt_st[[i]]$dx
  Lx <- asdlt_st[[i]]$Lx
  Tx <- asdlt_st[[i]]$Tx
  ex <- asdlt_st[[i]]$ex
  px_i <- asdlt_st[[i]]$px_i
  qx_i <- asdlt_st[[i]]$qx_i
  lx_i <- asdlt_st[[i]]$lx_i
  dx_i <- asdlt_st[[i]]$dx_i
  ax_i <- asdlt_st[[i]]$ax_i
  Lx_i <- asdlt_st[[i]]$Lx_i
  Tx_i <- asdlt_st[[i]]$Tx_i
  ex_i <- asdlt_st[[i]]$ex_i
  gained_years <- asdlt_st[[i]]$gained_yrs
  gained_month <- asdlt_st[[i]]$gained_month
  
  # Re-formatting data for each tibble
  ax_df <- ax %>%
    gather(key = "year", value = "ax", -g_age) %>%
    mutate(state = estado)
  
  qx_df <- qx %>%
    gather(key = "year", value = "qx", -g_age) %>%
    mutate(state = estado)
  
  px_df <- px %>%
    gather(key = "year", value = "px", -g_age) %>%
    mutate(state = estado)
  
  lx_df <- lx %>%
    gather(key = "year", value = "lx", -g_age) %>%
    mutate(state = estado)
  
  dx_df <- dx %>%
    gather(key = "year", value = "dx", -g_age) %>%
    mutate(state = estado)
  
  Lx_df <- Lx %>%
    gather(key = "year", value = "Lx", -g_age) %>%
    mutate(state = estado)
  
  Tx_df <- Tx %>%
    gather(key = "year", value = "Tx", -g_age) %>%
    mutate(state = estado)
  
  ex_df <- ex %>%
    gather(key = "year", value = "ex", -g_age) %>%
    mutate(state = estado)
  
  
  pxi_df <- px_i %>%
    gather(key = "year", value = "px_i", -g_age) %>%
    mutate(state = estado)
  
  qxi_df <- qx_i %>%
    gather(key = "year", value = "qx_i", -g_age) %>%
    mutate(state = estado)
  
  lxi_df <- lx_i %>%
    gather(key = "year", value = "lx_i", -g_age) %>%
    mutate(state = estado)
  
  dxi_df <- dx_i %>%
    gather(key = "year", value = "dx_i", -g_age) %>%
    mutate(state = estado)
  
  axi_df <- ax_i %>%
    gather(key = "year", value = "ax_i", -g_age) %>%
    mutate(state = estado)
  
  Lxi_df <- Lx_i %>%
    gather(key = "year", value = "Lx_i", -g_age) %>%
    mutate(state = estado)
  
  Txi_df <- Tx_i %>%
    gather(key = "year", value = "Tx_i", -g_age) %>%
    mutate(state = estado)
  
  exi_df <- ex_i %>%
    gather(key = "year", value = "ex_i", -g_age) %>%
    mutate(state = estado)
  
  gainedy_df <- gained_years %>%
    gather(key = "year", value = "gained_years", -g_age) %>%
    mutate(state = estado) 
  
  
  gainedm_df <- gained_month %>%
    gather(key = "year", value = "gained_months", -g_age) %>%
    mutate(state = estado) 
  
  # Full join all data frames
  combined_df <- full_join(ax_df, qx_df) %>%
    full_join(px_df) %>%
    full_join(lx_df) %>%
    full_join(dx_df) %>%
    full_join(Lx_df) %>%
    full_join(Tx_df) %>%
    full_join(ex_df) %>%
    full_join(pxi_df) %>%
    full_join(qxi_df) %>%
    full_join(lxi_df) %>%
    full_join(dxi_df) %>%
    full_join(axi_df) %>%
    full_join(Lxi_df) %>%
    full_join(Txi_df) %>%
    full_join(exi_df) %>%
    full_join(gainedy_df) %>% 
    full_join(gainedm_df)
  
  # Assign the combined data frame to the list
  list_lt[[i]] <- combined_df
}

# Combine all data frames in the list into one
combined_all <- bind_rows(list_lt) %>% 
  select(state, g_age, year, ax, qx, px, lx, dx, Lx, Tx, ex, px_i, qx_i, lx_i, dx_i, ax_i, Lx_i, Tx_i, ex_i, gained_years, gained_months) %>% 
  arrange(state, year, g_age)


LT_ST_comb_FINAL <- combined_all %>% 
  mutate(across(c(gained_years, gained_months), ~ifelse(. < 0, 0, .)))


save(LT_ST_comb_FINAL, file = "LT_ST_comb_FINAL.RData")
