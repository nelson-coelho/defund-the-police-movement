# ==============================================================================
# Project: Material of Policing (DTP Movement)
# Version: Article v1
#
# Purpose: This code instructs R to use additional threads to improve the
#          performance of all the other scripts (by default, R uses only 1 CPU core).
#
# Funding: This research was supported by funding from the Center for Studies of 
# the Economic Order at the Federal University of Sao Paulo (CEOE/Unifesp). The 
# funding was allocated through Decentralized Execution Agreement (02/2020) in 
# partnership with the Diffuse Rights Fund, managed by the National Consumer 
# Secretariat of the Ministry of Justice and Public Security (Process SEI 
# no. 08012.003253/2018-45).
#
# Author: Nelson Coelho
# Organization: Centro de Estudos da Ordem Economica - CEOE/Unifesp
# Date: 2025-2-07
#
# File Name: multicore.R
#
# Description: This file contains R code to improve the program's performance
#              by using additional cores.
#
# Modifications:
#
# Changes:
#
# - 2025-2-07: Initial version by Nelson Coelho
#
# ==============================================================================

# Install and load required packages ===========================================
required_packages <- c("future.apply")

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load libraries ===============================================================
library("future.apply") # Contains function future_lappy to use additional cores

# ==============================================================================
#gets number of threads available
availableCores()

#sets number of threads to be used
x <- 1:12

# Multicore in parallel
plan(multicore)
y <- future_lapply(x, FUN = quantile, probs = 1:3/4)
print(y)

