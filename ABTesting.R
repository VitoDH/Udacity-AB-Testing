setwd("E:/Code/Project/Udacity-AB-Testing/data/")

#------- load the data
experiment <- read.csv("Final Project Results - Experiment.csv")
control <- read.csv("Final Project Results - Control.csv")


#----------- 3. Measuring Standard Deviation
# using baseline values
n_views_base <- 40000
n_clicks_base <- 3200
n_enroll_base <- 660
p_gross_conversion <- 0.20625
p_retention <- 0.53
p_net_conversion <- 0.1093125

# notice the corresponding N in the denominator is calculated under 5000 unique cookies
n_views_sample <- 5000
sd_gross_conversion <- sqrt(p_gross_conversion*(1-p_gross_conversion)/(n_views_sample*n_clicks_base/n_views_base))
sd_gross_conversion
sd_retention <- sqrt(p_retention*(1-p_retention)/(n_views_sample*n_enroll_base/n_views_base))
sd_retention
sd_net_conversion <- sqrt(p_net_conversion*(1-p_net_conversion)/(n_views_sample*n_clicks_base/n_views_base))
sd_net_conversion

#-------------5. Experiment Analysis
n_exp <- nrow(experiment)
n_cont <- nrow(control)

#--------- Sanity Check

#----- number of cookies (views)
p_views_obs <- sum(control$Pageviews)/(sum(experiment$Pageviews)+sum(control$Pageviews))
p_views_obs
sd_views_obs <- sqrt(0.5*0.5/(sum(control$Pageviews)+sum(experiment$Pageviews)))
ci_views_lower_obs <- 0.5-1.96*sd_views_obs
ci_views_lower_obs
ci_views_upper_obs <- 0.5+1.96*sd_views_obs
ci_views_upper_obs

#------ number of clicks
p_clicks_obs <- sum(control$Clicks)/(sum(experiment$Clicks)+sum(control$Clicks))
p_clicks_obs
sd_clicks_obs <- sqrt(0.5*0.5/(sum(control$Clicks)+sum(experiment$Clicks)))
ci_clicks_lower_obs <- 0.5-1.96*sd_clicks_obs
ci_clicks_lower_obs
ci_clicks_upper_obs <- 0.5+1.96*sd_clicks_obs
ci_clicks_upper_obs

#------- click through probability
# Method 1
ctp_exp <- sum(experiment$Clicks)/sum(experiment$Pageviews)
ctp_cont <- sum(control$Clicks)/sum(control$Pageviews)
ctp_pool <- (sum(experiment$Clicks)+sum(control$Clicks))/(sum(experiment$Pageviews)+sum(control$Pageviews))

# expected value
sd_cont <- sqrt(ctp_cont*(1-ctp_cont)/sum(control$Pageviews))
ci_ctp_lower <- ctp_cont - 1.96*sd_cont
ci_ctp_lower
ci_ctp_upper <- ctp_cont + 1.96*sd_cont
ci_ctp_upper

# Method 2
ctp_pool
diff_obs <- ctp_exp - ctp_cont
diff_obs
se_pool <- sqrt(ctp_pool*(1-ctp_pool)*(1/sum(control$Pageviews)+1/sum(experiment$Pageviews)))
se_pool
ci_diff_lower <- 0 - 1.96*se_pool
ci_diff_lower
ci_diff_upper <- 0 + 1.96*se_pool
ci_diff_upper


#-------- Result Analysis

#---- gross conversion
experiment_clean <- na.omit(experiment)
control_clean  <- na.omit(control)
gc_exp <- sum(experiment_clean$Enrollments,na.rm = TRUE)/sum(experiment_clean$Clicks)
gc_cont <- sum(control_clean$Enrollments,na.rm = TRUE)/sum(control_clean$Clicks)
gc_pool <- (sum(experiment_clean$Enrollments,na.rm = TRUE)+sum(control_clean$Enrollments,na.rm = TRUE))/(sum(experiment_clean$Clicks)+sum(control_clean$Clicks))
gc_pool

diff_gc_obs <- gc_exp - gc_cont
diff_gc_obs

se_gc_pool <- sqrt(gc_pool*(1-gc_pool)*(1/sum(control_clean$Clicks)+1/sum(experiment_clean$Clicks)))
se_gc_pool
ci_gc_lower <- diff_gc_obs - 1.96*se_gc_pool
ci_gc_lower
ci_gc_upper <- diff_gc_obs + 1.96*se_gc_pool
ci_gc_upper

#---- net conversion
nc_exp <- sum(experiment_clean$Payments)/sum(experiment_clean$Clicks)
nc_cont <- sum(control_clean$Payments)/sum(control_clean$Clicks)
nc_pool <- (sum(experiment_clean$Payments)+sum(control_clean$Payments))/(sum(experiment_clean$Clicks)+sum(control_clean$Clicks))
nc_pool

diff_nc_obs <- nc_exp - nc_cont
diff_nc_obs

se_nc_pool <- sqrt(nc_pool*(1-nc_pool)*(1/sum(control_clean$Clicks)+1/sum(experiment_clean$Clicks)))
se_nc_pool
ci_nc_lower <- diff_nc_obs - 1.96*se_nc_pool
ci_nc_lower
ci_nc_upper <- diff_nc_obs + 1.96*se_nc_pool
ci_nc_upper

#----- sign test
experiment_clean$gc <- experiment_clean$Enrollments/experiment_clean$Clicks
experiment_clean$nc <- experiment_clean$Payments/experiment_clean$Clicks
control_clean$gc <- control_clean$Enrollments/control_clean$Clicks
control_clean$nc <- control_clean$Payments/control_clean$Clicks

#---  Gross Conversion
n_row <- nrow(experiment_clean)
gc_sign_count <- sum(control_clean$gc > experiment_clean$gc)
gc_sign_count
#---  net Conversion
nc_sign_count <- sum(control_clean$nc > experiment_clean$nc)
nc_sign_count


