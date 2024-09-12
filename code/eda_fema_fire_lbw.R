#------------------------------
# CSDE D4 Hack Week 
# September 9-13
# FEMA EDA 

#------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse, arrow, MetBrewer, patchwork)
pal <- met.brewer("Derain")

#------------------------------
# load data
my_root <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/d4-hack-week-uw/"
path <- "data/01_raw/"
df <- read_parquet(paste0(path, "wf_fema_dta.parquet")) %>% filter(damagedStateAbbreviation == "CA")

#------------------------------
# missingness eval
keep_vars <-
  c("disasterNumber", "damagedStateAbbreviation", "damagedZipCode", "declarationDate", "applicantAge", "householdComposition",
    "occupantsUnderTwo", "occupants2to5", "occupants6to18", "occupants19to64", "occupants65andOver",
    "grossIncome", "ownRent", "primaryResidence", "residenceType", "homeOwnersInsurance", "registrationMethod",
    "ihpEligible", "ihpAmount", "haAmount", "haEligible", "onaEligible", "onaAmount", "utilitiesOut",
    "habitabilityRepairsRequired", "rpfvl", "ppfvl", "renterDamageLevel", "destroyed",
    "rentalAssistanceEligible", "rentalAssistanceAmount", "repairAssistanceEligible", "repairAmount",
    "replacementAssistanceEligible", "replacementAmount", "personalPropertyEligible", "personalPropertyAmount",
    "ihpMax", "haMax", "onaMax", "id"
  )
df <- df %>%
  select(all_of(keep_vars)) %>% 
  mutate(year = year(declarationDate),
        grossIncome = ifelse(grossIncome == "0", NA, grossIncome))

#------------------------------
# missingness eval
plot_vars <-
  c("applicantAge", "householdComposition", "occupantsUnderTwo", "occupants2to5", "occupants6to18",  "occupants19to64", "occupants65andOver", "grossIncome", "ownRent", "primaryResidence", "homeOwnersInsurance", "registrationMethod", "ihpEligible", "haEligible", "utilitiesOut", "habitabilityRepairsRequired", "renterDamageLevel", "destroyed", "rentalAssistanceEligible", "repairAssistanceEligible","replacementAssistanceEligible", "personalPropertyEligible"
  )


# bar plot for categorical faceted by yr 
# line plot for numeric faceted by yr
# num missing
# num unknown 

n_levels <- 15
original_palette <- met.brewer("Derain")
expanded_palette <- colorRampPalette(original_palette)(n_levels)

plots <- list()
for (i in 1:length(plot_vars)) {
  var <- plot_vars[i]
  print(var)
  var_class <- class(df[[var]])[1]  
  
  df_var <- df %>% select(year, !!sym(var)) %>%
      mutate(!!sym(var) := as.factor(!!sym(var)))
  num_missing <- sum(is.na(df_var[[var]])) 
  num_unknown = sum(df_var[[var]] == "Unknown", na.rm = TRUE)
  tot_missing = num_missing + num_unknown
  var_name <- str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", var))  
  
  # Create bar plot
  bar_plot <- df_var %>%
    ggplot(aes(x = factor(year), fill = !!sym(var))) +
    geom_bar(position = "stack") +
    theme_bw() +
    scale_fill_manual(values = original_palette) +
    labs(
      title = paste0(var_name, " (missing or unknown: ", tot_missing, ")"), 
      x = var_name,
      fill = var_name
    ) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  plots[[i]] <- bar_plot
}

fema_money_vars <- c("ihpAmount", "haAmount", "rpfvl", "year")
plot_df <- df %>%
  select(all_of(fema_money_vars)) %>% 
  group_by(year) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  mutate(percent_granted = (ihpAmount + haAmount) / rpfvl)

ihp_plot <- plot_df %>%
    ggplot(aes(x = year, y = ihpAmount)) +
    geom_col(fill = met.brewer("Derain")[1]) +
    theme_bw() +
    labs(title = "IHP amount ($)", x = "year", y = "ihp amount")
ha_plot <- plot_df %>%
    ggplot(aes(x = year, y = haAmount)) +
    geom_col(fill = met.brewer("Derain")[2]) +
    theme_bw() +
    labs(title = "HA amount ($)", x = "year", y = "ha amount")
rpfvl_plot <- plot_df %>%
    ggplot(aes(x = year, y = rpfvl)) +
    geom_col(fill = met.brewer("Derain")[3]) +
    theme_bw() +
    labs(title = "FEMA assessed real property damage amount", x = "year", y = "FEMA assessed damage ($)")
perc_plot <- plot_df %>%
    filter(year > 2003) %>% 
    ggplot(aes(x = year, y = percent_granted)) +
    geom_col(fill = met.brewer("Derain")[4]) +
    theme_bw() +
    labs(title = "Percent of FEMA assessed damage ($) \n that was funded", x = "year", y = "percent granted")

left_column <- ihp_plot / ha_plot / rpfvl_plot
fema_money_plots <- left_column | perc_plot + plot_layout(widths = c(20, 6))

# res type plot
res_type_plot <- df %>%
    ggplot(aes(x = factor(year), fill = residenceType)) +
    geom_bar(position="stack") + 
    theme_bw() +
    labs(
      title = paste0("Residence type (missing or unknown: ", tot_missing, ")"), 
      x = var_name,
      fill = var_name
    ) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

#------------------------------
# save bar plots and money plots
pdf("~/Desktop/fema_plots.pdf", width = 10, height = 10)
fema_money_plots
res_type_plot
for(i in 1:length(plots)) {
  print(plots[[i]])
}
dev.off()

