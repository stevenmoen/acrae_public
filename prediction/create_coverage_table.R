#### Coverage table function ####

#' A way to create a nice coverage table
#'
#' @param input_list - the list from the prediction function
#' @param year_data - the year of the data.
#' @param year_parameters - the year of the parameters.
#' @param alpha - the alpha level. 1 - alpha is the coverage value
#' @param sig_figs - significant figures. Default is 4.
#'
#' @return - a table
#' @export
#'
#' @examples - create_coverage_table(data_2011_new_forecast_w_parameters_2011_new_90, year_data = 2011, year_parameters = 2011, alpha = 0.1)
create_coverage_table <- function(input_list, year_data,  year_parameters, alpha, sig_figs = 3, out_path = "Coverage_Tables/") {
  # Create a matrix
  sto_mat <- input_list$coverage_table_by_location$coverage_fraction %>% matrix(nrow = 12, ncol = 10, byrow = T) %>% signif(digits = sig_figs)
  # Compute a difference matrix from the target coverage
  coverage_level = 1-alpha
  coverage_mat_abs_dev = abs(sto_mat - coverage_level)
  # Compute the minimum and maximum deviations for use below
  min_dev = min(coverage_mat_abs_dev)
  max_dev = max(coverage_mat_abs_dev)
  # Create a header using weird code found here. See under "Important Caveat"
  # https://stackoverflow.com/questions/64414334/kableextra-how-does-the-add-header-above-function-work
  header_1 = 10 
  names(header_1) = paste(paste0("Expected Coverage: ", coverage_level*100, "%"), paste0("Actual Coverage: ", signif(mean(sto_mat)*100, sig_figs), "%"), sep = " / ")
  # Create a dataframe
  sto_df <- sto_mat %>%  as.data.frame()
  # Append a column with 1,12
  sto_df <- cbind(as.factor(seq(1,12,1)), sto_df)
  # Format nicely
  colnames(sto_df) <- c("Row / Column", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  # Convert columns to a percent. Can't figure out how to vectorize this.
  sto_df$`1` = formattable::percent(sto_df$`1`, digits = sig_figs - 2) 
  sto_df$`2` = formattable::percent(sto_df$`2`, digits = sig_figs - 2) 
  sto_df$`3` = formattable::percent(sto_df$`3`, digits = sig_figs - 2) 
  sto_df$`4` = formattable::percent(sto_df$`4`, digits = sig_figs - 2) 
  sto_df$`5` = formattable::percent(sto_df$`5`, digits = sig_figs - 2) 
  sto_df$`6` = formattable::percent(sto_df$`6`, digits = sig_figs - 2) 
  sto_df$`7` = formattable::percent(sto_df$`7`, digits = sig_figs - 2) 
  sto_df$`8` = formattable::percent(sto_df$`8`, digits = sig_figs - 2) 
  sto_df$`9` = formattable::percent(sto_df$`9`, digits = sig_figs - 2) 
  sto_df$`10` = formattable::percent(sto_df$`10`, digits = sig_figs - 2) 
  out_kable <- sto_df %>% kable(caption = paste0("Empirical Coverage for ", year_data, " One-Step Forecast Using ", year_parameters, " Parameters"), align = "c") %>%
    kable_styling(full_width = F) %>% 
    # This is in reverse order
    add_header_above(c(" ", header_1), italic = T) %>%
    # Bold the first column
    column_spec(1, bold = T) %>%
    # column_spec(2:3, color = "white", background = spec_color(coverage_mat[,1:2])) %>%
    column_spec(2, color = "white", background = spec_color(coverage_mat_abs_dev[,1], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(3, color = "white", background = spec_color(coverage_mat_abs_dev[,2], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(4, color = "white", background = spec_color(coverage_mat_abs_dev[,3], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(5, color = "white", background = spec_color(coverage_mat_abs_dev[,4], begin = 0.15, end = 0.8,  option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(6, color = "white", background = spec_color(coverage_mat_abs_dev[,5], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(7, color = "white", background = spec_color(coverage_mat_abs_dev[,6], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(8, color = "white", background = spec_color(coverage_mat_abs_dev[,7], begin = 0.15, end = 0.8,  option = "C",scale_from = c(min_dev, max_dev))) %>%
    column_spec(9, color = "white", background = spec_color(coverage_mat_abs_dev[,8], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(10, color = "white", background = spec_color(coverage_mat_abs_dev[,9], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    column_spec(11, color = "white", background = spec_color(coverage_mat_abs_dev[,10], begin = 0.15, end = 0.8, option = "C", scale_from = c(min_dev, max_dev))) %>%
    footnote(symbol = c("Excludes first time point."))
  out_kable
}