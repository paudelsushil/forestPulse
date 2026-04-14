create.timeclass <- function(data, 
                              date_column,
                              start_date,
                              end_date,
                              by,
                              types) {
    # First create start dates
    start_dates <- seq(from = start_date, 
                      to = end_date - by + 1, 
                      by = by)
    
    # Then create end dates
    end_dates <- start_dates + (by - 1)
    
    # Create case arguments
    case_args <- lapply(seq_along(start_dates), function(i) {
        quo(
            .data[[date_column]] >= !!start_dates[i] & 
            .data[[date_column]] <= !!end_dates[i] ~
            !!paste0(start_dates[i], "-", end_dates[i])
        )
    })
    
    # Create the new column
    new_col_name <- paste0(types, "By", by, "yrs")
    
    data %>%
        mutate(
            !!new_col_name := case_when(
                !!!case_args,
                TRUE ~ NA_character_
            )
        )
}
