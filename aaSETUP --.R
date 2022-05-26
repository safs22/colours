

# For writing a df to clipboard (i.e. for pasting in excel)
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# For writing a column from a df with only unique entries to clipboard (i.e. for pasting in excel)
write.col.excel <- function(df, col, row.names=FALSE,col.names=TRUE,...){
  x <- as.data.frame(unique(purrr::pluck(df, col)))
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# Instead of copying and pasting from a vector printed to console, use this instead to copy
tidytext <- function(integers = FALSE){ 
  a <- .Last.value
  if(integers != FALSE){
    a <- a[integers]
  }
  # Process
  b <- paste(a,collapse="@")
  b <- capture.output(cat(gsub("@", "\",\"",b)))
  c <- print(paste0('"',capture.output(print(b, quote = F)),'"'), quote = F)
  c <- substr(c, 6, nchar(c))
  print(paste0('"',c), quote = F)
}

dummyDate <- function(df, date_col, dummy_year_to_use = 1904){
  x <- df
  int <- which(colnames(x)==date_col)
  vec <- purrr::pluck(x, int)
  if(class(vec) != "Date"){
    print("ERROR: specified date_col is not of 'Date' type)")
    top()
  }
  # Create dummy_col
  x <- rename(x, "qwerty" = date_col)
  x <- x %>% 
    mutate(day_x = day(qwerty),
           month_x = month(qwerty),
           year_x = dummy_year_to_use,
           qwerty_dummy = paste0(day_x,"-",month_x,"-",year_x)) %>% 
    select(-day_x, -month_x, -year_x)
  x$qwerty_dummy <- dmy(x$qwerty_dummy)
  colname <- paste0(date_col,"_dummy")
  x <- rename(x, date = "qwerty")
  x <- rename(x, date_dummy = "qwerty_dummy")
  msg <- paste0("date_dummy column created from date column with a dummy year of ",dummy_year_to_use)
  print(msg)
  return(x)
}



classify <- function(data, group1 = NULL, group2 = NULL, group3 = NULL, group4 = NULL, group5 = NULL, 
                     group6 = NULL, group7 = NULL, group8 = NULL, group9 = NULL, group10 = NULL,
                     group11 = NULL, group12 = NULL, group13 = NULL, group14 = NULL, group15 = NULL,
                     group16 = NULL, group17 = NULL, group18 = NULL, group19 = NULL, group20 = NULL,
                     group21 = NULL, group22 = NULL, group23 = NULL, group24 = NULL, group25 = NULL, 
                     old_column = "old_column", new_column = "new_column"){
  if(is.null(group1)==TRUE){
    output <- pluck(data, old_column)
    output <- unique(output)
  } else {
    
  }
  return(output)
}

# classify: vlookup-like function to generate a new (grouping) column based on an old column
# Takes a list containing grouping to perfrom this function on a data.frame/tibble
# If group_list is left blank, the function will return all unique entries in the old_column
classify <- function(data,group_list = NULL, old_column, new_column = "new_column"){
  if(is.null(group_list)==TRUE){
    output <- pluck(data, old_column)
    output <- unique(output)
  } else {
  if(is.list(group_list)==FALSE){
    print("Error: group_list arg should be of class 'list'")
    stop()
  }
  ngroups <- length(group_list)
  groups <- names(group_list)
  d1 <- crossing("new_col" = groups[1],"old_col" = group_list[[1]])
  if(ngroups > 1){d2 <- crossing("new_col" = groups[2],"old_col" = group_list[[2]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 2){d2 <- crossing("new_col" = groups[3],"old_col" = group_list[[3]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 3){d2 <- crossing("new_col" = groups[4],"old_col" = group_list[[4]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 4){d2 <- crossing("new_col" = groups[5],"old_col" = group_list[[5]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 5){d2 <- crossing("new_col" = groups[6],"old_col" = group_list[[6]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 6){d2 <- crossing("new_col" = groups[7],"old_col" = group_list[[7]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 7){d2 <- crossing("new_col" = groups[8],"old_col" = group_list[[8]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 8){d2 <- crossing("new_col" = groups[9],"old_col" = group_list[[9]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 9){d2 <- crossing("new_col" = groups[10],"old_col" = group_list[[10]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 10){d2 <- crossing("new_col" = groups[11],"old_col" = group_list[[11]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 11){d2 <- crossing("new_col" = groups[12],"old_col" = group_list[[12]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 12){d2 <- crossing("new_col" = groups[13],"old_col" = group_list[[13]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 13){d2 <- crossing("new_col" = groups[14],"old_col" = group_list[[14]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 14){d2 <- crossing("new_col" = groups[15],"old_col" = group_list[[15]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 15){d2 <- crossing("new_col" = groups[16],"old_col" = group_list[[16]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 16){d2 <- crossing("new_col" = groups[17],"old_col" = group_list[[17]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 17){d2 <- crossing("new_col" = groups[18],"old_col" = group_list[[18]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 18){d2 <- crossing("new_col" = groups[19],"old_col" = group_list[[19]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 19){d2 <- crossing("new_col" = groups[20],"old_col" = group_list[[20]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 20){d2 <- crossing("new_col" = groups[21],"old_col" = group_list[[21]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 21){d2 <- crossing("new_col" = groups[22],"old_col" = group_list[[22]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 22){d2 <- crossing("new_col" = groups[23],"old_col" = group_list[[23]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 23){d2 <- crossing("new_col" = groups[24],"old_col" = group_list[[24]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 24){d2 <- crossing("new_col" = groups[25],"old_col" = group_list[[25]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 25){d2 <- crossing("new_col" = groups[26],"old_col" = group_list[[26]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 26){d2 <- crossing("new_col" = groups[27],"old_col" = group_list[[27]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 27){d2 <- crossing("new_col" = groups[28],"old_col" = group_list[[28]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 28){d2 <- crossing("new_col" = groups[29],"old_col" = group_list[[29]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 29){d2 <- crossing("new_col" = groups[30],"old_col" = group_list[[30]]); d1 <- bind_rows(d1,d2)}
  if(ngroups > 30){print("Error: 30 groupings maximum allowed with this function"); stop()}
  # Add the new column
  d1 <- d1 %>% 
    rename(!!sym(new_column) := "new_col", !!sym(old_column) := "old_col")
  output <- left_join(data, d1, by = old_column)
  # Check for missed classifications
  check <- pluck(output, new_column)
  if(any(is.na(check))==TRUE){
    msg <- paste0(length(which(is.na(check)))," unclassified variables labelled as NA")
    print(msg)
  }
  }
  return(output)
}
