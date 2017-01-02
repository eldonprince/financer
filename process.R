process <- function() {
  
  library(readr)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  
  # Create master list
  dlist <- vector("list", 29)
  names(dlist) <- c("dat_original", "budget_original", "lookup", "exclude", "dates", 
                    "dat", "budget", 
                    "cal_dat", "cal_plot",
                    "overall_dat", "overall_plot", "overall_value", 
                    "food_dat", "food_value", "food_cat", "food_plot", 
                    "shop_dat", "shop_value", "shop_cat", "shop_plot",
                    "activity_dat", "activity_value", "activity_cat", "activity_plot",
                    "other_dat", "other_value", "other_cat", "other_plot",
                    "detail_dat")
  
  #Color theme
  bad_col <- "#FFEC8B"
  good_col <- "#A6D785"
  neutral_col <- "#E8E8E8"
  
  # Read in transactions and budget
  dlist[["dat_original"]] <- read_csv("transactions.csv")
  dlist[["budget_original"]] <- read_excel("budget.xlsx", 1)
  dlist[["lookup"]] <- try(read_excel("budget.xlsx", 2), silent = TRUE)
  dlist[["exclude"]] <- try(read_excel("budget.xlsx", 3), silent = TRUE)
  dlist[["dates"]] <- read_excel("budget.xlsx", 4)
  
  # Data transformations and filtering
  dat <- dlist[["dat_original"]]
  dlist[["budget"]] <- dlist[["budget_original"]] %>% filter(Focus == "Y")
  dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
  dat$Subcategory <- dat$Category
  dat$Category <- NULL
  dat$Labels <- NULL
  dat$Notes <- NULL
  dat <- dat %>% left_join(dlist[["lookup"]], by = "Subcategory")
  dat$Category <- ifelse(!is.na(dat$Category), dat$Category, 
                         ifelse(dat$Subcategory %in% dlist[["budget"]][["Category"]], 
                                dat$Subcategory, "Other"))
  dat <- dat %>% left_join(dlist[["budget"]], by = "Category")
  dat <- dat %>% filter(!Category %in% dlist[["exlude"]][["Category"]])
  dat <- dat %>% filter(Date >= as.Date(dlist[["dates"]][["Month_Begin"]]) & 
                          Date < as.Date(dlist[["dates"]][["Week_Begin"]]))
  dat <- dat %>% filter(Focus == "Y")
  dlist[["dat"]] <- dat
  
  # Plot calendar
  # http://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-r
  num_days_month <- function(date) {
    return(as.numeric(format(as.Date(paste0(format(date, format = "%Y"),
                                            formatC(ifelse(format(date, format = "%m") == "12",
                                                           0, as.numeric(format(date,format = "%m"))) + 1, 
                                                    width = 2, format = "d", flag = "0"), "01"), "%Y%m%d") - 1,
                             format = "%d")))
  }
  num_days <- num_days_month(dlist[["dates"]][["Month_Begin"]])
  done_days <- as.numeric(dlist$dates$Week_Begin - dlist$dates$Month_Begin)
  dlist[["cal_dat"]] <- data.frame(total = c(1:num_days), done = c(rep(1, done_days), rep(0, num_days - done_days)))
  dlist[["cal_plot"]] <- ggplot(dlist[["cal_dat"]], aes(total, 1, fill = factor(done))) + 
    geom_tile(color = "#BEBEBE") + 
    scale_fill_manual(values = c("white", neutral_col)) +
    theme_void() +
    theme(legend.position = "none")
  
  # Prepare data for plotting overall status
  overall_prep <- function(dat, budget) {
    amount <- unname(unlist(dat %>% summarise(Amount = sum(Amount))))
    status <- ifelse(amount > sum(budget$Budget), "Bad", "Good")
    prep <- tibble(Group = "Overall", 
                   Amount = amount, 
                   Budget = sum(budget$Budget), 
                   Status = status)
    return(prep)
  }
  dlist[["overall_dat"]] <- overall_prep(dlist[["dat"]], dlist[["budget"]])
  dlist[["overall_value"]] <- dlist[["overall_dat"]][["Budget"]] - dlist[["overall_dat"]][["Amount"]]
  
  #Function to set color based on value
  plot_col <- function(value) {ifelse(value < 0, bad_col, good_col)}
  
  # Plot overall status
  over_plot <- function(dat, value) {
    col_plot <- plot_col(value)
    if (dat$Amount == 0) {
      dat$Amount <- dat$Budget
      col_plot <- neutral_col}
    ggplot(dat) +
      geom_bar(aes(Group, Amount, fill = Status), stat = "identity") +
      geom_errorbar(aes(Group, ymin = Budget, ymax = Budget)) +
      coord_flip() +
      scale_fill_manual(values = col_plot) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,0,0,0),"cm")) 
  }
  dlist[["overall_plot"]] <- over_plot(dlist[["overall_dat"]], dlist[["overall_value"]])
  
  # Prepare data for groups
  group_prep <- function(dat, budget, group) {
    dat <- dat %>% filter(Group == group)
    budget <- budget %>% filter(Group == group)
    amount <- unname(unlist(dat %>% summarise(Amount = sum(Amount))))
    status <- ifelse(amount > sum(budget$Budget), "Bad", "Good")
    prep <- data.frame(Group = group, 
                       Amount = amount, 
                       Budget = sum(budget$Budget), 
                       Status = status)
    return(prep)
  }
  dlist[["food_dat"]] <- group_prep(dlist[["dat"]], dlist[["budget"]], "Food")
  dlist[["food_value"]] <- dlist[["food_dat"]][["Budget"]] - dlist[["food_dat"]][["Amount"]]
  dlist[["shop_dat"]] <- group_prep(dlist[["dat"]], dlist[["budget"]], "Shopping")
  dlist[["shop_value"]] <- dlist[["shop_dat"]][["Budget"]] - dlist[["shop_dat"]][["Amount"]]
  dlist[["activity_dat"]] <- group_prep(dlist[["dat"]], dlist[["budget"]], "Activity")
  dlist[["activity_value"]] <- dlist[["activity_dat"]][["Budget"]] - dlist[["activity_dat"]][["Amount"]]
  dlist[["other_dat"]] <- group_prep(dlist[["dat"]], dlist[["budget"]], "Other")
  dlist[["other_value"]] <- dlist[["other_dat"]][["Budget"]] - dlist[["other_dat"]][["Amount"]]
  
  # Prepare data for categories
  category_prep <- function(dat, budget, group) {
    dat <- dat %>% filter(Group == group)
    budget <- budget %>% filter(Group == group)
    plot_order <- budget %>% arrange(Budget)
    prep <- dat %>% group_by(Category) %>% summarise(Amount = sum(Amount))
    prep <- prep %>% left_join(budget, by = "Category") 
    prep$Status <- ifelse(prep$Amount > prep$Budget, "Bad", "Good")
    prep_fill <- tibble(Category = budget$Category, 
                        Amount = budget$Budget,
                        Group = budget$Group,
                        Budget = budget$Budget,
                        Rollover = budget$Rollover,
                        Focus = budget$Focus,
                        Status = "Nothing")
    prep <- bind_rows(prep, prep_fill)
    prep <- prep %>% distinct(Category, .keep_all = TRUE)
    prep$Category <- factor(prep$Category, levels = plot_order$Category)
    return(prep)
  }
  dlist[["food_cat"]] <- category_prep(dlist[["dat"]], dlist[["budget"]], "Food")
  dlist[["shop_cat"]] <- category_prep(dlist[["dat"]], dlist[["budget"]], "Shopping")
  dlist[["activity_cat"]] <- category_prep(dlist[["dat"]], dlist[["budget"]], "Activity")
  dlist[["other_cat"]] <- category_prep(dlist[["dat"]], dlist[["budget"]], "Other")
  
  # Plot categories within groups
  cat_plot <- function(dat_cat) {
    col_status <- c("Bad", "Good", "Nothing") %in% unique(dat_cat$Status)
    col_status <- paste0(col_status, collapse = "")
    col_identifier <- tibble(status = c("TRUETRUETRUE", "TRUEFALSETRUE", 
                                        "FALSETRUETRUE", "TRUETRUEFALSE",
                                        "FALSEFALSETRUE", "FALSETRUEFALSE",
                                        "TRUEFALSEFALSE"),
                             color = list(c(bad_col, good_col, neutral_col), 
                                          c(bad_col, neutral_col),
                                          c(good_col, neutral_col),
                                          c(bad_col, good_col),
                                          c(neutral_col),
                                          c(good_col),
                                          c(bad_col)))
    col_plot <- col_identifier %>% filter(status == col_status) %>% select(color)
    col_plot <- unname(unlist(col_plot))
  
    ggplot(dat_cat) +
      geom_bar(aes(Category, Amount, fill = Status), stat = "identity") +
      geom_errorbar(aes(Category, ymin = Budget, ymax = Budget)) +
      geom_text(aes(Category, Budget, label = Budget), hjust = 1.1, size = 3) +
      coord_flip() +
      scale_fill_manual(values = col_plot) +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.title = element_blank(), 
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,0,0,0),"cm"))
  }
  dlist[["food_plot"]] <- cat_plot(dlist[["food_cat"]])
  dlist[["shop_plot"]] <- cat_plot(dlist[["shop_cat"]])
  dlist[["activity_plot"]] <- cat_plot(dlist[["activity_cat"]])
  dlist[["other_plot"]] <- cat_plot(dlist[["other_cat"]])
  
  dlist[["detail_dat"]] <- dat %>% arrange(desc(Amount)) %>% 
    select(Group, Category, Amount, Description)
  
  return(dlist)
}
dlist <- process()