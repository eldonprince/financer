report <- function(dlist) {
 
  library(ReporteRs)
  
  doc <- pptx(title = "Finance", template = "template.pptx")
  doc <- addSlide(doc, slide.layout = "Dashboard2")
  
  # Overall text and plots
  month_text <- pot(toupper(months(as.POSIXlt(dlist$dates$Month_Begin), abbreviate = FALSE)), 
                     format = textProperties(font.size = 18, font.family = "Calibri"))
  doc <- addParagraph(doc, month_text, offx = 4.32, offy = 0.15, width = 1.5, height = 0.4, 
                      par.properties = parProperties(text.align = "center"))
  
  # Budget text
  val_text <- ifelse(dlist$overall_value < 0, " over", " left")
  overall_val <- pot(paste0("Budget: ", "$", round(abs(dlist$overall_value), 2), val_text), 
                     format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, overall_val, offx = 0.25, offy = 0.17, width = 2, height = 0.4, 
                      par.properties = parProperties(text.align = "right"))
  
  # Rollover text
  val_text <- ifelse(dlist$rollover_value < 0, " over", " left")
  rollover_val <- pot(paste0("Rollover: ", "$", round(abs(dlist$rollover_value), 2), val_text), 
                     format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, rollover_val, offx = 0.25, offy = 0.47, width = 2, height = 0.4, 
                      par.properties = parProperties(text.align = "right"))
  
  # Overall plot
  doc <- addPlot(doc, fun = function() print(dlist$overall_plot), 
                 vector.graphic = TRUE,
                 offx = 2.1, offy = 0.24, width = 1.75, height = 0.35)
  
  # Rollover plot
  doc <- addPlot(doc, fun = function() print(dlist$rollover_plot), 
                 vector.graphic = TRUE,
                 offx = 2.1, offy = 0.54, width = 1.75, height = 0.35)
  
  doc <- addPlot(doc, fun = function() print(dlist$cal_plot), 
                 vector.graphic = TRUE,
                 offx = 6, offy = 0.25, width = 3, height = 0.2)
  
  # Group text
  add_group_text <- function(group_dat, position, spent_left) {
    if (spent_left == "left") {
      val <- group_dat$Budget - group_dat$Amount
      val_text <- ifelse(val < 0, " over", " left")
    }
    if (spent_left == "spent") {
      val <- group_dat$Amount
      val_text <- " spent"
    }
    positions <- list(c(0.5, 1.57, 1.5, 0.35),
                      c(0.5, 1.9, 1.5, 0.35),
                      c(3, 1.57, 1.5, 0.35),
                      c(3, 1.9, 1.5, 0.35),
                      c(5.5, 1.57, 1.5, 0.35),
                      c(5.5, 1.9, 1.5, 0.35),
                      c(8, 1.57, 1.5, 0.35),
                      c(8, 1.9, 1.5, 0.35))
    pos <- positions[[position]]
    val_pot <- pot(paste0("$", round(abs(val), 2), val_text), 
                    format = textProperties(font.size = 14, font.family = "Calibri"))
    doc <- addParagraph(doc, val_pot, offx = pos[1], offy = pos[2], width = pos[3], height = pos[4], 
                        par.properties = parProperties(text.align = "center"))  
  }
  add_group_text(dlist$food_dat, 1, "spent")
  add_group_text(dlist$food_dat, 2, "left")
  add_group_text(dlist$shop_dat, 3, "spent")
  add_group_text(dlist$shop_dat, 4, "left")
  add_group_text(dlist$activity_dat, 5, "spent")
  add_group_text(dlist$activity_dat, 6, "left")
  add_group_text(dlist$other_dat, 7, "spent")
  add_group_text(dlist$other_dat, 8, "left")

  # Group plots
  add_group_plot <- function(group_plot, position) {
    positions <- list(c(0.25, 2.57, 2, 0.75),
                      c(2.75, 2.57, 2, 0.75),
                      c(5.25, 2.57, 2, 0.75),
                      c(7.75, 2.57, 2, 0.75))
    pos <- positions[[position]]
    doc <- addPlot(doc, fun = function() print(group_plot), 
                   vector.graphic = TRUE,
                   offx = pos[1], offy = pos[2], width = pos[3], height = pos[4])  
  }
  add_group_plot(dlist$food_plot, 1)
  add_group_plot(dlist$shop_plot, 2)
  add_group_plot(dlist$activity_plot, 3)
  add_group_plot(dlist$other_plot, 4)
   
  # Tables
  add_group_table <- function(dat, cat, group, position) {
    positions <- list(c(0.2, 3.4, 2.1, 3),
                      c(2.7, 3.4, 2.1, 3),
                      c(5.2, 3.4, 2.1, 3),
                      c(7.7, 3.4, 2.1, 3))
    pos <- positions[[position]]
    fsort <- cat %>% arrange(desc(Budget))
    fdata <- dat %>% filter(Group == group) %>% top_n(30, Amount) %>% select(-Group) 
    fdata$Category <- factor(fdata$Category, levels = fsort$Category)
    fdata <- fdata %>% arrange(Category)
    if (nrow(fdata) == 0) {fdata <- tibble(Category = "None", Amount = 0, Description = "None")}
    ftable <- FlexTable(fdata,
                        header.cell.props = cellProperties(border.top.style = "none",
                                                           border.left.style = "none",
                                                           border.right.style = "none"),
                        body.cell.props = cellProperties(border.style = "none"),
                        header.text.props = textProperties(font.size = 8, font.family = "Calibri"),
                        body.text.props = textProperties(font.size = 8, font.family = "Calibri"))
    ftable <- setFlexTableWidths(ftable, c(0.8, 0.5, 0.8))
    doc <- addFlexTable(doc, ftable,
                        offx = pos[1], offy = pos[2], width = pos[3], height = pos[4])
  }
  add_group_table(dlist$detail_dat, dlist$food_cat, "Food", 1)
  add_group_table(dlist$detail_dat, dlist$shop_cat, "Shopping", 2)
  add_group_table(dlist$detail_dat, dlist$activity_cat, "Activity", 3)
  add_group_table(dlist$detail_dat, dlist$other_cat, "Other", 4)
  
  writeDoc(doc, paste0("Finance_", Sys.Date(), ".pptx"))
}
report(dlist)