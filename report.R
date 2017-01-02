report <- function(dlist) {
 
  library(ReporteRs)
  
  doc <- pptx(title = "Finance", template = "template.pptx")
  doc <- addSlide(doc, slide.layout = "Dashboard")
  
  # Overall text and plots
  val_text <- ifelse(dlist$overall_value < 0, " over", " left")
  overall_val <- pot(paste0("$", round(abs(dlist$overall_value), 2), val_text), 
                     format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, overall_val, offx = 0.83, offy = 0.17, width = 1.5, height = 0.4, 
                      par.properties = parProperties(text.align = "right"))
  
  doc <- addPlot(doc, fun = function() print(dlist$overall_plot), 
                 vector.graphic = TRUE,
                 offx = 2.1, offy = 0.24, width = 1.75, height = 0.35)
  
  doc <- addPlot(doc, fun = function() print(dlist$cal_plot), 
                 vector.graphic = TRUE,
                 offx = 6, offy = 0.25, width = 3, height = 0.2)
  
  # Group text and plots
  val_text <- ifelse(dlist$food_value < 0, " over", " left")
  food_val <- pot(paste0("$", round(abs(dlist$food_value), 2), val_text), 
                  format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, food_val, offx = 0.5, offy = 1.6, width = 1.5, height = 0.35, 
                      par.properties = parProperties(text.align = "center"))
  
  val_text <- ifelse(dlist$shop_value < 0, " over", " left")
  shop_val <- pot(paste0("$", round(abs(dlist$shop_value), 2), val_text), 
                  format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, shop_val, offx = 3, offy = 1.6, width = 1.5, height = 0.35, 
                      par.properties = parProperties(text.align = "center"))
  
  val_text <- ifelse(dlist$activity_value < 0, " over", " left")
  activity_val <- pot(paste0("$", round(abs(dlist$activity_value), 2), val_text), 
                      format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, activity_val, offx = 5.5, offy = 1.6, width = 1.5, height = 0.35, 
                      par.properties = parProperties(text.align = "center"))
  
  val_text <- ifelse(dlist$other_value < 0, " over", " left")
  other_val <- pot(paste0("$", round(abs(dlist$other_value), 2), val_text), 
                   format = textProperties(font.size = 14, font.family = "Calibri"))
  doc <- addParagraph(doc, other_val, offx = 8, offy = 1.6, width = 1.5, height = 0.35, 
                      par.properties = parProperties(text.align = "center"))
  
  doc <- addPlot(doc, fun = function() print(dlist$food_plot), 
                 vector.graphic = TRUE,
                 offx = 0.25, offy = 2.25, width = 2, height = 0.75)

  doc <- addPlot(doc, fun = function() print(dlist$shop_plot), 
                 vector.graphic = TRUE,
                 offx = 2.75, offy = 2.25, width = 2, height = 0.75)
  
  doc <- addPlot(doc, fun = function() print(dlist$activity_plot), 
                 vector.graphic = TRUE,
                 offx = 5.25, offy = 2.25, width = 2, height = 0.75)
  
  doc <- addPlot(doc, fun = function() print(dlist$other_plot), 
                 vector.graphic = TRUE,
                 offx = 7.75, offy = 2.25, width = 2, height = 0.75)

  # Tables
  fsort <- dlist$food_cat %>% arrange(desc(Budget))
  fdata <- dlist$detail_dat %>% filter(Group == "Food") %>% top_n(10, Amount) %>% select(-Group) 
  fdata$Category <- factor(fdata$Category, levels = fsort$Category)
  fdata <- fdata %>% arrange(Category)
  if (length(fdata) != 3) {fdata <- tibble(Category = "None", Amount = 0, Description = "None")}
  ftable <- FlexTable(fdata,
                      header.cell.props = cellProperties(border.top.style = "none",
                                                         border.left.style = "none",
                                                         border.right.style = "none"),
                      body.cell.props = cellProperties(border.style = "none"),
                      header.text.props = textProperties(font.size = 8, font.family = "Calibri"),
                      body.text.props = textProperties(font.size = 8, font.family = "Calibri"))
  ftable <- setFlexTableWidths(ftable, c(0.8, 0.5, 0.8))
  doc <- addFlexTable(doc, ftable,
                      offx = 0.2, offy = 3.25, width = 2.1, height = 3)
  
  fsort <- dlist$shop_cat %>% arrange(desc(Budget))
  fdata <- dlist$detail_dat %>% filter(Group == "Shopping") %>% top_n(10, Amount) %>% select(-Group) 
  fdata$Category <- factor(fdata$Category, levels = fsort$Category)
  fdata <- fdata %>% arrange(Category)
  if (length(fdata) != 3) {fdata <- tibble(Category = "None", Amount = 0, Description = "None")}
  ftable <- FlexTable(fdata,
                      header.cell.props = cellProperties(border.top.style = "none",
                                                         border.left.style = "none",
                                                         border.right.style = "none"),
                      body.cell.props = cellProperties(border.style = "none"),
                      header.text.props = textProperties(font.size = 8, font.family = "Calibri"),
                      body.text.props = textProperties(font.size = 8, font.family = "Calibri"))
  ftable <- setFlexTableWidths(ftable, c(0.8, 0.5, 0.8))
  doc <- addFlexTable(doc, ftable,
                      offx = 2.7, offy = 3.25, width = 2.1, height = 3)
  
  fsort <- dlist$activity_cat %>% arrange(desc(Budget))
  fdata <- dlist$detail_dat %>% filter(Group == "Activity") %>% top_n(10, Amount) %>% select(-Group) 
  fdata$Category <- factor(fdata$Category, levels = fsort$Category)
  fdata <- fdata %>% arrange(Category)
  if (length(fdata) != 3) {fdata <- tibble(Category = "None", Amount = 0, Description = "None")}
  ftable <- FlexTable(fdata,
                      header.cell.props = cellProperties(border.top.style = "none",
                                                         border.left.style = "none",
                                                         border.right.style = "none"),
                      body.cell.props = cellProperties(border.style = "none"),
                      header.text.props = textProperties(font.size = 8, font.family = "Calibri"),
                      body.text.props = textProperties(font.size = 8, font.family = "Calibri"))
  ftable <- setFlexTableWidths(ftable, c(0.8, 0.5, 0.8))
  doc <- addFlexTable(doc, ftable,
                      offx = 5.2, offy = 3.25, width = 2.1, height = 3)
  
  fsort <- dlist$other_cat %>% arrange(desc(Budget))
  fdata <- dlist$detail_dat %>% filter(Group == "Other") %>% top_n(10, Amount) %>% select(-Group) 
  fdata$Category <- factor(fdata$Category, levels = fsort$Category)
  fdata <- fdata %>% arrange(Category)
  if (length(fdata) != 3) {fdata <- tibble(Category = "None", Amount = 0, Description = "None")}
  ftable <- FlexTable(fdata,
                      header.cell.props = cellProperties(border.top.style = "none",
                                                         border.left.style = "none",
                                                         border.right.style = "none"),
                      body.cell.props = cellProperties(border.style = "none"),
                      header.text.props = textProperties(font.size = 8, font.family = "Calibri"),
                      body.text.props = textProperties(font.size = 8, font.family = "Calibri"))
  ftable <- setFlexTableWidths(ftable, c(0.8, 0.5, 0.8))
  doc <- addFlexTable(doc, ftable,
                      offx = 7.7, offy = 3.25, width = 2.1, height = 3)
  
  writeDoc(doc, paste0("Finance_", Sys.Date(), ".pptx"))
}
report(dlist)