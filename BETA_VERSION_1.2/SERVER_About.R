output$rhot_table_1 <- renderRHandsontable({
  
  den_info <- matrix(c("Excessively well drained", "3", "6", "8",
                       "Well drained", "6", "10", "13",
                       "Moderately well drained", "9", "13", "17.5", 
                       "Somewhat poorly drained", "13", "17.5", "25", 
                       "Poorly drained", "20","30","40"), 
                     nrow = 4, ncol = 5, 
                     dimnames = list(c("Soil organic matter content", "<2%", "2-5%", ">5")))
  rhandsontable(den_info, 
                rowHeaderWidth = 200, 
                readOnly = TRUE)
})

output$rhot_table_2 <- renderRHandsontable({
  
  fert_info <- matrix(c("Application Method", "Surface Broadcast", "Surface Dribble", "Incorporated","Surface Broadcast","Incorporated","Surface Broadcast","Incorporated","Injected","Surface Broadcast","Surface Dribble","Incorporated","Surface Broadcast","Surface Dribble","Incorporated","Any Method",
                       "Percent Lost", "15", "10", "5","25","10","12.5","7.5","1.5","15","10","1","7.5","5","1","1"),
                     nrow = 16, ncol = 2, 
                     dimnames = list(c("Fertilizer N Source", "Urea or UAN / Soil pH > 7", "", "","Ammonium Sulfate / Soil pH > 7","","Ammonium Nitrate / Soil pH > 7","","Anhydrous Ammonia / Soil pH >7","Urea / Soil pH <7","","","UAN / Soil pH <7","","","All other N sources / Soil pH <7")))
  rhandsontable(fert_info, 
                rowHeaderWidth = 225, 
                readOnly = TRUE)
})