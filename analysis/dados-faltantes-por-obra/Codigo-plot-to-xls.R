library(openxlsx)
plot <- panorama.municipios %>% 
    ggplot(aes(x = reorder(municipio, porc.criterios.validos), 
               y = porc.criterios.validos, 
               label = paste0(format(porc.criterios.validos * 100, decimal.mark=",", digits=2, nsmall=2), "% de ", qtde.obras, " obras"),
               fill = qtde.obras)) +
    geom_bar(stat = "identity") +
    geom_text(position = position_stack(vjust = 0.5), colour = "white") +
    labs(x = "Municípios", y = "Porcentagem de critérios válidos", title = "Porcentagem de critérios válidos por município") +
    coord_flip() +
    scale_y_continuous(label = percent) +
    scale_fill_viridis(name = "Quantidade de obras")
wb <- createWorkbook()
addWorksheet(wb, "Sheet 1", gridLines = FALSE) 
print(plot)
insertPlot(wb, 1, width = 7, height = 30, fileType = "png", units = "in")
saveWorkbook(wb, "insertPlotExample.xlsx", overwrite = TRUE)