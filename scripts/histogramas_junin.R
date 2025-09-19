#############################################
# Histogramas Vicuñas Junín
# Last version: 09-17-2025
# By: Fiorella Eduardo-Palomino / fioedupa@gmail.com
#############################################

source("scripts/utils.R")  # importar funciones

# Leer datos Junín
datos <- limpiar_datos("data/histogramaju.csv")
str(datos)

# Calcular número de barras por año
numbarras <- pretty(range(datos$X1997), n = nclass.Sturges(datos$X1997))
numbarras1 <- pretty(range(datos$X2000), n = nclass.Sturges(datos$X2000))
numbarras2 <- pretty(range(datos$X2012), n = nclass.Sturges(datos$X2012))
numbarras3 <- pretty(range(datos$X2024), n = nclass.Sturges(datos$X2024))

# Histograma 1997
gr1<-ggplot(data.frame(datos$X1997), aes(x = datos$X1997)) + 
  geom_histogram(breaks = numbarras, color = "black", fill = "darkgreen")+
  labs( x = "1997", y = "Frecuencia")+
  geom_point(aes(x = 6292, y = 0), color = "#486349", size = 3) +
  geom_point(aes(x = 4031, y = 0), color = "#689D72", size = 3) +
  geom_point(aes(x = 22069, y = 0), color = "#689D72", size = 3) +
  geom_point(aes(x = 19808, y = 0), color = "#689D72", size = 3) +
  # Nombres de los puntos
  annotate("text", x = 6292, y = 0,  label = str_wrap("valor real sin filtro", width = 8),
           color = "#486349", angle=0, hjust=1, vjust = -0.5, size = 3) +
  annotate("text", x = 4031, y = 0, label = str_wrap("valor con filtro temporal y sin cercos permanentes", 
                                                     width = 8),
           color = "#689D72", angle=0, hjust = -0.1, vjust = -0.2, size = 3) +
  annotate("text", x = 22069, y = 0,  label = str_wrap("Ovinos + valor real sin filtro",width = 8),color = "#486349", angle=0, hjust=1, vjust = -0.5, size = 3) +
  annotate("text", x = 19808, y = 0,  label = str_wrap("Ovinos + valor con filtro",width = 8),color = "#486349", angle=0, hjust=1, vjust = -0.5, size = 3) +
  coord_cartesian(xlim = c(3000, 22500)) +
  scale_x_break(c(4100, 6200),scales =0.5) +  
  scale_x_break(c(6300, 12080),scales = 0.5)+
  scale_x_break(c(12500, 19000),scales = 0.5)+
  theme_classic() 
gr1

ggsave("results/HistogramaV+O1997.png", gr1, width = 8, height = 5, dpi = 300)

# Histograma 2000
gr2<-ggplot(data.frame(datos$X2000), aes(x = datos$X2000)) + 
  geom_histogram(breaks = numbarras1, color = "black", fill = "darkred")+
  labs( x = "2000", y = "Frecuencia")+
  geom_point(aes(x = 6868, y = 1), color = "#B71C1C", size = 3) +
  geom_point(aes(x = 5380, y = 1), color = "#AE123A", size = 3) +
  geom_point(aes(x = 20973, y = 1), color = "#AE123A", size = 3) +
  geom_point(aes(x = 19485, y = 1), color = "#AE123A", size = 3) +
  # Nombres de los puntos
  annotate("text", x = 6868, y = 1,  label = str_wrap("valor real sin filtro", 
                                                      width = 8),
           color = "#B71C1C", angle=0, hjust=1, vjust = -0.5, size = 3) +
  annotate("text", x = 5380, y = 0, label = str_wrap("valor con filtro temporal y sin cercos permanentes", 
                                                     width = 8),
           color = "#AE123A", angle=0, hjust = -0.1, vjust = -0.2, size = 3) +
  annotate("text", x = 20973, y = 0, label = str_wrap("Ovinos + valor con filtro temporal y sin cercos permanentes", 
                                                      width = 8),
           color = "#AE123A", angle=0, hjust = -0.1, vjust = -0.2, size = 2) +
  annotate("text", x = 19485, y = 0, label = str_wrap("Ovinos + valor real sin filtro", 
                                                      width = 8),
           color = "#AE123A", angle=0, hjust = -0.1, vjust = -0.2, size = 2) +
  coord_cartesian(xlim = c(5300, 30400)) +
  scale_x_break(c(5390,6800),scales =0.3) +  
  scale_x_break(c(6900, 19000),scales =0.3)+
  scale_x_break(c(22000, 28500),scales = 1)+
  theme_classic() 
gr2
ggsave(filename = "results/HistogramaV+O2000.png", plot = gr2, width = 8, height = 5, units = "in", dpi = 300)

options(scipen = 999)

# Histograma 2012
gr3<-ggplot(data.frame(datos$X2012), aes(x = datos$X2012)) + 
  geom_histogram(breaks = numbarras2, color = "black", fill = "darkblue")+
  labs( x = "2012", y = "Frecuencia")+
  geom_point(aes(x = 9064, y = 0), color = "blue", size = 3) +
  geom_point(aes(x = 141304, y = 0), color = "blue", size = 3) +
  annotate("text", x = 9064, y = 0,  label = str_wrap("valor real sin filtro",width = 8),
           color = "blue", angle=0, hjust=-0.1, vjust = -0.5, size = 3) +
  annotate("text", x = 141304, y = 0,  label = str_wrap("Ovinos + valor real sin filtro",width = 8),
           color = "blue", angle=0, hjust=1.5, vjust = -0.5, size = 3) +
  coord_cartesian(xlim = c(9000,150000)) +
  scale_x_break(c(9100, 70000),scales =1) + 
  scale_x_break(c(105000, 135000),scales =1) +
  theme_classic() 
gr3
ggsave(filename = "results/HistogramaV+O2012.png", plot = gr3, width = 10, height = 5, units = "in", dpi = 300)

# Histograma 2024
gr4<-ggplot(data.frame(datos$X2024), aes(x = datos$X2024)) + 
  geom_histogram(breaks = numbarras3, color = "black", fill = "darkorange")+
  labs( x = "2024", y = "Frecuencia")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  theme_classic() 
gr4
ggsave(filename = "results/Histograma2024.png", plot = gr4, width = 10, height = 5, units = "in", dpi = 300)


# Combinar histogramas simples
gr_simple <- (
  ggplot(data.frame(datos$X1997), aes(x = datos$X1997)) + geom_histogram(breaks = numbarras, fill = "darkgreen") +
    ggplot(data.frame(datos$X2000), aes(x = datos$X2000)) + geom_histogram(breaks = numbarras1, fill = "darkred") +
    ggplot(data.frame(datos$X2012), aes(x = datos$X2012)) + geom_histogram(breaks = numbarras2, fill = "darkblue") +
    ggplot(data.frame(datos$X2024), aes(x = datos$X2024)) + geom_histogram(breaks = numbarras3, fill = "darkorange")
)
gr_simple

# Calcular modas
modas <- sapply(datos, moda)
modas_df <- data.frame(Anio = names(modas), Valor_mas_frecuente = modas)
write.csv(modas_df, "results/modas_junin.csv", row.names = FALSE)