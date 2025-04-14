
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(dplyr)
library(wordcloud)
library(tm)
library(stopwords)
library(RColorBrewer)

texto <- "La pesca sostenible es fundamental. La pesca artesanal tiene un rol clave en la conservación. La sostenibilidad es un objetivo importante."

# Convertir el texto a un tibble
df <- tibble(linea = 1, texto = texto)

palabras <- df %>%
  unnest_tokens(palabra, texto)

frecuencias <- palabras %>%
  count(palabra, sort = TRUE)

# Filtrar las palabras que aparecen más de una vez (si quieres)
frecuencias %>%
  filter(n > 1) %>%
  ggplot(aes(x = reorder(palabra, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Frecuencia de palabras",
       x = "Palabra",
       y = "Frecuencia")


data("stop_words") # en inglés
# Para español puedes usar un vector propio de palabras vacías en español o usar el paquete `stopwords`
#install.packages("stopwords")
library(stopwords)
stop_es <- stopwords("es")

# Eliminar palabras vacías
palabras_limpias <- palabras %>%
  filter(!palabra %in% stop_es)


# otro ejemplo

texto <- "La pesca sostenible es fundamental. La pesca artesanal tiene un rol clave en la conservación. La sostenibilidad es un objetivo importante."
# Convertir en un data frame
df <- tibble(linea = 1, texto = texto)

# Tokenizar (convertir a palabras)
palabras <- df %>%
  unnest_tokens(palabra, texto)

# Eliminar palabras vacías en español
palabras_limpias <- palabras %>%
  filter(!palabra %in% stopwords("es"))

# Contar frecuencia
frecuencias <- palabras_limpias %>%
  count(palabra, sort = TRUE)

set.seed(123) # Para que la nube sea reproducible
wordcloud(words = frecuencias$palabra,
          freq = frecuencias$n,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))





