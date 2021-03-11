library(tidyverse)
library(here)
library(jkmisc)
library(glue)
library(tidygraph)
library(ggraph)
library(magick)
library(ggimage)
library(fs)
library(tibble)

women <- readr::read_csv(here("Jupyter Notebooks", "Mujeres", "mujeres_cat.csv"))

png(tf <- here("Jupyter Notebooks", "Mujeres", "images", "mask.png"), 1000, 1000)
par(mar = rep(0,4), yaxs="i", xaxs="i")
plot(0, type = "n", ylim = c(0,1), xlim=c(0,1), axes=F, xlab=NA, ylab=NA)
plotrix::draw.circle(.5,0.5,.5, col="black")
dev.off()


# Image Processing
mask <- here("Jupyter Notebooks", "Mujeres", "images", "mask.png") %>% 
  image_read() %>% 
  image_scale("150")

category_color <- tibble(category = c("Negocios", "Gobierno", "Entretenimiento", "Activismo, Ciencia y Deportes"),
                         color = c("#5ac2de", "#ee741c", "#d04592", "#34aa4d")) 

women %>% 
  mutate(img_m = map(img, image_read) %>% 
           map(~image_scale(.x, "150")), 
         circle = map(img_m, ~image_composite(mask, .x, "plus", gravity = "Center") %>% 
                        image_trim())) %>%
  select(Nombres, circle) %>% 
  pwalk(~image_write(image = .y, path = here("Jupyter Notebooks", "Mujeres", "images", glue("{.x}.jpg"))))


here("Jupyter Notebooks", "Mujeres", "images") %>% 
  dir(full.names = TRUE) %>% 
  nth(3) %>% 
  image_read() 

women_nodes <- women %>% 
  mutate(circle = here("Jupyter Notebooks", "Mujeres", "images", glue("{Imagen}"))) %>% 
  left_join(category_color) 

## Dendrogram

nodes <- tibble(node = c('root', unique(women$category), unique(women$Nombres))) %>% 
  filter(node != "All", node != "Unsung hero") %>% 
  mutate(level = case_when(node == "root" ~ 1,
                           node %in% category_color$category ~ 2,
                           TRUE ~ 3))

lvl_one_edges <- women %>% 
  filter(category != "All") %>% 
  distinct(category) %>% 
  mutate(from = "root") %>% 
  left_join(category_color) %>% 
  rename(to = category)


lvl_two_edges <- women %>% 
  filter(category != "All") %>% 
  left_join(category_color) %>% 
  select(to = Nombres,
         from = category, 
         color)

edges <- bind_rows(lvl_one_edges, lvl_two_edges)

graph  <- tbl_graph(nodes, edges) 

portraits <- create_layout(graph,  layout = 'dendrogram', circular = TRUE) %>% 
  left_join(women_nodes, by = c("node" = "Nombres")) %>% 
  left_join(category_color, by = c("node" = "category")) %>% 
  mutate(color = coalesce(color.x, color.y)) %>% 
  select(-color.x, -color.y)

ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(aes(colour = color),  width = 1) +
  geom_node_text(data = filter(portraits, level == 2), aes(label = toupper(node), color = color, x = 0.75*x, y = 0.75 * y, hjust = c(1.1, 1.2, 0.8, -0.1), vjust = c(0, 0, -3, 0)), family = "Oswald", size = 5) +
  geom_node_text(data = filter(portraits, level == 3), aes(label = node, color = color, x = x*1.05, y = y*1.05, angle = -((-node_angle(x, y)+90)%%180)+90,  hjust = ifelse(between(node_angle(x,y), 90, 270), 1, 0)),  family = "Poppins", size = 3) +
  geom_label(aes(x = 0, y = 0.215, label = "Mujeres Poderosas"), family = "Oswald Bold", size = 8, fill = "white", label.size = 0) +
  geom_label(aes(x = 0, y = -0.205, label = "2020"), family = "Oswald Bold", size = 9, fill = 'white', label.size = 0) +
  geom_image(data = portraits, aes(x, y, image = circle), size = 0.025, by = "width") +
  #geom_image(data = filter(women_nodes, category == "All"), aes(x = 0, y = 0, image = circle), size = 0.125) +
  scale_edge_color_identity() +
  scale_color_identity() +
  theme_jk(grid = FALSE,
           markdown = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL,
       y = NULL,
       caption = "**Datos de**: Forbes | **Graphic**: @homargonzalez") +
  coord_equal(clip = "off") +
  ggsave(here("Jupyter Notebooks", "Mujeres", "mujerespoderosas.png"), width = 15, height = 15)