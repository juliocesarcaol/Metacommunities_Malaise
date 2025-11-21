##############################################
# Accumulation Curves with Hill Numbers (q = 0, 1, 2)
##############################################

# --- Load required libraries ---

library(iNEXT)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(cowplot)

# ============================================================
# 1) Convert abundance table to list
# ============================================================
abund_list <- split(tabla_abund[,-1], tabla_abund$locality)
abund_list <- lapply(abund_list, as.numeric)

# ============================================================
# 2) Run iNEXT q = 0,1,2
# ============================================================
out_q0 <- iNEXT(abund_list, q = 0, datatype = "abundance")
out_q1 <- iNEXT(abund_list, q = 1, datatype = "abundance")
out_q2 <- iNEXT(abund_list, q = 2, datatype = "abundance")

# ============================================================
# 3) Define function to remove legends & apply turbo scale
# ============================================================
colors_by_country <- c(
  "Guanacaste"     = "#597a2d",
  "Baru"           = "#c5dcc0",
  "Mashpi"         = "#597a2d",
  "Quetzales"      = "#dcc7a0",
  "Pululahua"      = "#dcc7a0",
  "BarroColorado"  = "#c5dcc0",
  "CerroBlanco"    = "#c5dcc0",
  "Paluguillo"     = "#dcc7a0",
  "Galapagos"      = "#d6eefe",
  "IslaCocos"      = "#d6eefe"
)

shapes_by_country <- c(
  "Guanacaste"     = 17,
  "Baru"           = 17,
  "Mashpi"         = 18,
  "Quetzales"      = 16,
  "Pululahua"      = 17,
  "BarroColorado"  = 16,
  "CerroBlanco"    = 18,
  "Paluguillo"     = 18,
  "Galapagos"      = 18,
  "IslaCocos"      = 17 
)

fix_plot <- function(p) {
  p +
    guides(color = "none", fill = "none", shape = "none") +
    scale_color_manual(values = colors_by_country) +
    scale_fill_manual(values = colors_by_country) +
    scale_shape_manual(values = shapes_by_country)
}



# ============================================================
# 4) Build all plots
# ============================================================
# q = 0
p0_size <- fix_plot( ggiNEXT(out_q0, type = 1) +
                       theme_minimal() +
                       labs(title="q = 0", subtitle="Sample-size R/E") )

p0_cov  <- fix_plot( ggiNEXT(out_q0, type = 3) +
                       theme_minimal() +
                       labs(title="q = 0", subtitle="Coverage R/E") )

# q = 1
p1_size <- fix_plot( ggiNEXT(out_q1, type = 1) +
                       theme_minimal() +
                       labs(title="q = 1", subtitle="Sample-size R/E") )

p1_cov  <- fix_plot( ggiNEXT(out_q1, type = 3) +
                       theme_minimal() +
                       labs(title="q = 1", subtitle="Coverage R/E") )

# q = 2
p2_size <- fix_plot( ggiNEXT(out_q2, type = 1) +
                       theme_minimal() +
                       labs(title="q = 2", subtitle="Sample-size R/E") )

p2_cov  <- fix_plot( ggiNEXT(out_q2, type = 3) +
                       theme_minimal() +
                       labs(title="q = 2", subtitle="Coverage R/E") )

# ============================================================
# 5) EXTRAER UNA SOLA LEYENDA
# ============================================================


# Orden deseado:
locality_order <- c(
  "IslaCocos", "Galapagos", 
  "BarroColorado", "Baru", "CerroBlanco",
  "Guanacaste", "Mashpi",
  "Quetzales", "Pululahua", "Paluguillo"
)

# 1. Crear dataframe dummy
legend_df <- data.frame(
  locality = locality_order,  # usar el orden deseado aquí también
  x = 1,
  y = 1
)

# 2. Plot para generar la leyenda
legend_plot <- ggplot(legend_df, aes(x, y, color = locality, shape = locality)) +
  geom_point(size = 5) +
  scale_color_manual(values = colors_by_country, breaks = locality_order) +
  scale_shape_manual(values = shapes_by_country, breaks = locality_order) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(color = "Localities", shape = "Localities")

# 3. Extraer la leyenda
legend_only <- cowplot::get_legend(legend_plot)

# 4. Guardar PDF
pdf("legend_localities.pdf", width = 4, height = 6)
grid::grid.draw(legend_only)
dev.off()


# ============================================================
# 6) PANEL 2×3 SIN LEYENDA
# ============================================================
panel_only <- 
  (p0_size | p1_size | p2_size) /
  (p0_cov  | p1_cov  | p2_cov)



ggsave(
  filename = "Accumulation_Curves_HillNumbers.pdf",
  plot = panel_only,
  device = cairo_pdf,   # evita problemas con transparencias
  width = 30,           # ajusta según necesites
  height = 15,
  dpi = 300
)

