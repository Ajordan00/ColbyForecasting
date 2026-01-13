#Brickman Tutorial

DB = brickman_database() |>
  print()
brickman_variables("all")
db = DB |>
  dplyr::filter(interval == "static")
static_vars = read_brickman(db)
static_vars
plot(static_vars['depth'], axes = TRUE)
db = DB |>
  dplyr::filter(scenario == "RCP85", 
                year == 2075,
                interval == "mon")
x = read_brickman(db)
x
plot(x['Tbtm'], axes = FALSE)
buoys = gom_buoys()
buoys
plot(x['Tbtm'] |> dplyr::filter(month == "Jan"), axes = FALSE, reset = FALSE)
plot(sf::st_geometry(buoys), col = "orange", pch = 20, add = TRUE)
text(buoys, labels = buoys$id, adj = c(1,1), col = "blue", cex = 0.8)
long_values = extract_brickman(x, buoys)
long_values
wide_values = extract_brickman(x, buoys, form = "wide")
wide_values

#Assignment 1

m01 = buoys |>
  dplyr::filter(id == "M01")
m01_values = extract_brickman(x, m01)

sst_m01 = m01_values |>
  dplyr::filter(name == "SST")
str(sst_m01$value)

sst_m01$month = factor(
  sst_m01$month,
  levels = c("Jan","Feb","Mar","Apr","May","Jun",
             "Jul","Aug","Sep","Oct","Nov","Dec")
)

plot(
  sst_m01$month,
  sst_m01$value,
  type = "b",
  pch = 16,
  xlab = "Month",
  ylab = "Sea Surface Temperature (°C)",
  main = "Buoy M01 – SST (RCP4.5, 2055)"
)


