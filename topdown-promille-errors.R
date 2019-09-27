# ---- promillerechner-errors ----

tell_me_how_drunk(
  age = 34,
  sex = "M",
  height = 190,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("colaweizen" = 3, "leggor-proseggor" = NA)
)

tell_me_how_drunk(
  age = 14,
  sex = "F",
  height = 160,
  weight = 54,
  drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
  drinks = c("schnaps" = -4)
)

tell_me_how_drunk(
  age = 14,
  sex = "F",
  height = 160,
  weight = 54,
  drinking_time = c("14.00", "21.00"),
  drinks = c("schnaps" = 4)
)

tell_me_how_drunk(
  age = 14,
  sex = "F",
  height = 160,
  weight = 54,
  drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
  drinks = c(4, 2)
)
