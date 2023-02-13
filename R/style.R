library(bs4Dash)
library(fresh)

mytheme <- fresh::create_theme(
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#51CBA4",
    navbar_light_hover_color = "#46B390"
  ),
  bs4dash_layout(
    main_bg = "#FFFF"
  ),
  bs4dash_sidebar_light(
    bg = "#272c30",
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30",
    submenu_color = "#FFF",
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#46B390",
    info = "#91DADA",
    success = "#51CBA4",
    danger = "#EB5050",
    warning = "#F9DB6D",
    light = "#272c30"
  ),
  bs4dash_color(
    gray_900 = "#353238"
  )
)
