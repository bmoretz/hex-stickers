# theme
# https://colors.muz.li/palette/272643/ffffff/e3f6f5/bae8e8/2c698d
library(hexSticker)
library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto", "roboto")

## Automatically use showtext to render text for future devices
showtext_auto()

img_logger <- file.path(here::here("resources"), "sp-logger.png")

# light: #e3f6f5
# med: #bae8e8
# dark: #272643

bdr_color <- "#e3f6f5"
# bg: #2c698d

bg_color <- "#2c698d"

sticker(img_logger,
        package="dyn.log",
        p_size=20, s_x=1, s_y=.75, s_width=.6,
        p_family = "roboto",
        h_fill=bg_color,
        h_color=bdr_color,
        filename=file.path(here::here("dyn-log"), "dyn-log-light.png"))
