# inspired by https://www.mitchelloharawild.com/blog/hexwall/

library(magick)
library(purrr)
library(dplyr)

hex_dir <- file.path(here::here(), "images", "hex")
hex_files <- list.files(hex_dir)
hex_obj <- map(file.path(hex_dir, hex_files), ~{
  switch(tools::file_ext(.x),
         svg = image_read_svg(.x),
         pdf = image_read_pdf(.x),
         image_read(.x))
}) %>%
  map(image_transparent, "white") %>%
  map(image_trim) %>%
  set_names(hex_files)

# Desired sticker resolution in pixels
sticker_width <- 100

# Scale all stickers to the desired pixel width
stickers <- hex_obj %>%
  map(image_scale, sticker_width)

# Identify low resolution stickers
stickers %>%
  map_lgl(~ with(image_info(.x),
                 width < (sticker_width-1)/2 && format != "svg")
  )

# Identify incorrect shapes / proportions (tolerance of +-2 height)
stickers %>%
  map_lgl(~ with(image_info(.x),
                 height < (median(height)-2) | height > (median(height) + 2))
  )

# Extract correct sticker height (this could also be calculated directly from width)
sticker_height <- stickers %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median

# Coerce sticker dimensions
stickers <- stickers %>%
  map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))

stickers[["golem.png"]]


sticker_row_size <- 3
# Calculate row sizes
sticker_col_size <- ceiling(length(stickers)/(sticker_row_size-0.5))
row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(stickers) - sum(row_lens))

sticker_rows <- map2(row_lens, cumsum(row_lens),
                     ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
  map(~ stickers[.x] %>%
        invoke(c, .) %>%
        image_append)

# Add stickers to canvas
canvas <- image_blank(sticker_row_size*sticker_width, 
                      sticker_height + (sticker_col_size-1)*sticker_height/1.33526,
                      "white")
                      
final_image <- reduce2(sticker_rows, seq_along(sticker_rows), 
                       ~ image_composite(
                         ..1, ..2,
                         offset = paste0("+", ((..3-1)%%2)*sticker_width/2,
                                         "+", round((..3-1)*sticker_height/1.33526))
                       ),
                       .init = canvas)

image_write(final_image, file.path(here::here(), "images", "pilot2_packages.png"))
