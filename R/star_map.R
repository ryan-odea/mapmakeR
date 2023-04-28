#' Create a location based map of the stars
#'
#' @param location The name of the place you want to map via OSM query
#' @param date String format date (%m/%d/%Y) of the time you would like the starmap
#' @param location_caption String placed for the location on the output plot
#' @param color Named list of colors for the star map
#'
#' @importFrom osmdata getbb
#' @importFrom dplyr mutate filter
#' @importFrom lubridate yday
#' @importFrom grid grid.polygon polygonGrob gpar circleGrob
#'
#' @export

star_map <- function(location, date, location_caption, color){
  long = (yday(as.Date(date, format = "%m/%d/%Y"))/365) * 360

  date_string = as.character(as.Date(date, format = "%m/%d/%Y"))
  y = sub("^([^\\-]*)\\-.*", "\\1", date_string)
  m = sub("^\\d{4}-(\\d{2}).*", "\\1", date_string)
  d = sub("^\\d{4}-\\d{2}-(\\d{2})", "\\1", date_string)

  date_string = paste(month.name[as.numeric(m)],
                      paste0(ifelse(as.numeric(d) %in% as.numeric(paste0(0, 1:9)),
                                    sub("0", "", d), d), ","),
                      y)

  coords = getbb(location)
  lat = (coords[2,1]+coords[2,2])/2

  proj = paste0("+proj=laea +x_0=0 +y_0=0 +lon_0=", long, " +lat_0=", lat)

  hemisphere = st_sfc(st_point(c(0, lat)), crs = proj) %>%
    st_buffer(dist = 1e7) %>%
    st_transform(crs = proj)

  stars = st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json", stringsAsFactors = FALSE) %>%
    st_transform(crs = proj) %>%
    st_intersection(hemisphere) %>%
    mutate(geometry = geometry * matrix(c(-1, 0, 0, 1), 2, 2))
  st_crs(stars) = proj

  constellations = st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/constellations.lines.json",
                           stringsAsFactors = FALSE) %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_cast("MULTILINESTRING") %>%
    st_transform(crs = proj) %>%
    st_intersection(hemisphere) %>%
    filter(!is.na(st_is_valid(.))) %>%
    mutate(geometry = geometry * matrix(c(-1, 0, 0, 1), 2, 2))
  st_crs(constellations) = proj

  mask = polygonGrob(x = c(1, 1, 0, 0, 1, 1,
                           0.5 + 0.46 * cos(seq(0, 2 *pi, len = 100))),
                     y =  c(0.5, 0, 0, 1, 1, 0.5,
                            0.5 + 0.46 * sin(seq(0, 2*pi, len = 100))),
                     gp = gpar(fill = '#191d29', col = '#191d29'))

  ggplot() +
    geom_sf(data = stars, aes(size = -exp(mag), alpha = -exp(mag)),
            color = "white")+
    geom_sf(data = constellations, color = "white",
            size = 0.5) +
    annotation_custom(circleGrob(r = 0.46,
                                 gp = gpar(col = "white", lwd = 10, fill = NA))) +
    scale_y_continuous(breaks = seq(0, 90, 15)) +
    scale_size_continuous(range = c(0, 2)) +
    annotation_custom(mask) +
    labs(caption = paste0(location_caption,
                          "\n", date_string,
                          "\n", round((coords[2,1] + coords[2,2])/2, 2), "° N",
                          ", ", ifelse(coords[1,1] < 0,
                                       paste0(-1*round((coords[1,1] + coords[1,2])/2, 2), "° W"),
                                       paste0(round((coords[1,1] + coords[1,2])/2, 2), "° E"))
    )) +
    theme_void() +
    theme(legend.position = "none",
          panel.grid.major = element_line(color = "grey35", size = 1),
          panel.grid.minor = element_line(color = "grey20", size = 1),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "#191d29", color = "#191d29"),
          plot.margin = margin(20, 20, 20, 20),
          plot.caption = element_text(color = 'white', hjust = 0.5,
                                      face = 2, size = 20,
                                      margin = margin(150, 20, 20, 20)))

}
