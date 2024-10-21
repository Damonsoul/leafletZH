#' Title: Adds a choropleth map layer for provinces with additional customization options.
#'
#' @param map The leaflet map object to add the layer to.
#' @param data A data frame containing the data to be visualized.
#' @param adcode 中国行政区划代码
#' @param provinceName A string specifying the column name in the data frame that corresponds to the province names.
#' @param layerId An optional string to identify the layer.
#' @param group An optional string for grouping data.
#' @param valueProperty The property in the geojson data that corresponds to the value to be mapped.
#' @param labelProperty The property in the geojson data that will be used for labels.
#' @param labelOptions Options for labels, defaults to leaflet's labelOptions.
#' @param popupProps A named vector of properties to display in the popup.
#' @param popupOptions Options for popups, defaults to leaflet's popupOptions.
#' @param scale A vector of colors to use for the scale of the choropleth map.
#' @param steps The number of steps for the color scale.
#' @param mode The mode for the color scale, can be "q" for quantile, "e" for equal interval, etc.
#' @param channelMode The color channel mode, can be "rgb", "lab", "hsl", or "lch".
#' @param padding Optional padding for the choropleth layer.
#' @param correctLightness A logical value to correct lightness for color scales.
#' @param bezierInterpolate Whether to use bezier interpolation for the lines.
#' @param colors An optional vector of colors to override the default color scale.
#' @param stroke Whether to draw the stroke along the paths.
#' @param color The color for the paths, defaults to white.
#' @param weight The weight for the paths.
#' @param opacity The opacity for the paths.
#' @param fillOpacity The fill opacity for the paths.
#' @param dashArray An optional array to create dashed lines.
#' @param smoothFactor A factor to smooth the factor for the paths.
#' @param noClip Whether to disable clipping of the paths.
#' @param pathOptions Additional options for the paths, defaults to leaflet's pathOptions.
#' @param highlightOptions Options for highlighting features, defaults to leaflet's highlightOptions.
#' @param legendOptions Options for the legend.
#' @param ... Additional arguments passed to other functions.
#'
#' @return The modified leaflet map object with the added layer.
#' @export
#'
#' @examples
#'
#'
#' # 使用省份名称绘图，使用字段的前两个字进行匹配
#' library(leaflet)
#' library(leaflet.extras)
#' library(leafletZH)
#' library(dplyr)
#' data <- data.frame(name = c("河北省", "山西", "陕西"), value = runif(3))
#' leaflet() %>%
#'   leafletZH::addTilesAmap() %>%
#'   addProvinceShape(
#'     data = data, provinceName = "name", valueProperty = "value",
#'     popupProps = c("value")
#'   ) %>%
#'   setView(lng = 110, lat = 40, zoom = 4)
#'
#'
#' # 使用adcode进行匹配,adcode可以在leafletZH::china_province中获取
#' library(leaflet)
#' library(leaflet.extras)
#' library(leafletZH)
#' library(dplyr)
#' data <- data.frame(adcode = seq(110000, 150000, 10000), value = runif(5))
#' leaflet() %>%
#'   leafletZH::addTilesAmap() %>%
#'   addProvinceShape(
#'     data = data, adcode = "adcode", valueProperty = "value",
#'     popupProps = c("value")
#'   ) %>%
#'   setView(lng = 110, lat = 40, zoom = 4)
#'
addProvinceShape <- function(map, data, adcode = NULL, provinceName = NULL, layerId = NULL, group = NULL,
                             valueProperty = NULL,
                             labelProperty = NULL, labelOptions = leaflet::labelOptions(),
                             popupProps = NULL, popupOptions = leaflet::popupOptions(),
                             scale = c("white", "red"), steps = 5, mode = "q", channelMode = c(
                               "rgb",
                               "lab", "hsl", "lch"
                             ), padding = NULL, correctLightness = FALSE,
                             bezierInterpolate = FALSE, colors = NULL, stroke = TRUE,
                             color = "#ffffff", weight = 1, opacity = 0.5, fillOpacity = 0.7,
                             dashArray = NULL, smoothFactor = 1, noClip = FALSE,
                             pathOptions = leaflet::pathOptions(),
                             highlightOptions = leaflet::highlightOptions(
                               weight = 2, color = "#000000",
                               fillOpacity = 1, opacity = 1,
                               bringToFront = TRUE, sendToBack = TRUE
                             ), legendOptions = NULL, ...) {
  requireNamespace(sf)
  china_province$省份 <- china_province$name

  if (!is.null(adcode)) {
    data["join"] <- data[adcode]
    china_province$join <- china_province$adcode

    province_sf <- merge(china_province, data, by.x = "join", by.y = "join", suffixes = c("", ".y"))
  } else {
    if (is.null(adcode) && !is.null(provinceName)) {
      data["join"] <- data.frame(lapply(data[provinceName], substr, 0, 2))
      china_province$join <- substr(china_province$name, 0, 2)
      province_sf <- merge(china_province, data, by.x = "join", by.y = "join", suffixes = c("", ".y"))
    } else {
      message("adcode 或者 provinceName 至少需要输入一个")
    }
  }





  province_geojson <- geojsonsf::sf_geojson(province_sf)

  popupProperty <- NULL

  table.attrs <- list(class = "table table-striped table-bordered")
  if (!is.null(popupProps) && length(popupProps) >= 1) {
    popupProperty <- leaflet::JS(sprintf(
      "function(feature){\n         return '<table%s><caption>详细信息</caption><tbody style=\"font-size:x-small\">' +\n      ( $.isEmptyObject(feature.properties) ? '' :\n             L.Util.template(\"%s\",feature.properties)\n           )  + \"</tbody></table>\";\n       }",
      if (!is.null(table.attrs)) {
        paste(sapply(names(table.attrs), function(attr) {
          sprintf(
            " %s=\"%s\"",
            attr, table.attrs[[attr]]
          )
        }), collapse = " ")
      } else {
        ""
      },
      paste("<tr><td><b>省份</b></td><td>{name}</td></tr>",
        paste(
          stringr::str_replace(
            popupProps, "(.*)",
            "<tr><td><b>\\1</b></td><td>{\\1}</td></tr>"
          ),
          collapse = ""
        ),
        collapse = ""
      )
    ))
  }

  leaflet.extras::addGeoJSONChoropleth(map,
    province_geojson,
    valueProperty = valueProperty,
    labelProperty = "name",
    popupProperty = popupProperty, scale = scale,
    color = color, weight = weight, fillOpacity = fillOpacity,
    highlightOptions = highlightOptions
  )
}
