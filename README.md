# README: Leaflet Map Layers for Chinese Administrative Regions

## Overview

This package provides functions to add choropleth map layers for Chinese cities and provinces to a Leaflet map object. These layers can be customized with various options such as color scales, labels, and popups.

## Installation



```         
devtools::install_github('Damonsoul/leafletZH')
```

## Usage

### Adding a City Layer

To add a choropleth map layer for cities, use the addcityShape function. You will need a data frame containing the data to be visualized, including the Chinese administrative division codes (adcode).

```         
library(leaflet) library(leaflet.extras) library(leafletZH) library(dplyr)  data <- data.frame(adcode = seq(110101, 110110, 1), value = runif(5)) leaflet() %>%   leafletZH::addTilesAmap() %>%   addcityShape(
  data = data,
  adcode = "adcode",
  valueProperty = "value",
  popupProps = c("value")
) %>%   setView(lng = 116, lat = 40, zoom = 8)
```

### Adding a Province Layer

To add a choropleth map layer for provinces, use the addProvinceShape function. Similar to the city layer, you will need a data frame containing the data to be visualized.

```         
library(leaflet) library(leaflet.extras) library(leafletZH) library(dplyr)  data <- data.frame(name = c("河北省", "山西", "陕西"), value = runif(3)) leaflet() %>%   leafletZH::addTilesAmap() %>%   addProvinceShape(
  data = data,
  provinceName = "name",
  valueProperty = "value",
  popupProps = c("value")
) %>%   setView(lng = 110, lat = 40, zoom = 4)
```

## Data Sources

The province and city data are sourced from [Aliyun DataV Atlas](http://datav.aliyun.com/tools/atlas/).

## License

This package is released under the MIT license. Please see the LICENSE file for more details.

## Contributing

Contributions are welcome! Please submit a pull request or open an issue on the GitHub repository.

## Authors

This package was created by [Damonsoul](https://github.com/Damonsoul).

# README：中国行政区划的Leaflet地图图层

## 概述

该包提供了将中国城市和省份的 choropleth 地图图层添加到 Leaflet 地图对象的函数。这些图层可以通过各种选项进行自定义，例如颜色比例尺、标签和弹出窗口。

## 安装

从GitHub进行安装

```         
devtools::install_github('Damonsoul/leafletZH')
```

## 使用方法

### 添加城市图层

使用 `addcityShape` 函数添加城市 形状 地图图层。需要一个包含要可视化数据的数据框，包括中国的行政区划代码（adcode）。

```         
library(leaflet) library(leaflet.extras) library(leafletZH) library(dplyr)  data <- data.frame(adcode = seq(110101, 110110, 1), value = runif(5)) leaflet() %>%   leafletZH::addTilesAmap() %>%   addcityShape(
  data = data,
  adcode = "adcode",
  valueProperty = "value",
  popupProps = c("value")
) %>%   setView(lng = 116, lat = 40, zoom = 8)
```

### 添加省份图层

使用 `addProvinceShape` 函数要添加省份 形状 地图图层与城市图层类似，您需要一个包含要可视化数据的数据框,需要包括中国的行政区划代码（adcode）或省份名称。
省份名称使用前两个字进行匹配。

```         
library(leaflet)
library(leaflet.extras)
library(leafletZH)
library(dplyr)
data <- data.frame(name = c("河北省", "山西", "陕西"), value = runif(3))
leaflet() %>%
  leafletZH::addTilesAmap() %>%   addProvinceShape(
    data = data,
    provinceName = "name",
    valueProperty = "value",
    popupProps = c("value")
  ) %>%   setView(lng = 110, lat = 40, zoom = 4)
```


## 数据来源

省份和城市数据来源于 [阿里云 DataV Atlas](http://datav.aliyun.com/tools/atlas/)。

## 许可证

该包在 MIT 许可证下发布。更多细节请参阅 LICENSE 文件。

## 贡献

欢迎贡献！请在 GitHub 仓库提交拉取请求或开一个问题。
