#ライブラリ
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ
setwd("~/Desktop/ggplot2_geom_bar/")

#シェープファイル読込
#eStatから取得 .dbf .prj .shxも合わせて作業ディレクトリ下部のshapeフォルダに入れておくべし。
shape <- st_read(dsn = "~/Desktop/ggplot2_geom_bar/shape/", layer = "h27ka27123")

#住民基本台帳csv読込
data1 <- read_csv("./jyuki_yodogawa_202003.csv")

#男女別が「計」のデータだけ抽出
data2 <- data1 %>% filter(data1$男女別=="計")

#shapeファイルとcsvを結合
data <- left_join(shape, data2, by=c("MOJI"="町丁目名"))

#カラム名取得
column = colnames(data)
kuname="淀川区"

#######################################################
#pngファイル版
{
  quartz(type="png", file=sprintf("淀川区_住基202003_総数.png"), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  col_km <- data[[column[40]]] %>% classIntervals(., 10, style="fixed", fixedBreaks=c(250,500,750,1000,1250,1500,1750,2000,2500,2750,max(.))) %>% findColours(.,pal=brewer.pal(10,"RdYlGn"))
  plot(st_geometry(shape[7]), col=col_km, main=paste(kuname, "　人口　総数 (住民基本台帳 2020年3月末現在)", sep=""))
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.0005, labels=shape$MOJI, cex=0.5)
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0003, labels=data[[column[40]]], cex=0.6)
  dev.off()
}
