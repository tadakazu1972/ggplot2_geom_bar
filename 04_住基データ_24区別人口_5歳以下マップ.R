#ライブラリ
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ
setwd("~/Desktop/ggplot2_geom_bar/")

#シェープファイル読込
#大阪市オープンデータポータルから取得 .dbf .prj .shxも合わせて作業ディレクトリ下部のshapeフォルダに入れておくべし。
shape <- st_read(dsn = "~/Desktop/ggplot2_geom_bar/shape/", layer = "24kuikishape")
#平面から緯度経度に変換
shape2 <- shape %>% st_transform(4612)

#住民基本台帳csv読込
data1 <- read_csv("./jyuki_24ku_202003.csv")

#男女別が「計」のデータだけ抽出
data2 <- data1 %>% filter(data1$男女別=="計")

#shapeファイルとcsvを結合　24区名で結合 府域全市町村のデータからなのでinner_joinを使うことに留意
data <- inner_join(shape2, data2, by=c("区名"="区名"))

#カラム名取得
column = colnames(data)
kuname="24区"

#######################################################
#pngファイル版
{
  quartz(type="png", file=sprintf(paste(kuname, "_住基202003_5歳以下.png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  col_km <- data[[column[6]]] %>% classIntervals(., 10, style="fixed", fixedBreaks=c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,max(.))) %>% findColours(.,pal=brewer.pal(10,"Greens"))
  plot(st_geometry(shape2[2]), col=col_km, main=paste(kuname, "　人口　5歳以下 (住民基本台帳 2020年3月末現在)", sep=""))
  text(st_coordinates(shape2 %>% st_centroid)[,1], st_coordinates(shape2 %>% st_centroid)[,2]+0.003, labels=shape2$区名, cex=0.7)
  text(st_coordinates(shape2 %>% st_centroid)[,1], st_coordinates(shape2 %>% st_centroid)[,2]-0.003, labels=data[[column[6]]], cex=0.8)
  dev.off()
}
