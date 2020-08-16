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
shape <- st_read(dsn = "~/Desktop/ggplot2_geom_bar/shape/", layer = "h27ka27117")

#住民基本台帳csv読込
data1 <- read_csv("./jyuki_asahi_202003.csv")

#男女別が「計」のデータだけ抽出
data2 <- data1 %>% filter(data1$男女別=="計")

#shapeファイルとcsvを結合
data <- left_join(shape, data2, by=c("MOJI"="町丁目名"))

#カラム名取得
column = colnames(data)
kuname="旭区"

#小中学校緯度経度読み込み
school1 <- read_csv("./小学校マスター.csv")
school2 <- read_csv("./中学校マスター.csv")

#生野区の小中学校のみ抽出
school1 <- school1 %>% filter(school1$地区名==kuname)
school2 <- school2 %>% filter(school2$地区名==kuname)

#学校名クレンジング 施設名から"大阪市立"を削除
school1$学校名 <- gsub("大阪市立","", school1$施設名)
school2$学校名 <- gsub("大阪市立","", school2$施設名)

#食品営業許可読み込み
eat <- read_csv("./飲食店喫茶店マスター.csv", locale=locale(encoding="CP932"))

#該当区のデータのみ抽出
eat <- eat %>% filter(eat$区=="旭")

#######################################################
#pngファイル版
{
  quartz(type="png", file=sprintf(paste(kuname, "_住基202003_総数_小中学校_飲食.png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")

  #色はK-meansで自動計算
  col_km <- data[[column[40]]] %>% classIntervals(., 10, style="kmeans") %>% findColours(.,pal=brewer.pal(10,"YlGn"))
  plot(st_geometry(shape[7]), col=col_km, main=paste(kuname, "　人口　総数 (住民基本台帳 2020年3月末現在)", sep=""))

  #飲食店営業許可申請
  points(eat$経度, eat$緯度, pch=16, col="red", cex=0.5)

  #小学校
  points(school1$X, school1$Y, ps=24, pch=15, col="yellow")
  text(school1$X, school1$Y-0.0007, labels=school1$学校名, cex=0.5)

  #中学校
  points(school2$X, school2$Y, ps=24, pch=16, col="blue")
  text(school2$X, school2$Y-0.0007, labels=school2$学校名, cex=0.5)

  #町丁目と人口はカブりを防ぐため最後に描画
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.0005, labels=shape$MOJI, cex=0.5)
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0003, labels=data[[column[40]]], cex=0.6)

  #凡例
  legend("bottomleft", legend=c("食品営業許可施設","(令和元年12月27日現在)","小学校","中学校"), pch=c(20,3,15,16), col=c("red","white","yellow","blue"), cex=0.8, bg="white")

  dev.off()
}



##################################################################
# オプション
#文字列含む行を抽出
#eat2 <- subset(eat, grepl("九条", eat$営業所所在地))

#色を固定
#col_km <- data[[column[40]]] %>% classIntervals(., 10, style="fixed", fixedBreaks=c(250,500,750,1000,1250,1500,1750,2000,2500,2750,max(.))) %>% findColours(.,pal=brewer.pal(10,"YlGn"))

#生野区　shapeファイル読んだときに、bboxの限界値表示される
#lefttop:34.6545261 135.5244732
#rightbottom:34.633056 135.563221
#xlim=c(135.5244732,135.563221), ylim=c(34.633056,34.672125)
