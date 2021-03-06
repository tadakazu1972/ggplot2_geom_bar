#ライブラリ
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ
setwd("~/Desktop/平野区_土地/")

#シェープファイル読込
#eStatから取得 .dbf .prj .shxも合わせて作業ディレクトリ下部のshapeフォルダに入れておくべし。
shape <- st_read(dsn = "~/Desktop/平野区_土地/", layer = "tochiriyo_平野区")

##############################################
#町名で勝山のみ抜き出し　部分一致なのでgreplを使う
#町レベル1
townname = "長原"
shape2 <- shape %>% filter(grepl(townname, shape$町名))

#土地用途コードごとに色設定
colors = c("gold","darkorange3","yellow","coral","coral2","aquamarine","aquamarine1","aquamarine4","aquamarine2","orange","maroon1","maroon2","lightskyblue","lightskyblue","lightskyblue","lightskyblue","lightskyblue","lightskyblue","lightskyblue","lightskyblue","lightskyblue","slateblue1","slateblue1","slateblue1","slateblue1","slateblue1","slateblue1","peachpuff","peachpuff","peachpuff","peachpuff","dodgerblue","deeppink","deeppink1","plum","pink","pink","pink","pink","dodgerblue","lightseagreen","lightseagreen","pink","slategray2","slategray2","slategray2","slategray2","slategray2","slategray2","slategray2","slategray2","slategray2","palegreen","orangered","turquoise1","turquoise3","springgreen3","wheat1","magenta","white","gray","green","tan1","blue","darkgray","red","gray40","cyan","gray70","magenta")

#pngファイル書き出し
{
  quartz(type="png", file=sprintf(paste(townname, ".png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  plot(shape2[11], col=colors, main=paste(townname, " (平成29年度　土地利用現況データ)", sep=""))

  dev.off()
}

###############################################
#町名で勝山のみ抜き出し　部分一致なのでgreplを使う
#町レベル2
townname = "長原東"
shape3 <- shape %>% filter(grepl(townname, shape$町名))

#pngファイル書き出し
{
  quartz(type="png", file=sprintf(paste(townname, ".png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  plot(shape3[11], col=colors, main=paste(townname, " (平成29年度　建物用途別現況データ)", sep=""))

  dev.off()
}

###############################################
#町名で勝山のみ抜き出し　部分一致なのでgreplを使う
#町レベル3
townname = "勝山南３丁目"
shape4 <- shape %>% filter(grepl(townname, shape$町名))

#pngファイル書き出し
{
  quartz(type="png", file=sprintf(paste(townname, ".png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  plot(shape4[13], main=paste(townname, " (平成29年度　建物用途別現況データ)", sep=""))

  dev.off()
}

###############################################
#町名で勝山のみ抜き出し　部分一致なのでgreplを使う
#町レベル3　複数　　長吉東部
townname = "長吉出戸７丁目|長吉出戸８丁目|長吉六反|長吉長原東|長吉川辺３丁目"
townname_group = "長吉東部"
shape4 <- shape %>% filter(grepl(townname, shape$町名))

#pngファイル書き出し
{
  quartz(type="png", file=sprintf(paste(townname_group, ".png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  plot(st_geometry(shape4[13]), main=paste(townname_group, " (平成29年度　建物用途別現況データ)", sep=""))

  dev.off()
}


###############################################
#町名で勝山のみ抜き出し　部分一致なのでgreplを使う
#町レベル3　複数　　長吉東部 + 飲食店
townname = "長吉出戸７丁目|長吉出戸８丁目|長吉六反|長吉長原東|長吉川辺３丁目"
townname_group = "長吉東部"
shape4 <- shape %>% filter(grepl(townname, shape$町名))
# 平面から緯度経度に変換
shape4 <- shape4 %>% st_transform(6668)

#食品営業許可読み込み
eat <- read_csv("./飲食店喫茶店マスター.csv", locale=locale(encoding="CP932"))
#該当のデータのみ抽出
eat2 <- eat %>% filter(grepl(townname, eat$営業所所在地))

#pngファイル書き出し
{
  quartz(type="png", file=sprintf(paste(townname_group, ".png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  plot(st_geometry(shape4[13]),  border="gray50", main=paste(townname_group, " (平成29年度　建物用途別現況データ)", sep=""))

  #飲食店営業許可申請
  points(eat2$経度, eat2$緯度, pch=16, col="red", cex=0.6)

  #凡例
  legend("bottomleft", legend=c("食品営業許可施設","(令和元年12月27日現在)"), pch=c(20,3), col=c("red","white"), cex=0.8, bg="white")

  dev.off()
}






#################################################
#　以下、住基データとマッチングさせるか今後研究用にとりあえず置いておく

#住民基本台帳csv読込
data1 <- read_csv("./jyuki_jyoto_202003.csv")

#男女別が「計」のデータだけ抽出
data2 <- data1 %>% filter(data1$男女別=="計")

#shapeファイルとcsvを結合
data <- left_join(shape, data2, by=c("MOJI"="町丁目名"))

#カラム名取得
column = colnames(data)
kuname="城東区"

#######################################################
#pngファイル版
{
  quartz(type="png", file=sprintf(paste(kuname, "_住基202003_総数.png", sep="")), dpi=144, bg="white")
	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  col_km <- data[[column[40]]] %>% classIntervals(., 10, style="fixed", fixedBreaks=c(250,500,750,1000,1250,1500,1750,2000,2500,2750,max(.))) %>% findColours(.,pal=brewer.pal(10,"RdYlGn"))
  plot(st_geometry(shape[7]), col=col_km, main=paste(kuname, "　人口　総数 (住民基本台帳 2020年3月末現在)", sep=""))
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.0005, labels=shape$MOJI, cex=0.5)
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0003, labels=data[[column[40]]], cex=0.6)
  dev.off()
}
