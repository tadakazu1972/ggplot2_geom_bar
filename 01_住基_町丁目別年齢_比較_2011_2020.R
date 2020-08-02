#ライブラリ
library(dplyr)
library(ggplot2)
library(gtable) #表を描画
library(gridExtra) #配置情報を含むデータに変換
library(grid)

#作業ディレクトリ
setwd("~/Desktop/ggplot2_geom_bar/")

#区名を設定
kuname = "浪速区"

#住民基本台帳csv読込　市HPから取得
data1 <- read.csv("./jyuki_naniwa_201103.csv") #2011
data2 <- read.csv("./jyuki_naniwa_202003.csv") #2020

#男女別が「計」のデータだけ抽出
total1 <- data1 %>% filter(data1$男女別=="計")
total2 <- data2 %>% filter(data2$男女別=="計")

#NAを0に置換
total1[is.na(total1)] <- 0
total2[is.na(total2)] <- 0

#町丁目名を取得
last <- length(total1$町丁目名)-1 #最後が総数のため-1にして読み込まないようにする
name <- total1[1:last,2]

######################################################
#10歳まとめで人数集計
#0-9歳|10-19歳|...と10歳ごとの人数計を格納する1次元配列を0初期値で準備　100歳以上と総計を入れるから12個
sum1 <- array(0, dim=c(1,12))
sum2 <- array(0, dim=c(1,12))

#描画するdata frameを作成
df <- data.frame(matrix(0, nrow=3, ncol=12))
colnames(df) <- c("0-9歳","10-19歳","20-29歳","30-39歳","40-49歳","50-59歳","60-69歳","70-79歳","80-89歳","90-99歳","100歳","計")
rownames(df) <- c("2011年3月末","2020年3月末","増減数")

#以下は未使用　data frameの列で整数と%の２つの型を混合できない
#増減割合は%表示するので整数のdata frameと別に配列を作成
#percentage <- array(0, dim=c(1,12))
#rownames(percentage) <- c("増減割合")

#######################################################
#町丁目ごとに、住基データをpngファイルに書き出し。あとでパワポに貼り付けて資料作成する
for(j in 1:last){
  quartz(type="png", file=sprintf(paste(kuname, "住基年齢構成201103_202003_%d%s.png", sep=""),j, name[j]), dpi=144, bg="white")

  p1 <- total1 %>% filter(total1$町丁目名==name[j])
  p2 <- total2 %>% filter(total2$町丁目名==name[j])

  #ggplot2の棒グラフで描くため、x軸をベースに転置し、0歳～100歳以上までのデータとする
  #その加工の過程でdata frameではなくなることに留意
  x <- 0:100
  y1 <- as.integer(t(p1)[8:108])
  y2 <- as.integer(t(p2)[8:108])

  #データフレームをつくる(x軸にラベル,y軸に人数)
  q1 <- data.frame(年齢=x, 人=y1)
  q2 <- data.frame(年齢=x, 人=y2)

  #文字化け解消
  theme_set(theme_bw(base_family="HiraKakuProN-W3"))

  #ggplot2 棒グラフ　描画
  g1 <- ggplot(NULL) #複数描くためにとりあえずNULL

  #タイトル
  g1 <- g1 + ggtitle(paste(kuname, "　年齢別人口(住民基本台帳)", sep="")) + theme(plot.title = element_text(hjust = 0.5))

  #町丁目　描画
  if (max(p2[8:108]>300)){
    g1 <- g1 + annotate("text", label=name[j], size=5, x=50, y=340, family="HiraKakuProN-W3")
  } else　if (max(p2[8:108]>200)){
    g1 <- g1 + annotate("text", label=name[j], size=6, x=50, y=190, family="HiraKakuProN-W3")
  } else if (max(p2[8:108]>100)){
    g1 <- g1 + annotate("text", label=name[j], size=6, x=50, y=150, family="HiraKakuProN-W3")
  } else {
    g1 <- g1 + annotate("text", label=name[j], size=6, x=50, y= 90, family="HiraKakuProN-W3")
  }

  #凡例　描画
  if (max(p2[8:108]>300)){
    g1 <- g1 + annotate("text", label="青：2011年3月末", size=2, x=80, y=320, family="HiraKakuProN-W3")
    g1 <- g1 + annotate("text", label="赤：2020年3月末", size=2, x=80, y=310, family="HiraKakuProN-W3")
  } else　if (max(p2[8:108]>200)){
    g1 <- g1 + annotate("text", label="青：2011年3月末", size=3, x=80, y=170, family="HiraKakuProN-W3")
    g1 <- g1 + annotate("text", label="赤：2020年3月末", size=3, x=80, y=165, family="HiraKakuProN-W3")
  } else if (max(p2[8:108]>100)){
    g1 <- g1 + annotate("text", label="青：2011年3月末", size=3, x=80, y=130, family="HiraKakuProN-W3")
    g1 <- g1 + annotate("text", label="赤：2020年3月末", size=3, x=80, y=125, family="HiraKakuProN-W3")
  } else {
    g1 <- g1 + annotate("text", label="青：2011年3月末", size=4, x=80, y= 75, family="HiraKakuProN-W3")
    g1 <- g1 + annotate("text", label="赤：2020年3月末", size=4, x=80, y= 70, family="HiraKakuProN-W3")
  }

  #2011年　棒グラフ
  g1 <- g1 + geom_bar(data=q1, aes(x=年齢, y=人), stat="identity", color=NA, fill="dodgerblue") + xlab("年齢") + ylab("人")

  #2020年　棒グラフ
  if (max(p2[8:108]>200)){
    g1 <- g1 + geom_bar(data=q2, aes(x=年齢, y=人), stat="identity", fill="red", alpha=0.4)
  } else {
    g1 <- g1 + geom_bar(data=q2, aes(x=年齢, y=人), stat="identity", color="darkred", fill="red", alpha=0.2)
  }

  #x軸10刻み設定 300を超える友渕町1丁目は20刻みにしないと重なる
  if (max(p2[8:108]>200)){
    g1 <- g1 + coord_fixed() + scale_x_continuous(breaks=seq(0,100,20))
  } else {
    g1 <- g1 + coord_fixed() + scale_x_continuous(breaks=seq(0,100,10))
  }


  #y軸を設定
  if (max(p2[8:108]>200)){
    g1 <- g1 + coord_fixed() + scale_y_continuous(breaks=seq(0,350,10))
  } else if (max(p2[8:108]>100)){
    g1 <- g1 + coord_fixed() + scale_y_continuous(breaks=seq(0,200,10))
  } else {
    g1 <- g1 + coord_fixed() + scale_y_continuous(breaks=seq(0,100,10))
  }

  #人数の表作成
  #蓄積防ぐためsum1, sum2をクリア
  sum1[1,1:12]<-0
  sum2[1,1:12]<-0

  #10歳ごと集計
  for(i in 1:10){
    #2011年集計
    sum1[1,1] <- sum1[1,1] + total1[j, 7+i] # 0- 9歳　計
    sum1[1,2] <- sum1[1,2] + total1[j,17+i] #10-19歳　計
    sum1[1,3] <- sum1[1,3] + total1[j,27+i] #20-29歳　計
    sum1[1,4] <- sum1[1,4] + total1[j,37+i] #30-39歳　計
    sum1[1,5] <- sum1[1,5] + total1[j,47+i] #40-49歳　計
    sum1[1,6] <- sum1[1,6] + total1[j,57+i] #50-59歳　計
    sum1[1,7] <- sum1[1,7] + total1[j,67+i] #60-69歳　計
    sum1[1,8] <- sum1[1,8] + total1[j,77+i] #70-79歳　計
    sum1[1,9] <- sum1[1,9] + total1[j,87+i] #80-89歳　計
    sum1[1,10] <- sum1[1,10] + total1[j,97+i] #90-99歳　計
    sum1[1,11] <- total1[j,108] #100歳以上
    sum1[1,12] <- total1[j,6] #総数

    #2020年集計
    sum2[1,1] <- sum2[1,1] + total2[j, 7+i] # 0- 9歳　計
    sum2[1,2] <- sum2[1,2] + total2[j,17+i] #10-19歳　計
    sum2[1,3] <- sum2[1,3] + total2[j,27+i] #20-29歳　計
    sum2[1,4] <- sum2[1,4] + total2[j,37+i] #30-39歳　計
    sum2[1,5] <- sum2[1,5] + total2[j,47+i] #40-49歳　計
    sum2[1,6] <- sum2[1,6] + total2[j,57+i] #50-59歳　計
    sum2[1,7] <- sum2[1,7] + total2[j,67+i] #60-69歳　計
    sum2[1,8] <- sum2[1,8] + total2[j,77+i] #70-79歳　計
    sum2[1,9] <- sum2[1,9] + total2[j,87+i] #80-89歳　計
    sum2[1,10] <- sum2[1,10] + total2[j,97+i] #90-99歳　計
    sum2[1,11] <- total2[j,108] #100歳以上
    sum2[1,12] <- total2[j,6] #総数
  }

  #表(df)に格納
  df[1,] <- sum1
  df[2,] <- sum2
  df[3,] <- sum2 - sum1

  #増減割合計算
  percentage <- (sum2 - sum1) / sum1 * 100

  #セルの位置情報を持ったデータ型に変換
  mytheme <- gridExtra::ttheme_default(base_size=6, base_family="HiraKakuProN-W3")
  g2 <- gridExtra::tableGrob(df, theme = mytheme)
  #枠つけ色塗り
  g2 <- gtable::gtable_add_grob(g2, #第一引数にデータを指定
                                grobs = rectGrob(gp=gpar(col="black",#枠線の色
                                                         fill="cyan", #Fillは塗りつぶし
                                                         lwd=5, #枠線の太さ
                                                         alpha=0.2)), #透過性
                                t = 4,  #Topの略。塗りつぶし枠の上限
                                b = nrow(g2), #bottomの略。枠の下限、行数をnrowで取得し指定すれば一番下まで
                                l = 1, #left。左側。
                                r = ncol(g2)#右側
                               )

  #描画 g1とg2の縦の比率が3:1になるように分割
  gridExtra::grid.arrange(g1, g2, heights=c(3,1))

  dev.off()
}
