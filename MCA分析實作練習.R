# 2019 厚數據與意義探勘工作坊 
# 民調資料探索練習
# 日期：108年8月16日
# 講師：國立中山大學政治學研究所劉正山教授

# 準備：請開啟一個新的專案，
# 將這個語法檔及資料檔（dataBD.rda）都放入該專案資料夾內

load("dataBD.rda")

library(dplyr)
library(FactoMineR)
library(factoextra)

dataBD4MCA  <- select(dataBD, 
                      Gender, # gender
                      college, # edu 
                      B19r, # 1 算是曉得與台灣相關的國際新聞
                      B22r, # 2 就業市場表現算好; 1 普通 0 不好
                      B23r, # 1 中國（大陸）工作環境好於台灣； 2 較差 3都有
                      B25r, # 1 心中國家的名字是台灣 2 中華民國 0其他
                      B29r, # 1 接受與與民主化後的中國（大陸）合為一個國家 2 不接受 3 看狀況
                      B31r, # 1 接受讓習近平管管看 2 不接受 3 沒想法
                      B33r, # 1 和家人一致偏統 2 偏獨 3 偏中間/多元
                      B38r, # 1 算是關注惠台政策
                      B39r, # 1 可接受大陸居住證 2 排斥 3 沒想法
                      B41r, # 1 像南北韓 2 不算 3 不清楚
                      B42r, # 1 一國兩制在港澳實施的效果還可以 2 不理想 3 不清楚
                      B44r, # 1 統一有利於提升人民福祉 2 獨立 3 維持現狀 4 不確定
                      B46r, # 1 被統後民生會變好 2 不會 3 不確定
                      B47r, # 1 被統後經濟會變好 2 不會 3 不確定
                      B51r, # 1 兩岸戰時積極抗戰 2 敗戰投降 3 不確定
                      B53r, # 1 統獨議題算重要 0 不重要
                      B54r, # 1 統獨需要去談 2 不需談 3以後再談/其他
                      B56r, # 1 統獨是假議題 2 不算 3 不確定
                      B57r, # 1 身份認同唯台灣人 2 唯中國人 3 都有
                      D25r, # 1 所支持的縣市長候選人當選 2 落選 3無支持的人選
                      D52r, # 1 選後支持柯文哲 2 不支持 3 普通
                      D56r, # 1 選後支持韓國瑜 2 不支持 3 普通
                      D58r, # 1 覺得自己也有影響政治的能力
                      D59r, # 1 覺政治事務太複雜，難理解
                      D60r, # 1 覺得去不去投票結果都一樣
                      D61r, # 1 投票是一種公民應盡的責任
                      D63r, # 1 國家的重大政策，應交由全民公投決定
                      D67r, # 1 民主最重要的就是有基本人權的保障；0 不算前三重要
                      D71r, # 1 民主最重要的就是有定期選舉；0 不算前三重要
                      D76r, # 1 只要能讓生活過得好就算不民主也沒關係
                      D77r, # 1 只要能將壞人繩之以法，就算程序不正義也沒關係
                      D78r, # 1 有一個不會受到立法院和選舉牽制的領導者對國家是好事
                      D79r, # 1 為解決媒體亂象應讓政府來把關某些言論可不可以在社會上傳播
                      D81r, # 1 不管什麼情況民主政治都是最好的體制
                      D82r, # 1 專制政體比民主政體好
                      D83r, # 1 對我而言，任何一種政治體制都是一樣的
                      D93r, # 1 對於通過公投第10案「同意民法婚姻規定應限定在一男一女的結合」(69.5%)  欣慰 2 失望 3沒想法
                      D98r, # 1 對於不通過第13案「以台灣（Taiwan）申請所有國際賽事」(52.3%) 欣慰 2 失望 3沒想法
                      D99r, # 1 會主動和家人、親友談論政治話題 2 不會 3很少討論
                      D104r, # 1 重視媒體上的民意調查報導
                      D132r, # 1 美國成為全球第一大經濟體 2 中國 0 其他
                      D134r, # 1 對中華民國的國旗海正面感受 2 負面 3 中性/無感
                      D136r, # 1 肯定中華民國正當性 2 挑戰 3 中性/無感
                      D140r, # 1 肯定一個中國為兩岸底線 等於戰敗 3 中性無感
                      D142r, # 1 肯定北京說出「九二共識」為一中原則的說法 2否定 3不清
                      D146r, # 1 對目前生活的現狀感到滿意
                      D147r, # 1 你對台灣民主實施的現況滿意
                      D150r # 1 願意到中國求學或工作 2 不願意 
)

dataBD4MCA.nona <- na.omit(dataBD4MCA) # 去除無回答的人
nrow(dataBD4MCA.nona) #559

names(dataBD4MCA.nona) 
resBD<-MCA(dataBD4MCA.nona, ncp=10, graph= F) 
fviz_screeplot(resBD, ncp=10) 

library(sjPlot)
library(sjmisc)
plot_frq(dataBD$D52r)

#dataBD$Email <- NULL
#dataBD$Name.x <- NULL 

# BD變數類別關係圖

# top 80
plot(resBD, axes=c(1, 2), new.plot=TRUE, 
     col.var="black", col.ind="black", col.ind.sup="black",
     col.quali.sup="darkgreen", col.quanti.sup="blue",
     label=c("var"), cex=0.7, 
     selectMod = "cos2 80",
     invisible=c("ind", "quali.sup"), 
     autoLab = "yes",
     xlim=c(-1, 1.5), ylim=c(-1.3, 1.5),
     title="") 

# 受訪者分佈圖
plot(resBD, axes=c(1, 2), new.plot=TRUE,
     col.var="red", col.ind="brown", col.ind.sup="black",
     col.quali.sup="darkgreen", col.quanti.sup="blue",
     label=c("var"), cex=0.8,
     selectMod = "cos2",
     invisible=c("var", "quali.sup"),
     xlim=c(-1, 1.5),
     title="")


## 用卡方檢定確認具潛在關聯的變數之間的相關性  


sjt.xtab(dataBD$D132r, dataBD$B29r, 
         show.row.prc = TRUE, # 顯示列百分比
         show.col.prc = TRUE  # 顯示欄百分比
)
