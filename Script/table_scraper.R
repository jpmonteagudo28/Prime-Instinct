using("rvest", "data.table")
url1 <- ("https://t5k.org/glossary/page.php?sort=PRP")
file <- read_html(url1)
carmichaelTable <- html_table(file); prp_num <- as.data.table(carmichaelTable)
prp_num[,-1]
rm("url1","file","carmichaelTable")
colnames(prp_num) <-c("Base","Composite PRP < 500","Percentage")
gsub("\r\n","",prp_num$`Composite PRP < 500`);gsub("%","",prp_num$Percentage)
write.table(prp_num, file = "prp_number.txt",sep = "|")
