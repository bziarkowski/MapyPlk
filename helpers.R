
col <- c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4')
 

col2 <- c("#214369", "#3056a0", "#4474ab", "#6597b8", "#e57e62", "#d32c21", "#c10b12", "#90051a")

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

strefy_3 = c(1, 15, 17, 31, 33, 47, 49, 50, 63, 65, 66, 78, 79, 81, 82, 83, 93, 94,
             95, 97, 98, 99, 100, 108, 109, 110, 111, 113, 114, 115, 116, 117, 123,
             124, 125, 126, 127, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
             140, 141, 142, 129, 143, 145, 146, 147, 148, 149, 150, 151, 152, 153, 
             154, 155, 156, 157, 158)

kwad <- function(zawo) {
  a <- readRDS("data/Rzuty.RDS")
  a$zawodnik <- as.character(a$zawodnik)
  z <- as.data.frame(readRDS("data/KwadStrefy.RDS"))

  ah <- a[a$zawodnik == zawo,]
  n <-1:628
  tr <- function(n) {
    x1 <- z[n,1]
    x2 <- z[n,2]
    y1 <- z[n,3]
    y2 <- z[n,4]
    yadj <- z[n,6]
    xadj <- z[n,5]
    fgp <- z[n,9]
    id <- z[n, 10]
    c <- subset(ah, ah$x > x1 & ah$x < x2 & ah$y > y1 & ah$y < y2)
    c$xadj <- xadj
    c$yadj <- yadj
    c$nr <- n
    c$FGP <- fgp
    tak <- c$trafienie[c$trafienie == "celny"]
    c$FGA1 <- length(c$trafienie)
    c$FGM1 <- length(tak)
    c$id = id
    c <- c[1,]
    return(c)
  }
  
  dat <- data.frame()
  
  foreach(i = n, .errorhandling = "remove") %dopar% {  
    dat <- rbind(dat, tr(i))
  }
  
  n = unique(dat$id)
  
  compute = function(x) {
    dat2 = dat[dat$id == x,]
    comp = z[z$id==x,]
    comp_FPG = round((sum(comp$FGM)/sum(comp$FGA)),3)
    dat_FPG = round((sum(dat2$FGM)/sum(dat2$FGA)),3)
    dat2$vsAvg = dat_FPG - comp_FPG
    dat2$vsAvg[dat2$vsAvg > 0.20] <- 0.20
    dat2$vsAvg[dat2$vsAvg < -0.20] <- -0.20
    return(dat2)
  }
  
  sh <- data.frame()
  
  foreach(i = n, .errorhandling = "remove") %dopar% {  
    sh <- rbind(sh, compute(i))
  }
  return(sh)
}



kwad2 <- function(zawo, sezo) {
  a <- readRDS("data/Rzuty.RDS")
  a$zawodnik <- as.character(a$zawodnik)
  z <- as.data.frame(readRDS("data/KwadStrefy.RDS"))
  
  ah <- a[a$Sezon == sezo,]
  ah <- ah[ah$zawodnik == zawo,]
  n <-1:628
  n <-1:628
  tr <- function(n) {
    x1 <- z[n,1]
    x2 <- z[n,2]
    y1 <- z[n,3]
    y2 <- z[n,4]
    yadj <- z[n,6]
    xadj <- z[n,5]
    fgp <- z[n,9]
    id <- z[n, 10]
    c <- subset(ah, ah$x > x1 & ah$x < x2 & ah$y > y1 & ah$y < y2)
    c$xadj <- xadj
    c$yadj <- yadj
    c$nr <- n
    c$FGP <- fgp
    tak <- c$trafienie[c$trafienie == "celny"]
    c$FGA1 <- length(c$trafienie)
    c$FGM1 <- length(tak)
    c$id = id
    c <- c[1,]
    return(c)
  }
  
  dat <- data.frame()
  
  foreach(i = n, .errorhandling = "remove") %dopar% {  
    dat <- rbind(dat, tr(i))
  }
  
  n = unique(dat$id)
  
  compute = function(x) {
    dat2 = dat[dat$id == x,]
    comp = z[z$id==x,]
    comp_FPG = round((sum(comp$FGM)/sum(comp$FGA)),3)
    dat_FPG = round((sum(dat2$FGM)/sum(dat2$FGA)),3)
    dat2$vsAvg = dat_FPG - comp_FPG
    dat2$vsAvg[dat2$vsAvg > 0.20] <- 0.20
    dat2$vsAvg[dat2$vsAvg < -0.20] <- -0.20
    return(dat2)
  }
  
  sh <- data.frame()
  
  foreach(i = n, .errorhandling = "remove") %dopar% {  
    sh <- rbind(sh, compute(i))
  }
  return(sh)
}



  

###Asysty###


Tabela_asyst2 <- function(zawod) {
  
  c <- readRDS("data/Asysty.RDS")
  
  c$zawodnik <- as.character(c$zawodnik)
  
  c$asysta <- as.character(c$asysta)
  
  ast <- subset(c, c$asysta == zawod)
  ast$zawodnik <- as.character(ast$zawodnik)
  rzucajacy <- unique(ast$zawodnik)
  n <- 1:length(rzucajacy)
  
Tabela_asyst <- function(x) {
  zawodnik_asystowany <- rzucajacy[x]
  asysty_do <- subset(ast, ast$zawodnik == zawodnik_asystowany)
  liczba_asyst <- length(asysty_do$trafienie)
  Asysty_do_zawodnika <- data.frame(zawodnik_asystowany, liczba_asyst)
  colnames(Asysty_do_zawodnika) <- c("Zawodnik", "Asysty")
  return(Asysty_do_zawodnika)
}
  Tabela <- data.frame()
  for(i in n) {
    Tabela <- rbind(Tabela, Tabela_asyst(i))
  }
  Tabela <- arrange(Tabela, desc(Asysty))
  return(Tabela)
}




Tabela_asyst3 <- function(zawod, sez) {
  
  c <- readRDS("data/Asysty.RDS")
  
  c$zawodnik <- as.character(c$zawodnik)
  
  c$asysta <- as.character(c$asysta)
  ast <- subset(c, c$Sezon == sez)
  ast <- subset(ast, ast$asysta == zawod)
  ast$zawodnik <- as.character(ast$zawodnik)
  rzucajacy <- unique(ast$zawodnik)
  n <- 1:length(rzucajacy)
  
  Tabela_asyst <- function(x) {
    zawodnik_asystowany <- rzucajacy[x]
    asysty_do <- subset(ast, ast$zawodnik == zawodnik_asystowany)
    liczba_asyst <- length(asysty_do$trafienie)
    Asysty_do_zawodnika <- data.frame(zawodnik_asystowany, liczba_asyst)
    colnames(Asysty_do_zawodnika) <- c("Zawodnik", "Asysty")
    return(Asysty_do_zawodnika)
  }
  Tabela <- data.frame()
  for(i in n) {
    Tabela <- rbind(Tabela, Tabela_asyst(i))
  }
  Tabela <- arrange(Tabela, desc(Asysty))
  return(Tabela)
}


