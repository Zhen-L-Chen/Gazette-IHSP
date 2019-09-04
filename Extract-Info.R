library(pdftools)
library(tidyverse)
library(dplyr)
library(rlist)
library(stringr)

#Trim white spaces at the front
trim.leading <- function(x) {
  sub("^\\s+", "", x)
}

#Trim white spaces at the end
trim.trailing <- function(x){
  sub("\\s+$", "", x)
}

download <- function(year, volume){
  year.end = year %% 100
  if(year.end < 10){
    year.end <- as.character(year.end)
    year.end = paste("0", year.end, sep = "")
  } else{
    year.end = as.character(year.end)
  }
  if(volume < 10){
    volume <- as.character(volume)
    volume = paste("0", volume, sep = "")
  } else{
    volume = as.character(volume)
  }
  download.name = sprintf("http://www2.publicationsduquebec.gouv.qc.ca/dynamicSearch/telecharge.php?type=4&file=%s%s.PDF",
                          year.end, volume)
  index.name = sprintf("./Index%s%s.pdf", year.end, volume)
  file <- download.file(download.name, index.name)
  return(file)
}

#PDF path
path <- function(year, volume){
  year.end = year %% 100
  if(year.end < 10){
    year.end <- as.character(year.end)
    year.end = paste("0", year.end, sep = "")
  } else{
    year.end = as.character(year.end)
  }
  if(volume < 10){
    volume <- as.character(volume)
    volume = paste("0", volume, sep = "")
  } else{
    volume = as.character(volume)
  }
  index.name = sprintf("./Index%s%s.pdf", year.end, volume)
  return(index.name)
}

download(2005, 35)

text.pdf <- pdf_text(path(2005, 35))

#Return year and volume 

year.volume <- function(info){
  a <- strsplit(info, ",")
  a <- unlist(a)
  b <- list()
  b <- list.append(b, trim.leading(a[1]))
  b <- list.append(b, trim.leading(a[2]))
  b <- unlist(b)
  return(b)
}

#Find page for "PROFESSIONNELS.."

page.find <- function(text.pdf){
  a = "Professionnels non participants"
  return(grep(a, text.pdf))
}

#Extract one page and standardize

extract.one.page <- function(text.pdf){
  a <- page.find(text.pdf)
  b <- length(a)
  pdf.one.page <- text.pdf[a[1:b]]
  pdf.one.page <- strsplit(pdf.one.page, "\n")
  pdf.one.page <- unlist(pdf.one.page)
  c <- (grep("Professionnels non participants", pdf.one.page)[1])
  pdf.one.page.len <- length(pdf.one.page)
  page.no.overhead <- pdf.one.page[(c+1):pdf.one.page.len]
  page.no.overhead.len <- length(page.no.overhead)
  d <- (grep("Dentiste", page.no.overhead))
  page.no.bottom <- page.no.overhead[1:(d-1)]
  page.no.bottom.len <- length(page.no.bottom)
  e <- (1: page.no.bottom.len)
  #remove fluff
  fluff.list <- list()
  fluff.list <- list.append(fluff.list, (grep("GAZETTE",page.no.bottom)))
  fluff.list <- list.append(fluff.list, (grep("Date",page.no.bottom)))
  fluff.list <- list.append(fluff.list, (grep("effective",page.no.bottom)))
  fluff.list <- list.append(fluff.list, (grep("Professionnels",page.no.bottom)))
  fluff.list <- unlist(fluff.list)
  fluff.list <- unique(fluff.list)
  f <- setdiff(e, fluff.list)
  page.cleaned <- page.no.bottom[f]
  return(page.cleaned)
}

#trim leading for every line
trim.leading.pdf <- function(text.pdf){
  a <- extract.one.page(text.pdf)
  a.len <- length(a)
  list.all <- list()
  for(i in 1:a.len){
    list.all <- list.append(list.all, trim.leading(a[i]))
  }
  list.all <- unlist(list.all)
  return(list.all)
}

#check which line needs to be appended
which.append.pdf <- function(text.pdf){
  a <- trim.leading.pdf(text.pdf)
  a.len <- length(a)
  list.a <- list()
  for(i in (1:a.len)){
    b <- strsplit(a[i], ',')
    b <- unlist(b)
    if(length(b) == 1){
      list.a <- list.append(list.a, i)
    }else{
      next
    }
  }
  return(list.a)
}

#append the right lines
append.pdf <- function(text.pdf){
  text <- trim.leading.pdf(text.pdf)
  a <- which.append.pdf(text.pdf)
  a.len <- length(a)
  a <- unlist(a)
  b.append <- list()
  for(i in 1:a.len){
    num <- a[i]
    e <- gregexpr("\\w\\d\\w \\d\\w\\d", text[num])
    if(e != -1){
      b.append <- list.append(b.append, paste(text[num-1], text[num], sep = " "))
    }else{
      next
    }
  }
  b.append <- unlist(b.append)
  return(b.append)
}


#add commma after Quebec
comma.pdf <- function(text.pdf){
  a <- append.pdf(text.pdf)
  a <- unlist(a)
  a.len <- length(a)
  final.list <- list()
  for(i in 1:a.len){
    b <- a[i]
    c <- strsplit(b, ")")
    c <- unlist(c)
    d <- paste(c[1], c[2], sep = ",")
    final.list <- list.append(final.list, d)
  }
  final.list <- unlist(final.list)
  return(final.list)
}

#match substring to main string
match.string <- function(text.pdf){
  a <- comma.pdf(text.pdf)
  b <- extract.one.page(text.pdf)
  a.len <- length(a)
  list.matching <- list()
  for(i in (1:a.len)){
    c <- (substring(a[i], 1, 12))
    d <- grep(c, b)
    list.matching <- list.append(list.matching, d)
  }
  list.matching <- unlist(list.matching)
  return(list.matching)
}

#replace substring to main text
replace.sub <- function(text.pdf){
  a <- comma.pdf(text.pdf)
  a.len <- length(a)
  b <- extract.one.page(text.pdf)
  c <- match.string(text.pdf)
  c <- unlist(c)
  b[c] <- a[1:a.len]
  return(b)
}

match.string.delete <- function(text.pdf){
  a <- match.string(text.pdf)
  a <- a + 1
}

final.version.pdf <- function(text.pdf){
  a <- match.string.delete(text.pdf)
  b <- replace.sub(text.pdf)
  b.len <- length(b)
  c <- c(1:b.len)
  d <- setdiff(c, a)
  return(b[d])
}


#append the right lines for remaining odd lines in final version
verification.final.pdf <- function(text.pdf){
  a <- final.version.pdf(text.pdf)
  a <- trim.leading(a)
  a.len <- length(a)
  b.append <- list()
  for(i in 1:a.len){
    c <- gregexpr("\\w\\d\\w \\d\\w\\d", a[i])
    d <- grepl("Région", a[i])
    e <- grepl("Médecin", a[i])
    f <- grepl("Québec", a[i])
    if(c == -1 & d == FALSE & e == FALSE & f == FALSE){
      b.append <- list.append(b.append, paste(a[i], a[i+1], sep = " "))
    }
  }
  b.append <- unlist(b.append)
  return(b.append)
}

#match verification substring to final main string (IF NULL DON'T DO VERIFICATION FINAL)
match.string.final <- function(text.pdf){
  a <- verification.final.pdf(text.pdf)
  b <- final.version.pdf(text.pdf)
  a.len <- length(a)
  list.matching <- list()
  for(i in (1:a.len)){
    c <- (substring(a[i], 1, 12))
    d <- grep(c, b)
    list.matching <- list.append(list.matching, d)
  }
  list.matching <- unlist(list.matching)
  return(list.matching)
}

#diff in final 
replace.sub.final <- function(text.pdf){
  a <- verification.final.pdf(text.pdf)
  a.len <- length(a)
  b <- final.version.pdf(text.pdf)
  c <- match.string.final(text.pdf)
  c <- unlist(c)
  b[c] <- a[1:a.len]
  return(b)
}

#delete match
match.string.final.delete <- function(text.pdf){
  a <- match.string.final(text.pdf)
  b <- a + 1
  return(b)
}

#setdiff
final.pdf <- function(text.pdf){
  a <- match.string.final.delete(text.pdf)
  b <- replace.sub.final(text.pdf)
  b.len <- length(b)
  c <- c(1:b.len)
  d <- setdiff(c, a)
  return(b[d])
}

choose.final.pdf <- function(text.pdf){
  a <- verification.final.pdf(text.pdf)
  if(is.null(a)){
    return(final.version.pdf(text.pdf))
  }else{
    return(final.pdf)
  }
}


#standardize comma
standardize.final.pdf <- function(text.pdf){
  a <- choose.final.pdf(text.pdf)
  a.len <- length(a)
  final.list <- list()
  for(i in 1:a.len){
    x <- grepl("Région", a[i])
    y <- grepl("Médecin", a[i])
    z <- grepl("Québec,", a[i])
    if(x == TRUE || y == TRUE || z == TRUE){
      next 
    }else{
      b <- a[i]
      c <- strsplit(b, ")")
      c <- unlist(c)
      d <- paste(c[1], c[2], sep = ",")
      final.list <- list.append(final.list, d)
    }
  }
  final.list <- unlist(final.list)
  return(final.list)
}

#final match
final.match <- function(text.pdf){
  a <- standardize.final.pdf(text.pdf)
  b <- choose.final.pdf(text.pdf)
  a.len <- length(a)
  list.matching <- list()
  for(i in (1:a.len)){
    c <- (substring(a[i], 1, 12))
    d <- grep(c, b)
    list.matching <- list.append(list.matching, d)
  }
  list.matching <- unlist(list.matching)
  return(list.matching)
}

#replace match string
working.version <- function(text.pdf){
  a <- standardize.final.pdf(text.pdf)
  a.len <- length(a)
  b <- choose.final.pdf(text.pdf)
  c <- final.match(text.pdf)
  c <- unlist(c)
  b[c] <- a[1:a.len]
  return(b)
}

#append the type of doctor
append.doctor <- function(text.pdf){
  a <- working.version(text.pdf)
  a.len <- length(a)
  b <- grep("Médecin", a)
  final.list <- list()
  for(i in b[1]:b[2]){
    w <- grepl("Région", a[i])
    x <- grepl("Médecins omnipraticiens", a[i])
    z <- gregexpr("\\w\\d\\w \\d\\w\\d", a[i])
    if(w == FALSE || x == FALSE & z != -1){
      final.list <- list.append(final.list, paste(a[i], "Médecin omnipracticien", sep = ", "))
    }else{
      final.list <- list.append(final.list, a[i])
    }
  }
  for(i in b[2]:a.len){
    w <- grepl("Région", a[i])
    y <- grepl("Médecins spécialistes", a[i])
    z <- gregexpr("\\w\\d\\w \\d\\w\\d", a[i])
    if(w == FALSE ||  y == FALSE & z != -1){
      final.list <- list.append(final.list, paste(a[i], "Médecin spécialiste", sep = ", "))
    }else{
      final.list <- list.append(final.list, a[i])
    }
  }
  final.list <- unlist(final.list)
  return(final.list)
}

remove.doctor <- function(text.pdf){
  a <- append.doctor(text.pdf)
  a.len <- length(a)
  list.a <- list()
  for(i in 1:a.len){
    y <- grepl("Médecins", a[i])
    z <- gregexpr("\\w\\d\\w \\d\\w\\d", a[i])
    if(y == TRUE & z == -1){
      list.a <- list.append(list.a, i)
    }
  }
  list.a <- unlist(list.a)
  return(list.a)
}

extra.doctor <- function(text.pdf){
  a <- append.doctor(text.pdf)
  a.len <- length(a)
  a.num <- c(1:a.len)
  b <- remove.doctor(text.pdf)
  c <- setdiff(a.num, b)
  return(c)
}

clean.doctor <- function(text.pdf){
  a <- append.doctor(text.pdf)
  b <- extra.doctor(text.pdf)
  c <- a[b]
  return(c)
}

#append regions
append.regions <- function(text.pdf){
  a <- clean.doctor(text.pdf)
  a.len <- length(a)
  b <- which(grepl("Région", a))
  b.len <- length(b)
  b.max <- which.max(b)
  max.num <- b[b.max]
  list.c <- list()
  for(i in 1:b.len){
    if(b[i] == max.num){
      x <- (b[i] + 1)
      y <- (a.len)
      z <- b[i]
    }else{
      x <- (b[i] + 1)
      y <- (b[i+1] - 1)
      z <- a[i]
    }
    for(i in x:y){
      list.c <- list.append(list.c, paste(a[i], a[x-1], sep = ", "))
    }
  }
  list.c <- unlist(list.c)
  return(list.c)
}

final.list <- function(text.pdf){
  a <- append.regions(text.pdf)
  a <- unlist(a)
  len.a <- length(a)
  list.last <- list()
  list.first <- list()
  list.doctor <- list()
  list.municipality <- list()
  list.region <- list()
  list.date.one <- list()
  list.date.two <- list()
  list.year <- list()
  list.volume <- list()
  info <- year.volume("2015, 23")
  list.year <- list.append(list.year, info[1])
  list.volume <- list.append(list.volume, info[2])
  b <- strsplit(a, ",")
  for(i in 1:len.a){
    x <- unlist(b[i])
    c <- which(grepl("Région", x))
    d <- which(grepl("Médecin", x))
    e <- gregexpr("\\(Québec", x)
    e <- unlist(e)
    e <- which(e != -1)
    f <- regmatches(x, gregexpr("\\d\\d \\d\\d \\d\\d", x))
    f <- unlist(f)
    f.len <- length(f)
    if(f.len == 1){
      list.date.one <- list.append(list.date.one, f[1])
      list.date.two <- list.append(list.date.two, NA)
    }else{
      list.date.one <- list.append(list.date.one, f[1])
      list.date.two <- list.append(list.date.two, f[2])
    }
    list.last <- list.append(list.last, x[1])
    list.first <- list.append(list.first, x[2])
    h <- x[e]
    h <- gsub("[0-9]", " ", h)
    h <- gsub("\\(", "", h)
    h <- trim.leading(h)
    h <- trim.trailing(h)
    h <- strsplit(h, " ")
    h <- unlist(h)
    h.len <- length(h)
    k <- h[-h.len]
    k <- paste(k, collapse = " ")
    list.municipality <- list.append(list.municipality, k)
    list.doctor <- list.append(list.doctor, x[d])
    list.region <- list.append(list.region, x[c])
  }
  return(do.call(rbind, Map(data.frame, Last.Name = list.last, First.Name = list.first, Municipality = list.municipality, Doctor = list.doctor, Region = list.region, First.Date = list.date.one, Second.Date = list.date.two, Year = list.year, Volume = list.volume)))
}

main <- function(year, volume){
  download(year, volume)
  text.pdf <- pdf_text(path(year, volume))
  return(final.list(text.pdf))
}

a <- main(2011, 48)

