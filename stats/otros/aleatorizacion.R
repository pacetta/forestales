library(agricolae)

variedades <- c("E. Dunni","CG09", "H117", "H105","CG 08")
tratamientos <- c("control", "sub-con yeso", "sub-sin yeso")

outdesign <- design.split(trt1 = variedades, trt2 = tratamientos, seed = 28,
             r= 4, design= "rcbd")

book<-outdesign$book

writexl::write_xlsx(book,"book.xlsx")
file.show("book.xlsx")
