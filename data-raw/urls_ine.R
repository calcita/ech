urls_ine <- structure(list(yy = 2011:2022, md_sav = structure(c("http://www.ine.gub.uy:82/Anda5/index.php/catalog/726/download/1096",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/725/download/1088",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/724/download/1079",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/723/download/1071",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/721/download/1062",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/722/download/1044",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/720/download/1019",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/719/download/1015",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/715/download/956",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/714/download/957",
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/716/download/971", # Segundo semestre implantacion
                                                                "http://www.ine.gub.uy:82/Anda5/index.php/catalog/717/download/984"), #Primer semestre implantacion
                                                              class = c("fs_path", "character")),
                           upm_sav = structure(c("https:/www.ine.gub.uy/c/document_library",
                                                 "https:/www.ine.gub.uy/c/document_library", "https:/www.ine.gub.uy/c/document_library",
                                                 "https:/www.ine.gub.uy/c/document_library", "https:/www.ine.gub.uy/c/document_library",
                                                 "https:/www.ine.gub.uy/c/document_library", "https:/www.ine.gub.uy/c/document_library",
                                                 "http://www.ine.gub.uy:82/Anda5/index.php/catalog/719/download/1016",
                                                 "https:/www.ine.gub.uy/c/document_library", "https:/www.ine.gub.uy/c/document_library",
                                                 "https:/www.ine.gub.uy/c/document_library", "https:/www.ine.gub.uy/c/document_library"
                                                 ), class = c("fs_path", "character")),
                           dic = structure(c("http://www.ine.gub.uy:82/Anda5/index.php/catalog/726/download/1098",
                                             "http://www.ine.gub.uy:82/Anda5/index.php/catalog/725/download/1089",
                                             "http://www.ine.gub.uy:82/Anda5/index.php/catalog/724/download/1080",
                                             "http://www.ine.gub.uy:82/Anda5/index.php/catalog/723/download/1073",
                                             "http://www.ine.gub.uy:82/Anda5/index.php/catalog/721/download/1040",
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2016/Diccionario%20ECH%20%202016.xls",
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2017/Dicccionario%20de%20Variables%20ECH%202017.xls",
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2018/Diccionario%20de%20Variables%20ECH%202018.xlsx",
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2019/Diccionario%20de%20Variables%20ECH%202019%20(1).xlsx",
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2020/DICCIONARIO%20ECH%202020_tercerosBaseReducida%20(1).xlsx",
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2021/DICCIONARIO%20ECH%202021_2doSemestre_Terceros%20(2).xlsx", #Segundo semestre
                                             "https://www5.ine.gub.uy/documents/ANDA/ECH/2022/DICCIONARIO%20ECH%202022_1erSemestre%20(1).xlsx" #primer semestre
                           ), class = c("fs_path", "character"))), class = "data.frame", row.names = c(NA, -9L))

usethis::use_data(urls_ine, overwrite = TRUE)
