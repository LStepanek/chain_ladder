###############################################################################
###############################################################################
###############################################################################

## instaluji a inicializuji balíčky -------------------------------------------

for(package in c(
                 "openxlsx",
                 "xtable"
                 )){
                 
    if(!(package %in% rownames(installed.packages()))){
    
        install.packages(
            package,
            dependencies = TRUE,
            repos = "http://cran.us.r-project.org"
        )
        
    }
    
    library(package, character.only = TRUE)
    
}


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"domaci_ukol_4.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## helper funkce --------------------------------------------------------------

isTriangle <- function(
    
    triangle_data
    
){
    
    # '''
    # Kontroluje, zda čtvercová matice "triangle_data" může být
    # smysluplným aktuárským trojúhelníkem.
    #
    # Čtvercovou matici "triangle_data" považujeme za smysluplný
    # aktuárský trojúhelník, pokud má v horním levém trojúhelníkovém
    # poli závazky tak, že v řádcích jsou závazky vzniklé v roce,
    # který je uveden řádkem, a vyřešeném v roce, který
    # uvozuje sloupec; dolní pravé trojúhelníkové pole je tvořeno
    # nedostupnými hodnotami (NA).
    #
    # Zároveň je požadováno, aby měla data "triangle_data" smysluplné
    # popisky.
    # '''
    
    if(dim(triangle_data)[1] != dim(triangle_data)[2]){
        
        return(FALSE)
        
    }
    
    if(any(
        is.na(
            as.numeric(
                triangle_data[upper.tri(
                    as.matrix(
                        triangle_data,
                        nrow = triangle_data[1]
                    ),
                    diag = TRUE
                )[, dim(triangle_data)[2]:1]]
            )
        )
    )){
        
        return(FALSE)
        
    }
    
    if(!all(
        is.na(
            as.numeric(
                triangle_data[lower.tri(
                    as.matrix(
                        triangle_data,
                        nrow = triangle_data[1]
                    ),
                    diag = FALSE
                )[, dim(triangle_data)[2]:1]]
            )
        )
    )){
        
        return(FALSE)
        
    }
    
    if(is.null(colnames(triangle_data))){
        
        return(FALSE)
        
    }
    
    if(is.null(rownames(triangle_data))){
        
        return(FALSE)
        
    }
    
    if(any(is.na(
        as.integer(
            gsub(
                "(.*?)([0-9]+)(.*?)",
                "\\2",
                colnames(triangle_data)
            )
        )
    ))){
        
        return(FALSE)
        
    }
    
    if(any(is.na(
        as.integer(
            gsub(
                "(.*?)([0-9]+)(.*?)",
                "\\2",
                rownames(triangle_data)
            )
        )
    ))){
        
        return(FALSE)
        
    }
    
    if(length(
        unique(
            as.integer(
                gsub(
                    "(.*?)([0-9]+)(.*?)",
                    "\\2",
                    colnames(triangle_data)
                )
            )[2:length(colnames(triangle_data))] - as.integer(
                gsub(
                    "(.*?)([0-9]+)(.*?)",
                    "\\2",
                    colnames(triangle_data)
                )
            )[1:(length(colnames(triangle_data)) - 1)]
        )
    ) != 1){
        
        return(FALSE)
        
    }
    
    if(length(
        unique(
            as.integer(
                gsub(
                    "(.*?)([0-9]+)(.*?)",
                    "\\2",
                    rownames(triangle_data)
                )
            )[2:length(rownames(triangle_data))] - as.integer(
                gsub(
                    "(.*?)([0-9]+)(.*?)",
                    "\\2",
                    rownames(triangle_data)
                )
            )[1:(length(rownames(triangle_data)) - 1)]
        )
    ) != 1){
        
        return(FALSE)
        
    }
    
    return(TRUE)
    
}


## ----------------------------------------------------------------------------

asTriangle <- function(
    
    triangle_data
    
){
    
    # '''
    # Čtvercovou matici "triangle_data", která je smysluplným
    # aktuárským trojúhelníkem, vrací ve formátu, kde horní levé
    # trojúhelníkové pole jsou závazky tak, že v řádcích jsou závazky
    # vzniklé v roce, který je uveden řádkem, a vyřešeném v roce, který
    # uvozuje sloupec; dolní pravé trojúhelníkové pole je tvořeno
    # nedostupnými hodnotami (NA). Zároveň jsou přidány popisky popisků.
    # '''
    
    if(isTriangle(triangle_data)){
        
        return(
            matrix(
                apply(triangle_data, 2, as.numeric),
                nrow = dim(triangle_data)[1],
                dimnames = list(
                    "origin" = gsub(
                        "(.*?)([0-9]+)(.*?)",
                        "\\2",
                        rownames(triangle_data)
                    ),
                    "development" = gsub(
                        "(.*?)([0-9]+)(.*?)",
                        "\\2",
                        colnames(triangle_data)
                    )
                )
            )
        )
        
    }
    
}


## ----------------------------------------------------------------------------

getCumulativeTriangle <- function(
    
    my_triangle
    
){
    
    # '''
    # Vrací pro objekt "my_triangle", který je aktuárským trojúhelníkem,
    # kumulativní závazky pro každý rok vzniku.
    # '''
    
    if(isTriangle(my_triangle)){
        
        return(
            asTriangle(
                t(apply(
                    my_triangle,
                    1,
                    cumsum
                ))
            )
        )
        
    }
    
}


## ----------------------------------------------------------------------------

getDevelopmentFactors <- function(
    
    cumulative_triangle
    
){
    
    # '''
    # Vrací pro objekt "cumulative_triangle", který je kumulativním
    # aktuárským trojúhelníkem, vývojové faktory pro všechny
    # sousední dvojice development let.
    # '''
    
    if(isTriangle(cumulative_triangle)){
                
        my_sums_wi_diag <- apply(
            cumulative_triangle,
            2,
            sum,
            na.rm = TRUE
        )
        
        my_sums_wo_diag <- apply(
            cumulative_triangle,
            2,
            sum,
            na.rm = TRUE
        ) - rev(
            diag(
                cumulative_triangle[,
                    dim(cumulative_triangle)[2]:1
                ]
            )
        )
        
        return(
            
            setNames(
                my_sums_wi_diag[2:length(my_sums_wi_diag)] /
                my_sums_wo_diag[1:(length(my_sums_wo_diag) - 1)],
                paste(
                    colnames(cumulative_triangle)[
                        2:length(colnames(cumulative_triangle))
                    ],
                    colnames(cumulative_triangle)[
                        1:(length(colnames(cumulative_triangle)) - 1)
                    ],
                    sep = "/"
                )
            )
            
        )
        
    }
    
}


## ----------------------------------------------------------------------------

getOutstandingClaimsReserves <- function(
    
    cumulative_triangle
    
){
    
    # '''
    # Vrací pro objekt "cumulative_triangle", který je kumulativním
    # aktuárským trojúhelníkem, odhady budoucích (nesplacených) rezerv.
    # '''
    
    if(isTriangle(cumulative_triangle)){
        
        my_matrix <- matrix(
            data = diag(rev(
                diag(
                    cumulative_triangle[,
                        dim(cumulative_triangle)[2]:1
                    ]
                )
            ))[
                dim(cumulative_triangle)[1]:1
                ,
            ],
            nrow = dim(cumulative_triangle)[1],
            dimnames = list(
                    "origin" = rownames(cumulative_triangle),
                    "development" = colnames(cumulative_triangle)
            )
        )
        
        my_matrix[upper.tri(my_matrix)[, dim(my_matrix)[2]:1]] <- 1
        my_matrix[lower.tri(my_matrix)[, dim(my_matrix)[2]:1]] <-
        t(
            replicate(
                dim(cumulative_triangle)[1],
                c("1/0" = 1, getDevelopmentFactors(cumulative_triangle))
            )
        )[lower.tri(my_matrix)[, dim(my_matrix)[2]:1]]
        
        output <- t(
            apply(
                my_matrix,
                1,
                cumprod
            )
        )
        
        output[upper.tri(output, diag = TRUE)[, dim(output)[2]:1]] <- NA
        
        return(
            output
        )
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## nahrávám data --------------------------------------------------------------

setwd(mother_working_directory)

my_triangle <- asTriangle(read.table(
    file = "trojuhelnik.csv",
    header = TRUE,
    sep = ",",
    row.names = 1    
))

my_codebook <- read.table(
    file = "codebook.txt",
    header = TRUE,
    sep = ";",
    colClasses = "character"
)


## ----------------------------------------------------------------------------

###############################################################################

## počítám kumulativní formu trojúhelníku -------------------------------------

cumulative_triangle <- getCumulativeTriangle(my_triangle)


## počítám vývojové faktory ---------------------------------------------------

development_factors <- t(
    as.matrix(getDevelopmentFactors(cumulative_triangle))
)


## počítám budoucí nesplacené závazky (rezervy) -------------------------------

outstanding_claims_reserves <- getOutstandingClaimsReserves(
    cumulative_triangle
)


## ----------------------------------------------------------------------------

###############################################################################

## TeXový výstup --------------------------------------------------------------

for(my_table_name in c(
    "my_triangle",
    "cumulative_triangle",
    "development_factors",
    "outstanding_claims_reserves"
)){
    
    # '''
    # Vrací TeXový kód postupně pro všechny zadané tabulky.
    # '''
    
    my_table <- get(my_table_name)
    
    print(xtable(my_table, align = rep("", ncol(my_table) + 1),
                        digits = if(
                            my_table_name == "development_factors"
                        ){3}else{0}),
                        floating = FALSE, tabular.environment = "tabular",
                        hline.after = NULL, include.rownames = TRUE,
                        include.colnames = TRUE
                        )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím i excelový výstup -------------------------------------------------

#### vytvářím sešit -----------------------------------------------------------

actuary_triangles <- createWorkbook()


for(my_table_name in c(
    "my_triangle",
    "cumulative_triangle",
    "development_factors",
    "outstanding_claims_reserves"
)){
    
    # '''
    # Vrací excelový sešit s listy obsahujícími jednotlivé tabulky.
    # '''
    
    my_table <- get(my_table_name)
    
    
    ## vytvářím list ----------------------------------------------------------

    addWorksheet(
        wb = actuary_triangles,
        sheetName = my_codebook[
            my_codebook$variable_name == my_table_name,
            "variable_czech_name"
        ]
    )
    
    
    ## ukládám do sešitu data -------------------------------------------------

    writeData(
        wb = actuary_triangles,
        sheet = my_codebook[
            my_codebook$variable_name == my_table_name,
            "variable_czech_name"
        ],
        rowNames = TRUE,
        colNames = TRUE,
        x = my_table
    )


    ## nastavuji automatickou šířku sloupce -----------------------------------
     
    setColWidths(
        wb = actuary_triangles,
        sheet = my_codebook[
            my_codebook$variable_name == my_table_name,
            "variable_czech_name"
        ],
        cols = 1:(dim(my_table)[2] + 1),
        widths = "auto"
    )


    ## vytvářím dva své styly - jednak tučné písmo, jednak písmo zarovnané
    ## doprava v rámci buňky --------------------------------------------------
    
    my_bold_style <- createStyle(textDecoration = "bold")
    right_halign_cells <- createStyle(halign = "right")
    
    addStyle(
        wb = actuary_triangles,
        sheet = my_codebook[
            my_codebook$variable_name == my_table_name,
            "variable_czech_name"
        ],
        style = my_bold_style,
        rows = c(1:(dim(my_table)[1] + 1), rep(1, dim(my_table)[2] + 1)),
        cols = c(rep(1, dim(my_table)[1] + 1), 1:(dim(my_table)[2] + 1))
    )
    
    addStyle(
        wb = actuary_triangles,
        sheet = my_codebook[
            my_codebook$variable_name == my_table_name,
            "variable_czech_name"
        ],
        style = right_halign_cells,
        rows = 2:(dim(my_table)[1] + 1),
        cols = 2:(dim(my_table)[2] + 1),
        gridExpand = TRUE
    )
    
}


## ----------------------------------------------------------------------------

#### ukládám workbook ---------------------------------------------------------

setwd(mother_working_directory)

saveWorkbook(
    wb = actuary_triangles,
    file = "aktuarske_trojuhelniky.xlsx",
    overwrite = TRUE
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





