Top features and community keyness
================

Loading/prepping data
=====================

``` r
#setwd("code/work/")
options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

library(quanteda)
```

    ## Package version: 2.0.1

    ## Parallel computing: 2 of 6 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(stringr)
library(ggplot2)

#For 10 year chunks
#dfm_files <- list.files("../../data/work/netowrking_archives/dfm/", full.names = TRUE)
dfm_files <- list.files("../../ESTC_SNA_data_creation/data/work/netowrking_archives/com_dfm_titles/", full.names = TRUE)
#dfm_files <- dfm_files[-c(1:159)]
#dfm_files <- dfm_files[-c(267:length(dfm_files))] #update this if I can get the last few -1730

eras <- unique(str_extract(dfm_files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))

#stop words
latin_stopwords <- read.csv("../../ESTC_SNA_data_creation/data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
                     "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
                     "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
                     #THIS IS IMPORTANT!
                     "non-latin", "alphabet", "saith", "thus", "|", "¦", "⁻", "vol", "year", "printed", "published",
                     "na", "amp", "ye", "anno", "translated", "vpon", "vnto", "yeere", "haue", "three", "second", "thereof", "yeare", "written", "mr",
                     "wherein", "written", "esq", "author", "part", "dr", "volumes", "1800", "volume", "mrs", "m.d", "d.d", "b.d")
```

Basic analyses
==============

``` r
#Get top features per era

for(i_era in 1:length(eras)) {
  cat("\n", eras[i_era], "\n")
  
  temp_files <- dfm_files[grep(eras[i_era], dfm_files)]
  temp_dfm <- readRDS(temp_files)
  temp_dfm <- dfm_select(temp_dfm, pattern = c(stopwords("english"), stopwords("french"), stopwords("italian"), stopwords("spanish"), stopwords("german"), latin_stopwords$a, extra_stopwords), selection = "remove", valuetype = "fixed")
  
  cat("\n50 top features/tokens\n")
  print(topfeatures(temp_dfm, n = 20))
  
  com_num <- unique(temp_dfm@docvars$Community)
  
  #One era only has one community (1506-1515), so need to skip it
  if(length(com_num) == 1) { next }
  
  for (i_com_keyness in 1:length(com_num)) {
    cat("\nCommunity:", i_com_keyness, "\n")
    tstat_key <- textstat_keyness(temp_dfm, 
                                  target = temp_dfm@docvars$Community == paste0("Community_", i_com_keyness))
    print(textplot_keyness(tstat_key, n = 20))
  }
}
```

    ## 
    ##  1501-1510 
    ## 
    ## 50 top features/tokens
    ## begynneth     sarum  treatyse   henrici   termino      boke     lorde       god 
    ##        26        13        13        12        12        11        11        10 
    ##     kynge    anglie  ecclesie     henry   incipit      vsum    lytell    sancte 
    ##        10         9         9         9         8         8         8         7 
    ##    called     regis      holy   edwardi 
    ##         7         6         6         6 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-1.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-2.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-3.png)

    ## 
    ##  1506-1515 
    ## 
    ## 50 top features/tokens
    ##      lorde      kynge  begynneth        god    henrici      henry     anglie 
    ##         28         26         26         25         21         18         15 
    ##      regis    termino       holy      regni       boke      noble     sancti 
    ##         13         12         11         11         10          9          9 
    ##       yere parlyament   treatyse   certayne    statuta soueraygne 
    ##          9          9          9          8          8          8 
    ## 
    ##  1511-1520 
    ## 
    ## 50 top features/tokens
    ##       lorde       regis       regni         god       kynge     termino 
    ##          30          26          23          22          21          20 
    ##   begynneth      anglie     roberti      sancti        holy       henry 
    ##          19          19          17          16          16          15 
    ##        yere    magistri     edwardi  conquestum  parlyament     henrici 
    ##          12          12          12          12          12          12 
    ## grammatices whittintoni 
    ##          11          11 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-4.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-5.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-6.png)

    ## 
    ##  1516-1525 
    ## 
    ## 50 top features/tokens
    ##      begynneth        termino        roberti          lorde          regis 
    ##             32             23             22             21             21 
    ##           holy        edwardi            god          regni         anglie 
    ##             21             18             17             16             13 
    ##       magistri           yere lichfeldiensis           boke         sancti 
    ##             12             12             12             12             11 
    ##           kyng    grammatices     parlyament     conquestum          henry 
    ##             11             11             10             10             10 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-7.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-8.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-9.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-10.png)

    ## 
    ##  1521-1530 
    ## 
    ## 50 top features/tokens
    ##   termino      boke begynneth michaelis   henrici    whiche       god     lorde 
    ##        59        40        34        31        25        22        22        19 
    ## englysshe   roberti    pasche  treatyse   edwardi   dyaloge  treateth      kyng 
    ##        18        18        18        16        15        14        14        14 
    ##     regis     euery     sexti     maner 
    ##        13        13        13        13 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-11.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-12.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-13.png)

    ## 
    ##  1526-1535 
    ## 
    ## 50 top features/tokens
    ##   termino      boke   henrici     lorde      yere    whiche       god     kynge 
    ##        65        55        47        45        40        39        38        35 
    ## begynneth michaelis     henry englysshe     maner    called     regis  treatyse 
    ##        34        34        33        33        32        27        26        24 
    ##     regni     moste      good      holy 
    ##        24        23        22        21 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-14.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-15.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-16.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-17.png)

    ## 
    ##  1531-1540 
    ## 
    ## 50 top features/tokens
    ##       yere      henry      lorde      kynge    henrici        god       boke 
    ##         73         71         70         65         54         52         45 
    ##      moste     holden     kynges       holy     called   englyshe       good 
    ##         43         43         39         36         31         30         28 
    ##   statutes parlyament     realme  englysshe     reygne    termino 
    ##         28         27         25         25         25         24 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-18.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-19.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-20.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-21.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-22.png)

    ## 
    ##  1536-1545 
    ## 
    ## 50 top features/tokens
    ##       yere     kynges      henry      lorde    henrici        god       boke 
    ##         97         70         68         65         65         64         55 
    ##   englande      kynge     reygne      moste    termino   statutes     holden 
    ##         54         49         47         45         40         38         37 
    ##   christen      newly     realme parlyament        set      euery 
    ##         35         35         34         34         32         31 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-23.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-24.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-25.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-26.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-27.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-28.png)

    ## 
    ##  1541-1550 
    ## 
    ## 50 top features/tokens
    ##    kynges       god      yere      boke     lorde       set   churche     moste 
    ##       125       119       104       103        89        83        69        69 
    ##  englande     henry  christen     euery   henrici     newly     maner      wyth 
    ##        67        64        59        57        54        54        50        49 
    ## necessary     godly   termino     forth 
    ##        49        48        48        47 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-29.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-30.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-31.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-32.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-33.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-34.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-35.png)

    ## 
    ##  1546-1555 
    ## 
    ## 50 top features/tokens
    ##      god     yere    lorde    moste     boke   kynges      set  churche 
    ##      167      117      110       99       99       98       96       87 
    ## englande     wyth   reigne   thomas    grace    maner    godly    added 
    ##       74       71       69       65       62       60       57       57 
    ##    euery      hys    actes gathered 
    ##       56       56       55       54 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-36.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-37.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-38.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-39.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-40.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-41.png)

    ## 
    ##  1551-1560 
    ## 
    ## 50 top features/tokens
    ##         god       lorde        yere       quene       moste    englande 
    ##         134          99          93          68          67          67 
    ##       grace     termino      reigne         set     fraunce       actes 
    ##          66          62          60          59          58          58 
    ##      quenes      holden     england westminster     henrici   continued 
    ##          57          52          51          50          50          49 
    ##        boke    foloweth 
    ##          48          48 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-42.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-43.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-44.png)

    ## 
    ##  1556-1565 
    ## 
    ## 50 top features/tokens
    ##       god    quenes     quene     lorde      yere  maiestie   termino       set 
    ##       111        94        87        74        74        74        73        71 
    ##   henrici     regis    realme   england maiesties     newly    london     regni 
    ##        58        55        48        47        46        44        43        43 
    ##   fraunce   edwardi  englishe      iohn 
    ##        41        41        39        39 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-45.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-46.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-47.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-48.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-49.png)

    ## 
    ##  1561-1570 
    ## 
    ## 50 top features/tokens
    ##       god       set  maiestie   queenes      yere   termino    queene   henrici 
    ##       110       108        85        82        81        75        70        68 
    ##      iohn    realme    quenes  englishe     forth   english maiesties     regis 
    ##        67        64        61        61        58        58        56        56 
    ##   allowed    london     lorde     order 
    ##        55        55        53        50 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-50.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-51.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-52.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-53.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-54.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-55.png)

    ## 
    ##  1566-1575 
    ## 
    ## 50 top features/tokens
    ##        god        set     queene    queenes    termino    english    henrici 
    ##        180        104         99         96         88         82         81 
    ##       iohn       yere  according   englishe      order     thomas    allowed 
    ##         71         71         67         66         66         65         64 
    ##    fraunce     realme parliament      regis      seene      grace 
    ##         63         63         61         60         60         59 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-56.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-57.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-58.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-59.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-60.png)

    ## 
    ##  1571-1580 
    ## 
    ## 50 top features/tokens
    ##        god    english       iohn        set     queene  christian profitable 
    ##        217        136        136        114         98         94         91 
    ##    queenes      booke     french    learned  discourse   gathered    fraunce 
    ##         84         84         83         79         78         77         75 
    ##  necessary  according  excellent    england   englishe     church 
    ##         73         73         73         73         73         72 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-61.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-62.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-63.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-64.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-65.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-66.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-67.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-68.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-69.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-70.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-71.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-72.png)

    ## 
    ##  1576-1585 
    ## 
    ## 50 top features/tokens
    ##        god       iohn    english       true        set      booke  christian 
    ##        235        206        202        148        142        129        129 
    ## profitable     church     french      godly    learned  discourse      right 
    ##        126        120        119        116        115        113        106 
    ##   treatise     christ  excellent     sermon      great     diuers 
    ##        102         99         99         95         94         94 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-73.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-74.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-75.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-76.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-77.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-78.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-79.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-80.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-81.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-82.png)

    ## 
    ##  1581-1590 
    ## 
    ## 50 top features/tokens
    ##       god   english      true      iohn    french       set christian     booke 
    ##       260       234       194       190       161       147       129       127 
    ##    church     great maiesties     right discourse    sermon     godly  treatise 
    ##       125       123       122       120       117       110       109       109 
    ##   england   queenes  maiestie    london 
    ##       105       105       104       104 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-83.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-84.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-85.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-86.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-87.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-88.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-89.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-90.png)

    ## 
    ##  1586-1595 
    ## 
    ## 50 top features/tokens
    ##   english       god    french      true      king   england     great       set 
    ##       233       222       210       169       143       139       138       132 
    ##      lord      iohn maiesties    queene    diuers     right      last  certaine 
    ##       131       126       121       117       114       114       113       107 
    ##    church  maiestie     booke  treatise 
    ##       105       105       104       103 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-91.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-92.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-93.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-94.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-95.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-96.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-97.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-98.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-99.png)

    ## 
    ##  1591-1600 
    ## 
    ## 50 top features/tokens
    ##    english        god       true       king     french       lord        set 
    ##        212        172        162        158        158        150        135 
    ##      booke       iohn      great     diuers       last   certaine    england 
    ##        131        121        115        115        114        109        104 
    ## containing  maiesties     thomas     london  christian      death 
    ##        103        100         99         95         94         94 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-100.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-101.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-102.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-103.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-104.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-105.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-106.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-107.png)

    ## 
    ##  1596-1605 
    ## 
    ## 50 top features/tokens
    ##       king    english       true        god    england       lord       iohn 
    ##        301        248        227        222        201        197        155 
    ##     london        set  maiesties     french      great   certaine     thomas 
    ##        152        151        143        141        137        137        135 
    ##      booke     diuers   preached containing   maiestie  christian 
    ##        128        127        118        116        114        108 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-108.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-109.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-110.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-111.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-112.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-113.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-114.png)

    ## 
    ##  1601-1610 
    ## 
    ## 50 top features/tokens
    ##      king       god   england      true  preached      lord     great      iohn 
    ##       392       332       282       260       242       242       220       213 
    ##   english    church     kings    sermon maiesties    london       set  maiestie 
    ##       210       206       188       185       184       179       177       165 
    ##    thomas christian      word      last 
    ##       164       162       159       152 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-115.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-116.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-117.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-118.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-119.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-120.png)

    ## 
    ##  1606-1615 
    ## 
    ## 50 top features/tokens
    ##      king       god  preached     great      iohn      lord      true    sermon 
    ##       356       350       326       320       307       269       266       249 
    ##    church      word christian      last    thomas       new   english   england 
    ##       248       246       241       237       224       222       218       209 
    ##  together maiesties      gods     kings 
    ##       208       208       208       192 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-121.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-122.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-123.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-124.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-125.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-126.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-127.png)

    ## 
    ##  1611-1620 
    ## 
    ## 50 top features/tokens
    ##        god   preached       king      great       word       iohn     sermon 
    ##        381        363        349        324        317        313        301 
    ##       lord       gods        new  maiesties      death     london       true 
    ##        297        271        255        247        242        242        240 
    ##  christian     thomas     church   together   preacher containing 
    ##        236        230        228        224        220        201 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-128.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-129.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-130.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-131.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-132.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-133.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-134.png)

    ## 
    ##  1616-1625 
    ## 
    ## 50 top features/tokens
    ##       king        god      great        new   preached       word       iohn 
    ##        574        530        485        401        398        385        382 
    ##     sermon       true       lord       last       gods     london   together 
    ##        364        356        351        331        321        320        310 
    ##     church   preacher    english  maiesties containing     diuers 
    ##        300        288        270        268        257        255 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-135.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-136.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-137.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-138.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-139.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-140.png)

    ## 
    ##  1621-1630 
    ## 
    ## 50 top features/tokens
    ##         king          god          new        great         iohn         lord 
    ##          780          547          521          498          435          390 
    ##     preached         true       sermon       london       church         last 
    ##          385          370          367          350          341          317 
    ##        newes proclamation     together         word       diuers      english 
    ##          309          303          300          292          287          287 
    ##         duke      england 
    ##          273          270 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-141.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-142.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-143.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-144.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-145.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-146.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-147.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-148.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-149.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-150.png)

    ## 
    ##  1626-1635 
    ## 
    ## 50 top features/tokens
    ##         king          new          god        great         iohn         lord 
    ##          781          430          362          362          348          329 
    ##     preached proclamation       church       london       sermon     together 
    ##          289          271          266          264          244          241 
    ##      english    maiesties   containing         true         gods       sweden 
    ##          238          229          226          219          219          217 
    ##      present   concerning 
    ##          210          209 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-151.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-152.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-153.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-154.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-155.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-156.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-157.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-158.png)

    ## 
    ##  1631-1640 
    ## 
    ## 50 top features/tokens
    ##         king          new          god         lord        great         iohn 
    ##          515          410          391          353          339          328 
    ##       london     preached      english      sermons     together         gods 
    ##          308          295          282          243          242          235 
    ##   containing       church proclamation       sermon         true     preacher 
    ##          232          231          229          226          220          215 
    ##         good   concerning 
    ##          214          208 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-159.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-160.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-161.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-162.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-163.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-164.png)

    ## 
    ##  1636-1645 
    ## 
    ## 50 top features/tokens
    ##  parliament     commons       lords       house  concerning        lord 
    ##        4229        2788        2029        1690        1590        1488 
    ##   majesties   assembled      london        1642         sir        true 
    ##        1320        1309        1235        1209        1190        1166 
    ##      houses     ordered        sent      letter       great        cler 
    ##        1078        1070        1062        1044        1042        1018 
    ## declaration    together 
    ##         998         950 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-165.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-166.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-167.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-168.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-169.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-170.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-171.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-172.png)

    ## 
    ##  1641-1650 
    ## 
    ## 50 top features/tokens
    ##  parliament     commons       lords  concerning   assembled       house 
    ##        6516        3905        2701        2526        2206        2147 
    ##        lord      london         sir     england     ordered   majesties 
    ##        2113        1817        1765        1732        1607        1586 
    ##      letter declaration    together       great        true        sent 
    ##        1568        1525        1423        1399        1383        1369 
    ##      houses        cler 
    ##        1355        1321 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-173.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-174.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-175.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-176.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-177.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-178.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-179.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-180.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-181.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-182.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-183.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-184.png)

    ## 
    ##  1646-1655 
    ## 
    ## 50 top features/tokens
    ##  parliament     england        lord  concerning      london     commons 
    ##        2688        1458        1458        1347        1197        1132 
    ##        john      christ         god    together       great         act 
    ##        1110        1094        1089        1062        1059         921 
    ##   assembled       lords         new        king        army    severall 
    ##         918         866         817         785         729         709 
    ## declaration     ordered 
    ##         689         688 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-185.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-186.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-187.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-188.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-189.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-190.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-191.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-192.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-193.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-194.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-195.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-196.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-197.png)

    ## 
    ##  1651-1660 
    ## 
    ## 50 top features/tokens
    ##       lord        god     christ parliament       john    england      great 
    ##       1544       1486       1341       1250       1242       1218       1011 
    ##   together     london       true     church     people       king   minister 
    ##       1007        979        931        902        846        830        815 
    ##        new    english    several     gospel concerning     sermon 
    ##        787        786        778        748        745        653 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-198.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-199.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-200.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-201.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-202.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-203.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-204.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-205.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-206.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-207.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-208.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-209.png)

    ## 
    ##  1656-1665 
    ## 
    ## 50 top features/tokens
    ##        god       lord parliament       john       king    england     christ 
    ##       1139       1115        981        820        817        792        739 
    ##       true     london      great     church     people    several   together 
    ##        702        697        669        668        656        635        616 
    ##        new concerning     called    english   minister     sermon 
    ##        538        501        488        465        458        457 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-210.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-211.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-212.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-213.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-214.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-215.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-216.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-217.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-218.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-219.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-220.png)

    ## 
    ##  1661-1670 
    ## 
    ## 50 top features/tokens
    ##       king       lord        new       john        god    several     london 
    ##        497        476        445        445        431        420        418 
    ##      great  majesties    england       true    english     church   together 
    ##        381        357        355        348        331        312        302 
    ## concerning     sermon   preached    william       life    majesty 
    ##        264        262        255        243        228        227 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-221.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-222.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-223.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-224.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-225.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-226.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-227.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-228.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-229.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-230.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-231.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-232.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-233.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-234.png)

    ## 
    ##  1666-1675 
    ## 
    ## 50 top features/tokens
    ##       new   several      john    london      true      love      lord  together 
    ##       686       561       513       488       469       458       455       423 
    ##       god      king   english     great   england majesties   william  preached 
    ##       417       415       414       404       401       369       320       300 
    ##    sermon      life       use    church 
    ##       292       285       283       280 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-235.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-236.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-237.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-238.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-239.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-240.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-241.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-242.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-243.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-244.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-245.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-246.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-247.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-248.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-249.png)

    ## 
    ##  1671-1680 
    ## 
    ## 50 top features/tokens
    ##   several       new      john      true    london   england      lord   account 
    ##       796       726       692       687       656       634       597       537 
    ##      king  together   english     great    church    sermon   william       god 
    ##       516       514       514       508       505       472       471       455 
    ## majesties      love  preached      life 
    ##       454       449       446       398 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-250.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-251.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-252.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-253.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-254.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-255.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-256.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-257.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-258.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-259.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-260.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-261.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-262.png)

    ## 
    ##  1676-1685 
    ## 
    ## 50 top features/tokens
    ##   several      true    london   account       new   england      john      king 
    ##      1115      1088      1084      1080      1022      1010       986       959 
    ##      lord    church    sermon  together  preached     great     death   william 
    ##       944       868       846       740       726       723       657       641 
    ##   english majesties       god      life 
    ##       630       605       595       587 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-263.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-264.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-265.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-266.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-267.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-268.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-269.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-270.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-271.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-272.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-273.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-274.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-275.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-276.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-277.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-278.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-279.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-280.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-281.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-282.png)

    ## 
    ##  1681-1690 
    ## 
    ## 50 top features/tokens
    ##      king   account   england       new      true    church   several    london 
    ##      1375      1372      1265      1154      1098      1092      1083      1021 
    ##      lord      john    sermon  together majesties     order according     great 
    ##       975       887       849       847       837       836       832       823 
    ##   english    letter  licensed   present 
    ##       781       763       762       710 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-283.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-284.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-285.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-286.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-287.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-288.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-289.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-290.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-291.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-292.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-293.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-294.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-295.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-296.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-297.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-298.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-299.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-300.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-301.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-302.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-303.png)

    ## 
    ##  1686-1695 
    ## 
    ## 50 top features/tokens
    ##      king   account   england  licensed majesties       new   several     order 
    ##      1144      1108      1029       974       932       902       885       881 
    ## according      lord    church    sermon   english    london    french      true 
    ##       879       799       780       739       731       723       689       684 
    ##      john     great    letter   present 
    ##       681       671       665       618 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-304.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-305.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-306.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-307.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-308.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-309.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-310.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-311.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-312.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-313.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-314.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-315.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-316.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-317.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-318.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-319.png)

    ## 
    ##  1691-1700 
    ## 
    ## 50 top features/tokens
    ##    several    account     sermon       john        new    england       king 
    ##       1118       1028        949        930        906        905        862 
    ##       lord    william     london    english     church        god containing 
    ##        763        740        733        731        674        650        635 
    ##  majesties     french      added      great concerning       true 
    ##        628        626        609        587        578        575 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-320.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-321.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-322.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-323.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-324.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-325.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-326.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-327.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-328.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-329.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-330.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-331.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-332.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-333.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-334.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-335.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-336.png)

    ## 
    ##  1696-1705 
    ## 
    ## 50 top features/tokens
    ##    several    england     sermon       john    account        new     church 
    ##       1214       1004        998        991        965        861        859 
    ##       king    english    william containing     london       lord    majesty 
    ##        806        770        763        685        661        656        629 
    ##      added    preachd parliament        god concerning     letter 
    ##        620        615        606        601        597        590 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-337.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-338.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-339.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-340.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-341.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-342.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-343.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-344.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-345.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-346.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-347.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-348.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-349.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-350.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-351.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-352.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-353.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-354.png)

    ## 
    ##  1701-1710 
    ## 
    ## 50 top features/tokens
    ##     sermon    several     church       john    preachd    england    account 
    ##       1356       1109       1090       1047       1044        998        953 
    ##       lord        new    english containing     letter    majesty      right 
    ##        795        782        734        690        662        653        653 
    ##    william       king        god      great     london      added 
    ##        652        616        604        604        595        573 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-355.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-356.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-357.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-358.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-359.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-360.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-361.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-362.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-363.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-364.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-365.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-366.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-367.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-368.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-369.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-370.png)

    ## 
    ##  1706-1715 
    ## 
    ## 50 top features/tokens
    ##     sermon    preachd     church    several    account       john       lord 
    ##       1678       1424       1238       1122       1115       1078       1039 
    ##    england     letter        new      right      great    majesty       king 
    ##        932        879        835        817        813        792        787 
    ##    english      added containing    present     london honourable 
    ##        771        758        682        680        651        648 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-371.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-372.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-373.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-374.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-375.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-376.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-377.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-378.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-379.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-380.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-381.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-382.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-383.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-384.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-385.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-386.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-387.png)

    ## 
    ##  1711-1720 
    ## 
    ## 50 top features/tokens
    ##     sermon     church    several    preachd       lord    account     letter 
    ##       1494       1411       1301       1247       1209       1208       1191 
    ##       john       king    england        new      right      added containing 
    ##       1077       1072        945        888        875        873        865 
    ##      great     bishop    english    history   reverend    present 
    ##        808        807        779        741        726        705 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-388.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-389.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-390.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-391.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-392.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-393.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-394.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-395.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-396.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-397.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-398.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-399.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-400.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-401.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-402.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-403.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-404.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-405.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-406.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-407.png)

    ## 
    ##  1716-1725 
    ## 
    ## 50 top features/tokens
    ##    several     sermon     church       lord       john    account containing 
    ##       1190       1095       1071       1043        995        969        906 
    ##     letter    preachd        new      added      right    england    english 
    ##        898        854        827        738        737        725        708 
    ##      great       king     bishop   reverend    history     london 
    ##        701        697        674        647        640        636 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-408.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-409.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-410.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-411.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-412.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-413.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-414.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-415.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-416.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-417.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-418.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-419.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-420.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-421.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-422.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-423.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-424.png)

    ## 
    ##  1721-1730 
    ## 
    ## 50 top features/tokens
    ##    several        new       john    account containing     sermon       lord 
    ##       1102        919        890        875        852        828        737 
    ##      added      great    english     church     london    history     letter 
    ##        736        710        690        664        634        633        592 
    ##      right    preachd   together        use      whole       king 
    ##        587        566        532        518        517        501 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-425.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-426.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-427.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-428.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-429.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-430.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-431.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-432.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-433.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-434.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-435.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-436.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-437.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-438.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-439.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-440.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-441.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-442.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-443.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-444.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-445.png)

    ## 
    ##  1726-1735 
    ## 
    ## 50 top features/tokens
    ##    several        new containing      added    account       john     letter 
    ##       1111        959        904        835        829        804        773 
    ##      great     sermon    history       lord     church     london    english 
    ##        753        701        657        618        600        570        568 
    ##    present      whole       king      right parliament       life 
    ##        523        503        503        499        494        470 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-446.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-447.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-448.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-449.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-450.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-451.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-452.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-453.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-454.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-455.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-456.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-457.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-458.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-459.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-460.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-461.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-462.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-463.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-464.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-465.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-466.png)

    ## 
    ##  1731-1740 
    ## 
    ## 50 top features/tokens
    ##    several containing        new    account       john     letter      added 
    ##       1015       1004        950        949        831        819        809 
    ##      great     sermon    history        act    english     church       lord 
    ##        774        741        666        659        646        602        592 
    ##    present      whole     london       life parliament    remarks 
    ##        589        576        548        521        518        517 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-467.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-468.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-469.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-470.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-471.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-472.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-473.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-474.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-475.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-476.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-477.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-478.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-479.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-480.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-481.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-482.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-483.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-484.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-485.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-486.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-487.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-488.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-489.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-490.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-491.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-492.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-493.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-494.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-495.png)

    ## 
    ##  1736-1745 
    ## 
    ## 50 top features/tokens
    ##    account containing    several       john        new     sermon    present 
    ##       1013        925        892        883        871        835        795 
    ##     letter        act      great      added     london    history      whole 
    ##        741        730        725        703        637        633        606 
    ##    english       lord       king     church   preached       life 
    ##        600        559        555        553        522        518 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-496.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-497.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-498.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-499.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-500.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-501.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-502.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-503.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-504.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-505.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-506.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-507.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-508.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-509.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-510.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-511.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-512.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-513.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-514.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-515.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-516.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-517.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-518.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-519.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-520.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-521.png)

    ## 
    ##  1741-1750 
    ## 
    ## 50 top features/tokens
    ##    account containing     sermon    several       john     letter    present 
    ##        978        968        921        904        861        856        815 
    ##        new        act      great      added   preached       lord     london 
    ##        798        712        675        674        663        663        631 
    ##     church    history      whole    general     thomas      right 
    ##        602        598        542        530        510        501 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-522.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-523.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-524.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-525.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-526.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-527.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-528.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-529.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-530.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-531.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-532.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-533.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-534.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-535.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-536.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-537.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-538.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-539.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-540.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-541.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-542.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-543.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-544.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-545.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-546.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-547.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-548.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-549.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-550.png)

    ## 
    ##  1746-1755 
    ## 
    ## 50 top features/tokens
    ## containing    account        new    several       john     sermon     letter 
    ##       1055       1042        999        948        904        870        848 
    ##       lord        act      added    present   preached     london      great 
    ##        750        723        717        686        682        680        663 
    ##    history     church        use      whole parliament     thomas 
    ##        651        626        544        539        535        523 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-551.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-552.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-553.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-554.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-555.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-556.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-557.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-558.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-559.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-560.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-561.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-562.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-563.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-564.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-565.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-566.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-567.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-568.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-569.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-570.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-571.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-572.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-573.png)

    ## 
    ##  1751-1760 
    ## 
    ## 50 top features/tokens
    ##        new    account containing       john    several     letter        act 
    ##       1090       1057       1047       1002        983        868        864 
    ##     sermon      added   preached      great       lord    history    present 
    ##        861        802        775        763        728        722        718 
    ##     london    english    general       life    england parliament 
    ##        641        593        557        547        547        544 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-574.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-575.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-576.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-577.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-578.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-579.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-580.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-581.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-582.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-583.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-584.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-585.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-586.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-587.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-588.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-589.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-590.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-591.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-592.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-593.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-594.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-595.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-596.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-597.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-598.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-599.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-600.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-601.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-602.png)

    ## 
    ## Community: 30

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-603.png)

    ## 
    ## Community: 31

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-604.png)

    ## 
    ## Community: 32

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-605.png)

    ## 
    ## Community: 33

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-606.png)

    ## 
    ## Community: 34

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-607.png)

    ## 
    ## Community: 35

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-608.png)

    ## 
    ##  1756-1765 
    ## 
    ## 50 top features/tokens
    ##        new containing    several       john    account        act     letter 
    ##       1172       1043        922        889        849        819        717 
    ##     sermon      great   preached      added    present    history       lord 
    ##        717        711        701        683        682        640        638 
    ##    english     london    general parliament      whole    england 
    ##        610        566        558        537        527        521 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-609.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-610.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-611.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-612.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-613.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-614.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-615.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-616.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-617.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-618.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-619.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-620.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-621.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-622.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-623.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-624.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-625.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-626.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-627.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-628.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-629.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-630.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-631.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-632.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-633.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-634.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-635.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-636.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-637.png)

    ## 
    ##  1761-1770 
    ## 
    ## 50 top features/tokens
    ##        new containing    several        act       john    account    history 
    ##       1408       1216        883        875        872        795        672 
    ##    present      added parliament      great     london     letter    england 
    ##        667        658        656        640        637        632        624 
    ##    english      royal     sermon       lord   preached    general 
    ##        605        595        585        575        561        521 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-638.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-639.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-640.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-641.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-642.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-643.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-644.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-645.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-646.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-647.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-648.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-649.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-650.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-651.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-652.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-653.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-654.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-655.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-656.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-657.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-658.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-659.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-660.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-661.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-662.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-663.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-664.png)

    ## 
    ##  1766-1775 
    ## 
    ## 50 top features/tokens
    ##        new containing        act       john    several    account parliament 
    ##       1545       1280       1108       1009        891        835        798 
    ##      added    england    history     public    present      great     london 
    ##        796        781        779        754        744        743        707 
    ##    english     letter      state      royal    letters   complete 
    ##        682        636        632        613        607        592 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-665.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-666.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-667.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-668.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-669.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-670.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-671.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-672.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-673.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-674.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-675.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-676.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-677.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-678.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-679.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-680.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-681.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-682.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-683.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-684.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-685.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-686.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-687.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-688.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-689.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-690.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-691.png)

    ## 
    ## Community: 28

    ## Warning: Removed 20 rows containing missing values (geom_segment).

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-692.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-693.png)

    ## 
    ##  1771-1780 
    ## 
    ## 50 top features/tokens
    ##        new containing        act       john parliament      added    present 
    ##       1713       1391       1256        983        851        835        804 
    ##    english      great     london     public    account    several    history 
    ##        786        785        783        764        762        760        729 
    ##   complete    general    england       lord     letter    william 
    ##        654        649        642        638        632        615 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-694.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-695.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-696.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-697.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-698.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-699.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-700.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-701.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-702.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-703.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-704.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-705.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-706.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-707.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-708.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-709.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-710.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-711.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-712.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-713.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-714.png)

    ## 
    ##  1776-1785 
    ## 
    ## 50 top features/tokens
    ##          new          act   containing         john   parliament       london 
    ##         1597         1322         1306          980          939          927 
    ##      present       public        great        added      account     complete 
    ##          900          844          825          804          788          747 
    ##        royal        state      several      general      england      history 
    ##          740          732          718          709          702          700 
    ##         lord observations 
    ##          696          679 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-715.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-716.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-717.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-718.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-719.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-720.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-721.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-722.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-723.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-724.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-725.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-726.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-727.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-728.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-729.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-730.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-731.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-732.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-733.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-734.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-735.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-736.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-737.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-738.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-739.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-740.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-741.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-742.png)

    ## 
    ##  1781-1790 
    ## 
    ## 50 top features/tokens
    ##          new          act   containing         john       london      present 
    ##         1534         1407         1407         1184         1090         1045 
    ##        royal   parliament        added      account        great          rev 
    ##          974          959          952          904          894          859 
    ##      several       public        state      history      england     complete 
    ##          844          823          804          803          799          764 
    ## observations      william 
    ##          753          752 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-743.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-744.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-745.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-746.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-747.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-748.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-749.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-750.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-751.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-752.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-753.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-754.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-755.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-756.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-757.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-758.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-759.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-760.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-761.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-762.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-763.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-764.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-765.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-766.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-767.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-768.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-769.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-770.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-771.png)

    ## 
    ## Community: 30

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-772.png)

    ## 
    ##  1786-1795 
    ## 
    ## 50 top features/tokens
    ##          new   containing          act         john      present          rev 
    ##         1854         1616         1611         1543         1233         1191 
    ##       london        added        royal        great      history      general 
    ##         1177         1176         1096         1094         1015         1011 
    ##      account       letter      william       sermon      england   parliament 
    ##         1005          959          956          911          899          886 
    ##     preached observations 
    ##          864          842 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-773.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-774.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-775.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-776.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-777.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-778.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-779.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-780.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-781.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-782.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-783.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-784.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-785.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-786.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-787.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-788.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-789.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-790.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-791.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-792.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-793.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-794.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-795.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-796.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-797.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-798.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-799.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-800.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-801.png)

    ## 
    ## Community: 30

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-802.png)

    ## 
    ## Community: 31

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-803.png)

    ## 
    ## Community: 32

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-804.png)

    ## 
    ## Community: 33

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-805.png)

    ## 
    ## Community: 34

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-806.png)

    ## 
    ##  1791-1800 
    ## 
    ## 50 top features/tokens
    ##        act        new       john containing        rev    present     london 
    ##       2143       2127       1892       1704       1531       1499       1363 
    ##    general      great     french    history    william      added      royal 
    ##       1327       1306       1273       1243       1240       1223       1155 
    ##    account     sermon     letter    england   preached     thomas 
    ##       1148       1136       1082       1029       1021       1020 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-807.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-808.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-809.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-810.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-811.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-812.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-813.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-814.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-815.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-816.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-817.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-818.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-819.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-820.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-821.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-822.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-823.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-824.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-825.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-826.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-827.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-828.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-829.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-830.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-831.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-832.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-833.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-834.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-835.png)

    ## 
    ## Community: 30

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-836.png)

    ## 
    ## Community: 31

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-837.png)

    ## 
    ## Community: 32

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-838.png)

    ## 
    ## Community: 33

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-839.png)

    ## 
    ## Community: 34

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-840.png)

    ## 
    ##  1796-1805 
    ## 
    ## 50 top features/tokens
    ##        act        new       john containing    present        rev     london 
    ##       1237        992        914        793        782        742        685 
    ##      great    william     french      added    general    history      royal 
    ##        680        678        677        627        626        621        602 
    ##    account parliament      right     sermon       1799       1797 
    ##        590        557        542        537        532        524 
    ## 
    ## Community: 1

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-841.png)

    ## 
    ## Community: 2

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-842.png)

    ## 
    ## Community: 3

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-843.png)

    ## 
    ## Community: 4

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-844.png)

    ## 
    ## Community: 5

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-845.png)

    ## 
    ## Community: 6

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-846.png)

    ## 
    ## Community: 7

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-847.png)

    ## 
    ## Community: 8

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-848.png)

    ## 
    ## Community: 9

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-849.png)

    ## 
    ## Community: 10

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-850.png)

    ## 
    ## Community: 11

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-851.png)

    ## 
    ## Community: 12

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-852.png)

    ## 
    ## Community: 13

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-853.png)

    ## 
    ## Community: 14

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-854.png)

    ## 
    ## Community: 15

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-855.png)

    ## 
    ## Community: 16

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-856.png)

    ## 
    ## Community: 17

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-857.png)

    ## 
    ## Community: 18

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-858.png)

    ## 
    ## Community: 19

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-859.png)

    ## 
    ## Community: 20

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-860.png)

    ## 
    ## Community: 21

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-861.png)

    ## 
    ## Community: 22

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-862.png)

    ## 
    ## Community: 23

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-863.png)

    ## 
    ## Community: 24

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-864.png)

    ## 
    ## Community: 25

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-865.png)

    ## 
    ## Community: 26

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-866.png)

    ## 
    ## Community: 27

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-867.png)

    ## 
    ## Community: 28

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-868.png)

    ## 
    ## Community: 29

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-869.png)

    ## 
    ## Community: 30

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-870.png)

    ## 
    ## Community: 31

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-871.png)

    ## 
    ## Community: 32

![](WB_Top_features_and_keyness_by_era_files/figure-markdown_github/unnamed-chunk-2-872.png)
