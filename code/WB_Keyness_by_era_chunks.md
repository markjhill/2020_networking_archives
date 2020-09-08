Keyness by era
================

Prepare data
============

``` r
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

#stop words
latin_stopwords <- read.csv("../../ESTC_SNA_data_creation/data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
                     "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
                     "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
                     #THIS IS IMPORTANT!
                     "non-latin", "alphabet", "saith", "thus", "|", "¦", "⁻", "vol", "year", "printed", "published",
                     "na", "amp", "ye", "anno", "translated", "vpon", "vnto", "yeere", "haue", "three", "second", "thereof", "yeare", "written", "mr",
                     "wherein", "written", "esq", "author", "part", "dr", "volumes", "1800", "volume", "mrs", "m.d", "d.d", "b.d")


#Get keyness per era, export png of each community

total_dfm <- readRDS("../../ESTC_SNA_data_creation/data/work/netowrking_archives/total_era_dfm.rds")
total_dfm <- dfm_select(total_dfm, pattern = c(stopwords("english"), stopwords("french"), stopwords("italian"), stopwords("spanish"), latin_stopwords$a, extra_stopwords), selection = "remove", valuetype = "fixed")
total_dfm <- dfm_select(total_dfm, pattern = c("\\d\\d\\d\\d\\d", "\\d\\d\\d\\d", "\\d\\d\\d", "\\d\\d", "\\d"), selection = "remove", valuetype = "regex")

eras <- unique(total_dfm@docvars$Era)
```

Print plots
===========

``` r
for(i_era in 1:length(eras)) {
  cat("\n", eras[i_era], "\n")
  tstat_key <- textstat_keyness(total_dfm, 
                                target = total_dfm@docvars$Era == paste0(eras[i_era]))
  print(textplot_keyness(tstat_key, n = 20))
}
```

    ## 
    ##  1501-1510

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-1.png)

    ## 
    ##  1506-1515

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-2.png)

    ## 
    ##  1511-1520

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-3.png)

    ## 
    ##  1516-1525

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-4.png)

    ## 
    ##  1521-1530

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-5.png)

    ## 
    ##  1526-1535

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-6.png)

    ## 
    ##  1531-1540

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-7.png)

    ## 
    ##  1536-1545

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-8.png)

    ## 
    ##  1541-1550

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-9.png)

    ## 
    ##  1546-1555

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-10.png)

    ## 
    ##  1551-1560

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-11.png)

    ## 
    ##  1556-1565

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-12.png)

    ## 
    ##  1561-1570

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-13.png)

    ## 
    ##  1566-1575

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-14.png)

    ## 
    ##  1571-1580

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-15.png)

    ## 
    ##  1576-1585

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-16.png)

    ## 
    ##  1581-1590

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-17.png)

    ## 
    ##  1586-1595

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-18.png)

    ## 
    ##  1591-1600

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-19.png)

    ## 
    ##  1596-1605

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-20.png)

    ## 
    ##  1601-1610

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-21.png)

    ## 
    ##  1606-1615

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-22.png)

    ## 
    ##  1611-1620

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-23.png)

    ## 
    ##  1616-1625

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-24.png)

    ## 
    ##  1621-1630

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-25.png)

    ## 
    ##  1626-1635

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-26.png)

    ## 
    ##  1631-1640

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-27.png)

    ## 
    ##  1636-1645

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-28.png)

    ## 
    ##  1641-1650

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-29.png)

    ## 
    ##  1646-1655

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-30.png)

    ## 
    ##  1651-1660

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-31.png)

    ## 
    ##  1656-1665

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-32.png)

    ## 
    ##  1661-1670

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-33.png)

    ## 
    ##  1666-1675

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-34.png)

    ## 
    ##  1671-1680

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-35.png)

    ## 
    ##  1676-1685

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-36.png)

    ## 
    ##  1681-1690

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-37.png)

    ## 
    ##  1686-1695

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-38.png)

    ## 
    ##  1691-1700

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-39.png)

    ## 
    ##  1696-1705

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-40.png)

    ## 
    ##  1701-1710

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-41.png)

    ## 
    ##  1706-1715

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-42.png)

    ## 
    ##  1711-1720

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-43.png)

    ## 
    ##  1716-1725

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-44.png)

    ## 
    ##  1721-1730

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-45.png)

    ## 
    ##  1726-1735

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-46.png)

    ## 
    ##  1731-1740

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-47.png)

    ## 
    ##  1736-1745

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-48.png)

    ## 
    ##  1741-1750

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-49.png)

    ## 
    ##  1746-1755

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-50.png)

    ## 
    ##  1751-1760

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-51.png)

    ## 
    ##  1756-1765

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-52.png)

    ## 
    ##  1761-1770

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-53.png)

    ## 
    ##  1766-1775

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-54.png)

    ## 
    ##  1771-1780

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-55.png)

    ## 
    ##  1776-1785

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-56.png)

    ## 
    ##  1781-1790

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-57.png)

    ## 
    ##  1786-1795

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-58.png)

    ## 
    ##  1791-1800

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-59.png)

    ## 
    ##  1796-1805

![](WB_Keyness_by_era_chunks_files/figure-markdown_github/unnamed-chunk-2-60.png)
