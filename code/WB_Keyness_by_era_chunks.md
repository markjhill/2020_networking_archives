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
print(topfeatures(total_dfm, groups = "Era", n = 25))
```

    ## $`1501-1510`
    ##  begynneth      sarum   treatyse    henrici    termino       boke      lorde 
    ##         26         13         13         12         12         11         11 
    ##        god      kynge     anglie   ecclesie      henry    incipit       vsum 
    ##         10         10          9          9          9          8          8 
    ##     lytell     sancte     called      regis       holy    edwardi       yere 
    ##          8          7          7          6          6          6          6 
    ##  souerayne     diuina     pasche trinitatis 
    ##          6          5          5          5 
    ## 
    ## $`1506-1515`
    ##      lorde  begynneth      kynge        god    henrici      henry     anglie 
    ##         28         26         26         25         21         18         15 
    ##      regis    termino       holy      regni       boke     sancti       yere 
    ##         13         12         11         11         10          9          9 
    ##   treatyse      noble parlyament    statuta  folowynge      saynt  souerayne 
    ##          9          9          9          8          8          8          8 
    ##     lytell  michaelis   certayne soueraygne 
    ##          8          8          8          8 
    ## 
    ## $`1511-1520`
    ##          lorde          regis          regni            god          kynge 
    ##             30             26             23             22             21 
    ##        termino         anglie      begynneth        roberti         sancti 
    ##             20             19             19             17             16 
    ##           holy          henry        henrici        edwardi           yere 
    ##             16             15             12             12             12 
    ##       magistri     parlyament     conquestum    grammatices lichfeldiensis 
    ##             12             12             12             11             11 
    ##       laureati    whittintoni      folowynge        present       certayne 
    ##             11             11             10             10             10 
    ## 
    ## $`1516-1525`
    ##      begynneth        termino        roberti          regis           holy 
    ##             32             23             22             21             21 
    ##          lorde        edwardi            god          regni         anglie 
    ##             21             18             17             16             13 
    ##           boke           yere       magistri lichfeldiensis         sancti 
    ##             12             12             12             12             11 
    ##           kyng    grammatices          henry     parlyament     conquestum 
    ##             11             11             10             10             10 
    ##        henrici       treatyse          saynt       libellus          noble 
    ##              9              9              9              9              9 
    ## 
    ## $`1521-1530`
    ##     termino        boke   begynneth   michaelis     henrici         god 
    ##          59          40          34          31          25          22 
    ##      whiche       lorde      pasche   englysshe     roberti    treatyse 
    ##          22          19          18          18          18          16 
    ##     edwardi        kyng    treateth     dyaloge       regis       euery 
    ##          15          14          14          14          13          13 
    ##       sexti       maner        holy       moost vvhitintoni       regni 
    ##          13          13          12          12          12          11 
    ##      called 
    ##          11 
    ## 
    ## $`1526-1535`
    ##   termino      boke   henrici     lorde      yere    whiche       god     kynge 
    ##        65        55        47        45        40        39        38        35 
    ## begynneth michaelis englysshe     henry     maner    called     regis       man 
    ##        34        34        33        33        32        27        26        25 
    ##  treatyse     regni     moste      good      holy     theyr  statutes  treatise 
    ##        24        24        23        22        21        21        20        20 
    ##    holden 
    ##        20 
    ## 
    ## $`1531-1540`
    ##       yere      henry      lorde      kynge    henrici        god       boke 
    ##         73         71         70         65         54         52         45 
    ##      moste     holden     kynges       holy     called   englyshe   statutes 
    ##         43         43         39         36         31         30         28 
    ##       good parlyament  englysshe     reygne     realme    termino     whiche 
    ##         28         27         25         25         25         24         23 
    ##        man priuilegio    present     dyuers 
    ##         23         23         22         21 
    ## 
    ## $`1536-1545`
    ##       yere     kynges      henry    henrici      lorde        god       boke 
    ##         97         70         68         65         65         64         55 
    ##   englande      kynge     reygne      moste    termino   statutes     holden 
    ##         54         49         47         45         40         38         37 
    ##      newly   christen parlyament     realme        man        set      euery 
    ##         35         35         34         34         32         32         31 
    ##   englyshe      actes    churche      maner 
    ##         30         30         29         28 
    ## 
    ## $`1541-1550`
    ##    kynges       god      yere      boke     lorde       set     moste   churche 
    ##       125       119       104       103        89        83        69        69 
    ##  englande     henry       man  christen     euery   henrici     newly     maner 
    ##        67        64        62        59        57        54        54        50 
    ## necessary      wyth   termino     godly     forth      holy      kyng     added 
    ##        49        49        48        48        47        46        46        45 
    ##    wherin 
    ##        45 
    ## 
    ## $`1546-1555`
    ##       god      yere     lorde      boke     moste    kynges       set   churche 
    ##       167       117       110        99        99        98        96        87 
    ##  englande      wyth    reigne    thomas     grace     maner     added     godly 
    ##        74        71        69        65        62        60        57        57 
    ##     euery       hys     actes       man  gathered   england      daye     booke 
    ##        56        56        55        54        54        53        52        51 
    ## necessary 
    ##        50 
    ## 
    ## $`1551-1560`
    ##         god       lorde        yere       quene       moste    englande 
    ##         134          99          93          68          67          67 
    ##       grace     termino      reigne         set     fraunce       actes 
    ##          66          62          60          59          58          58 
    ##      quenes      holden     england     henrici westminster   continued 
    ##          57          52          51          50          50          49 
    ##        boke    foloweth     churche    irelande        daye    maiestie 
    ##          48          48          47          47          46          46 
    ##       regis 
    ##          45 
    ## 
    ## $`1556-1565`
    ##         god      quenes       quene        yere       lorde    maiestie 
    ##         111          94          87          74          74          74 
    ##     termino         set     henrici       regis      realme     england 
    ##          73          71          58          55          48          47 
    ##   maiesties       newly       regni      london     edwardi     fraunce 
    ##          46          44          43          43          41          41 
    ##      holden       booke    englishe        iohn       moste        good 
    ##          39          39          39          39          38          37 
    ## westminster 
    ##          37 
    ## 
    ## $`1561-1570`
    ##         god         set    maiestie     queenes        yere     termino 
    ##         110         108          85          82          81          75 
    ##      queene     henrici        iohn      realme    englishe      quenes 
    ##          70          68          67          64          61          61 
    ##     english       forth       regis   maiesties      london     allowed 
    ##          58          58          56          56          55          55 
    ##       lorde       order   according       quene iniunctions       regni 
    ##          53          50          49          47          47          45 
    ##      church 
    ##          45 
    ## 
    ## $`1566-1575`
    ##        god        set     queene    queenes    termino    english    henrici 
    ##        180        104         99         96         88         82         81 
    ##       yere       iohn  according      order   englishe     thomas    allowed 
    ##         71         71         67         66         66         65         64 
    ##    fraunce     realme parliament      regis      seene      grace    england 
    ##         63         63         61         60         60         59         57 
    ##      forth     holden soueraigne      lorde 
    ##         57         56         56         55 
    ## 
    ## $`1571-1580`
    ##        god    english       iohn        set     queene  christian profitable 
    ##        217        136        136        114         98         94         91 
    ##      booke    queenes     french    learned  discourse   gathered    fraunce 
    ##         84         84         83         79         78         77         75 
    ##  excellent  necessary    england   englishe  according      order     diuers 
    ##         73         73         73         73         73         72         72 
    ##     church      seene       true     thomas 
    ##         72         71         69         67 
    ## 
    ## $`1576-1585`
    ##        god       iohn    english       true        set      booke  christian 
    ##        235        206        202        148        142        129        129 
    ## profitable     church     french      godly    learned  discourse      right 
    ##        126        120        119        116        115        113        106 
    ##   treatise  excellent     christ     sermon      great     diuers     london 
    ##        102         99         99         95         94         94         92 
    ##     queene   gathered   preached     thomas 
    ##         90         89         89         86 
    ## 
    ## $`1581-1590`
    ##       god   english      true      iohn    french       set christian     booke 
    ##       260       234       194       190       161       147       129       127 
    ##    church     great maiesties     right discourse    sermon  treatise     godly 
    ##       125       123       122       120       117       110       109       109 
    ##   england   queenes    london  maiestie  preached      lord  certaine    queene 
    ##       105       105       104       104       103       102       101       100 
    ##    christ 
    ##        98 
    ## 
    ## $`1586-1595`
    ##    english        god     french       true       king    england      great 
    ##        233        222        210        169        143        139        138 
    ##        set       lord       iohn  maiesties     queene      right     diuers 
    ##        132        131        126        121        117        114        114 
    ##       last   certaine     church   maiestie      booke   treatise  christian 
    ##        113        107        105        105        104        103        103 
    ##    queenes       good     london profitable 
    ##        102         99         96         96 
    ## 
    ## $`1591-1600`
    ##    english        god       true       king     french       lord        set 
    ##        212        172        162        158        158        150        135 
    ##      booke       iohn      great     diuers       last   certaine    england 
    ##        131        121        115        115        114        109        104 
    ## containing  maiesties     thomas     london      death  christian      right 
    ##        103        100         99         95         94         94         93 
    ##  gentleman     christ  discourse     queene 
    ##         90         89         88         87 
    ## 
    ## $`1596-1605`
    ##       king    english       true        god    england       lord       iohn 
    ##        301        248        227        222        201        197        155 
    ##     london        set  maiesties     french      great   certaine     thomas 
    ##        152        151        143        141        137        137        135 
    ##      booke     diuers   preached containing   maiestie  christian      right 
    ##        128        127        118        116        114        108        106 
    ##     church      kings       time       last 
    ##        104        103        100         94 
    ## 
    ## $`1601-1610`
    ##      king       god   england      true      lord  preached     great      iohn 
    ##       392       332       282       260       242       242       220       213 
    ##   english    church     kings    sermon maiesties    london       set  maiestie 
    ##       210       206       188       185       184       179       177       165 
    ##    thomas christian      word      last   william  certaine diuinitie    french 
    ##       164       162       159       152       146       145       144       143 
    ##       new 
    ##       142 
    ## 
    ## $`1606-1615`
    ##       king        god   preached      great       iohn       lord       true 
    ##        356        350        326        320        307        269        266 
    ##     sermon     church       word  christian       last     thomas        new 
    ##        249        248        246        241        237        224        222 
    ##    english    england   together  maiesties       gods     london      kings 
    ##        218        209        208        208        208        192        192 
    ##    william containing  diuinitie        set 
    ##        185        182        177        176 
    ## 
    ## $`1611-1620`
    ##        god   preached       king      great       word       iohn     sermon 
    ##        381        363        349        324        317        313        301 
    ##       lord       gods        new  maiesties      death     london       true 
    ##        297        271        255        247        242        242        240 
    ##  christian     thomas     church   together   preacher containing       last 
    ##        236        230        228        224        220        201        195 
    ##    english    sermons   minister     prince 
    ##        193        183        177        171 
    ## 
    ## $`1616-1625`
    ##       king        god      great        new   preached       word       iohn 
    ##        574        530        485        401        398        385        382 
    ##     sermon       true       lord       last       gods     london   together 
    ##        364        356        351        331        321        320        310 
    ##     church   preacher    english  maiesties containing     diuers    present 
    ##        300        288        270        268        257        255        250 
    ##     prince       duke      newes  christian 
    ##        246        245        241        233 
    ## 
    ## $`1621-1630`
    ##         king          god          new        great         iohn         lord 
    ##          780          547          521          498          435          390 
    ##     preached         true       sermon       london       church         last 
    ##          385          370          367          350          341          317 
    ##        newes proclamation     together         word      english       diuers 
    ##          309          303          300          292          287          287 
    ##         duke      present      england   containing       france    maiesties 
    ##          273          270          270          269          265          260 
    ##         gods 
    ##          254 
    ## 
    ## $`1626-1635`
    ##         king          new          god        great         iohn         lord 
    ##          781          430          362          362          348          329 
    ##     preached proclamation       church       london       sermon     together 
    ##          289          271          266          264          244          241 
    ##      english    maiesties   containing         true         gods       sweden 
    ##          238          229          226          219          219          217 
    ##      present   concerning        since      sermons        parts         word 
    ##          210          209          205          204          199          198 
    ##       french 
    ##          195 
    ## 
    ## $`1631-1640`
    ##         king          new          god         lord        great         iohn 
    ##          515          410          391          353          339          328 
    ##       london     preached      english      sermons     together         gods 
    ##          308          295          282          243          242          235 
    ##   containing       church proclamation       sermon         true     preacher 
    ##          232          231          229          226          220          215 
    ##         good   concerning       christ    majesties     severall        parts 
    ##          214          208          202          200          197          192 
    ##         life 
    ##          191 
    ## 
    ## $`1636-1645`
    ##  parliament     commons       lords       house  concerning        lord 
    ##        4229        2788        2029        1690        1590        1488 
    ##   majesties   assembled      london         sir        true      houses 
    ##        1320        1309        1235        1190        1166        1078 
    ##     ordered        sent      letter       great        cler declaration 
    ##        1070        1062        1044        1042        1018         998 
    ##    together     england        king    relation  honourable    severall 
    ##         950         915         892         890         867         854 
    ##       order 
    ##         830 
    ## 
    ## $`1641-1650`
    ##  parliament     commons       lords  concerning   assembled       house 
    ##        6516        3905        2701        2526        2206        2147 
    ##        lord      london         sir     england     ordered   majesties 
    ##        2113        1817        1765        1732        1607        1586 
    ##      letter declaration    together       great        true        sent 
    ##        1568        1525        1423        1399        1383        1369 
    ##      houses        cler   forthwith       order    severall        king 
    ##        1355        1321        1281        1244        1238        1193 
    ##         die 
    ##        1161 
    ## 
    ## $`1646-1655`
    ##  parliament        lord     england  concerning      london     commons 
    ##        2688        1458        1458        1347        1197        1132 
    ##        john      christ         god    together       great         act 
    ##        1110        1094        1089        1062        1059         921 
    ##   assembled       lords         new        king        army    severall 
    ##         918         866         817         785         729         709 
    ## declaration     ordered        true      answer        city         sir 
    ##         689         688         687         685         670         666 
    ##      church 
    ##         665 
    ## 
    ## $`1651-1660`
    ##       lord        god     christ parliament       john    england      great 
    ##       1544       1486       1341       1250       1242       1218       1011 
    ##   together     london       true     church     people       king   minister 
    ##       1007        979        931        902        846        830        815 
    ##        new    english    several     gospel concerning     sermon      forth 
    ##        787        786        778        748        745        653        647 
    ##    william   preached     called     thomas 
    ##        646        644        614        593 
    ## 
    ## $`1656-1665`
    ##        god       lord parliament       john       king    england     christ 
    ##       1139       1115        981        820        817        792        739 
    ##       true     london      great     church     people    several   together 
    ##        702        697        669        668        656        635        616 
    ##        new concerning     called    english   minister     sermon       life 
    ##        538        501        488        465        458        457        453 
    ##      forth   preached     thomas    william 
    ##        452        444        438        430 
    ## 
    ## $`1661-1670`
    ##       king       lord        new       john        god    several     london 
    ##        497        476        445        445        431        420        418 
    ##      great  majesties    england       true    english     church   together 
    ##        381        357        355        348        331        312        302 
    ## concerning     sermon   preached    william       life    majesty        use 
    ##        264        262        255        243        228        227        222 
    ##    charles       love     christ      right 
    ##        218        216        213        208 
    ## 
    ## $`1666-1675`
    ##       new   several      john    london      true      love      lord  together 
    ##       686       561       513       488       469       458       455       423 
    ##       god      king   english     great   england majesties   william  preached 
    ##       417       415       414       404       401       369       320       300 
    ##    sermon      life       use    church     death discourse     young     added 
    ##       292       285       283       280       276       274       264       262 
    ##      good 
    ##       260 
    ## 
    ## $`1671-1680`
    ##   several       new      john      true    london   england      lord   account 
    ##       796       726       692       687       656       634       597       537 
    ##      king   english  together     great    church    sermon   william       god 
    ##       516       514       514       508       505       472       471       455 
    ## majesties      love  preached      life     death     right discourse    thomas 
    ##       454       449       446       398       380       365       352       345 
    ##     added 
    ##       345 
    ## 
    ## $`1676-1685`
    ##    several       true     london    account        new    england       john 
    ##       1115       1088       1084       1080       1022       1010        986 
    ##       king       lord     church     sermon   together   preached      great 
    ##        959        944        868        846        740        726        723 
    ##      death    william    english  majesties        god       life      right 
    ##        657        641        630        605        595        587        578 
    ## concerning    majesty     thomas    present 
    ##        561        559        547        524 
    ## 
    ## $`1681-1690`
    ##       king    account    england        new       true     church    several 
    ##       1375       1372       1265       1154       1098       1092       1083 
    ##     london       lord       john     sermon   together  majesties      order 
    ##       1021        975        887        849        847        837        836 
    ##  according      great    english     letter   licensed    present    william 
    ##        832        823        781        763        762        710        692 
    ##   preached concerning containing      death 
    ##        666        643        602        574 
    ## 
    ## $`1686-1695`
    ##       king    account    england   licensed  majesties        new    several 
    ##       1144       1108       1029        974        932        902        885 
    ##      order  according       lord     church     sermon    english     london 
    ##        881        879        799        780        739        731        723 
    ##     french       true       john      great     letter    present    william 
    ##        689        684        681        671        665        618        617 
    ##      queen   together containing   preached 
    ##        605        570        544        521 
    ## 
    ## $`1691-1700`
    ##    several    account     sermon       john        new    england       king 
    ##       1118       1028        949        930        906        905        862 
    ##       lord    william     london    english     church        god containing 
    ##        763        740        733        731        674        650        635 
    ##  majesties     french      added      great concerning       true   together 
    ##        628        626        609        587        578        575        538 
    ##   preached    present  according      right 
    ##        538        529        529        527 
    ## 
    ## $`1696-1705`
    ##    several    england     sermon       john    account        new     church 
    ##       1214       1004        998        991        965        861        859 
    ##       king    english    william containing     london       lord    majesty 
    ##        806        770        763        685        661        656        629 
    ##      added    preachd parliament        god concerning     letter      right 
    ##        620        615        606        601        597        590        574 
    ## honourable   together     french        act 
    ##        554        545        530        528 
    ## 
    ## $`1701-1710`
    ##     sermon    several     church       john    preachd    england    account 
    ##       1356       1109       1090       1047       1044        998        953 
    ##       lord        new    english containing     letter      right    majesty 
    ##        795        782        734        690        662        653        653 
    ##    william       king        god      great     london      added parliament 
    ##        652        616        604        604        595        573        550 
    ##     french honourable    present       true 
    ##        532        527        521        491 
    ## 
    ## $`1706-1715`
    ##     sermon    preachd     church    several    account       john       lord 
    ##       1678       1424       1238       1122       1115       1078       1039 
    ##    england     letter        new      right      great    majesty       king 
    ##        932        879        835        817        813        792        787 
    ##    english      added containing    present     london honourable        god 
    ##        771        758        682        680        651        648        639 
    ##        act parliament    history    william 
    ##        631        627        598        594 
    ## 
    ## $`1711-1720`
    ##     sermon     church    several    preachd       lord    account     letter 
    ##       1494       1411       1301       1247       1209       1208       1191 
    ##       john       king    england        new      right      added containing 
    ##       1077       1072        945        888        875        873        865 
    ##      great     bishop    english    history   reverend    present    majesty 
    ##        808        807        779        741        726        705        689 
    ##     london        act        god honourable 
    ##        688        666        661        642 
    ## 
    ## $`1716-1725`
    ##    several     sermon     church       lord       john    account containing 
    ##       1190       1095       1071       1043        995        969        906 
    ##     letter    preachd        new      added      right    england    english 
    ##        898        854        827        738        737        725        708 
    ##      great       king     bishop   reverend    history     london        act 
    ##        701        697        674        647        640        636        574 
    ##   together        use        god   publishd 
    ##        554        553        544        543 
    ## 
    ## $`1721-1730`
    ##    several        new       john    account containing     sermon       lord 
    ##       1102        919        890        875        852        828        737 
    ##      added      great    english     church     london    history     letter 
    ##        736        710        690        664        634        633        592 
    ##      right    preachd   together        use      whole       king    england 
    ##        587        566        532        518        517        501        489 
    ##        act   reverend       life     french 
    ##        480        478        476        465 
    ## 
    ## $`1726-1735`
    ##    several        new containing      added    account       john     letter 
    ##       1111        959        904        835        829        804        773 
    ##      great     sermon    history       lord     church     london    english 
    ##        753        701        657        618        600        570        568 
    ##    present       king      whole      right parliament       life        act 
    ##        523        503        503        499        494        470        469 
    ## concerning honourable    william    remarks 
    ##        453        450        442        434 
    ## 
    ## $`1731-1740`
    ##    several containing        new    account       john     letter      added 
    ##       1015       1004        950        949        831        819        809 
    ##      great     sermon    history        act    english     church       lord 
    ##        774        741        666        659        646        602        592 
    ##    present      whole     london       life parliament    remarks       king 
    ##        589        576        548        521        518        517        516 
    ##    england      right        use     answer 
    ##        481        465        460        446 
    ## 
    ## $`1736-1745`
    ##    account containing    several       john        new     sermon    present 
    ##       1013        925        892        883        871        835        795 
    ##     letter        act      great      added     london    history      whole 
    ##        741        730        725        703        637        633        606 
    ##    english       lord       king     church   preached       life      right 
    ##        600        559        555        553        522        518        475 
    ##    england parliament     george    general 
    ##        473        452        425        424 
    ## 
    ## $`1741-1750`
    ##    account containing     sermon    several       john     letter    present 
    ##        978        968        921        904        861        856        815 
    ##        new        act      great      added       lord   preached     london 
    ##        798        712        675        674        663        663        631 
    ##     church    history      whole    general     thomas      right    english 
    ##        602        598        542        530        510        501        497 
    ##       king    england       life parliament 
    ##        482        469        462        435 
    ## 
    ## $`1746-1755`
    ## containing    account        new    several       john     sermon     letter 
    ##       1055       1042        999        948        904        870        848 
    ##       lord        act      added    present   preached     london      great 
    ##        750        723        717        686        682        680        663 
    ##    history     church        use      whole parliament     thomas    england 
    ##        651        626        544        539        535        523        520 
    ##    english       life    general      right 
    ##        519        505        505        496 
    ## 
    ## $`1751-1760`
    ##        new    account containing       john    several     letter        act 
    ##       1090       1057       1047       1002        983        868        864 
    ##     sermon      added   preached      great       lord    history    present 
    ##        861        802        775        763        728        722        718 
    ##     london    english    general       life    england parliament        use 
    ##        641        593        557        547        547        544        540 
    ##      whole     church    william      right 
    ##        537        519        488        481 
    ## 
    ## $`1756-1765`
    ##        new containing    several       john    account        act     letter 
    ##       1172       1043        922        889        849        819        717 
    ##     sermon      great   preached      added    present    history       lord 
    ##        717        711        701        683        682        640        638 
    ##    english     london    general parliament      whole    england       life 
    ##        610        566        558        537        527        521        482 
    ##    william        use     church      royal 
    ##        482        481        466        458 
    ## 
    ## $`1761-1770`
    ##        new containing    several        act       john    account    history 
    ##       1408       1216        883        875        872        795        672 
    ##    present      added parliament      great     london     letter    england 
    ##        667        658        656        640        637        632        624 
    ##    english      royal     sermon       lord   preached    general    letters 
    ##        605        595        585        575        561        521        508 
    ##        use     public      whole      court 
    ##        508        508        485        473 
    ## 
    ## $`1766-1775`
    ##        new containing        act       john    several    account parliament 
    ##       1545       1280       1108       1009        891        835        798 
    ##      added    england    history     public    present      great     london 
    ##        796        781        779        754        744        743        707 
    ##    english     letter      state      royal    letters   complete       lord 
    ##        682        636        632        613        607        592        582 
    ##      whole        use    general        rev 
    ##        555        552        544        544 
    ## 
    ## $`1771-1780`
    ##          new   containing          act         john   parliament        added 
    ##         1713         1391         1256          983          851          835 
    ##      present      english        great       london       public      account 
    ##          804          786          785          783          764          762 
    ##      several      history     complete      general      england         lord 
    ##          760          729          654          649          642          638 
    ##       letter      letters      william        state        royal observations 
    ##          632          615          615          613          609          607 
    ##          rev 
    ##          591 
    ## 
    ## $`1776-1785`
    ##          new          act   containing         john   parliament       london 
    ##         1597         1322         1306          980          939          927 
    ##      present       public        great        added      account     complete 
    ##          900          844          825          804          788          747 
    ##        royal        state      several      general      england      history 
    ##          740          732          718          709          702          700 
    ##         lord observations          law       letter      letters        court 
    ##          696          679          677          659          643          636 
    ##      william 
    ##          629 
    ## 
    ## $`1781-1790`
    ##          new          act   containing         john       london      present 
    ##         1534         1407         1407         1184         1090         1045 
    ##        royal   parliament        added      account        great          rev 
    ##          974          959          952          904          894          859 
    ##      several       public        state      history      england     complete 
    ##          844          823          804          803          799          764 
    ## observations      william         lord      general          law      society 
    ##          753          752          743          718          701          684 
    ##       sermon 
    ##          670 
    ## 
    ## $`1786-1795`
    ##          new   containing          act         john      present          rev 
    ##         1854         1616         1611         1543         1233         1191 
    ##       london        added        royal        great      history      general 
    ##         1177         1176         1096         1094         1015         1011 
    ##      account       letter      william       sermon      england   parliament 
    ##         1005          959          956          911          899          886 
    ##     preached observations      several         lord       french       thomas 
    ##          864          842          841          830          824          822 
    ##       public 
    ##          795 
    ## 
    ## $`1791-1800`
    ##          act          new         john   containing          rev      present 
    ##         2143         2127         1892         1704         1531         1499 
    ##       london      general        great       french      history      william 
    ##         1363         1327         1306         1273         1243         1240 
    ##        added        royal      account       sermon       letter      england 
    ##         1223         1155         1148         1136         1082         1029 
    ##     preached       thomas   parliament observations         lord        right 
    ##         1021         1020          982          955          949          935 
    ##       church 
    ##          930 
    ## 
    ## $`1796-1805`
    ##          act          new         john   containing      present          rev 
    ##         1237          992          914          793          782          742 
    ##       london        great      william       french        added      general 
    ##          685          680          678          677          627          626 
    ##      history        royal      account   parliament        right       sermon 
    ##          621          602          590          557          542          537 
    ##      england      majesty         lord      britain     preached observations 
    ##          507          500          497          492          479          467 
    ##       public 
    ##          464

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
