
## ---- colqua15 ----

col_qua15 <- list(
    c1 = '#db6d00',
    c2 = '#009292',
    c3 = '#006ddb',
    c4 = '#924900',
    c5 = '#490092',
    c6 = '#24ff24',
    c7 = '#ff6db6',
    c8 = '#004949',
    c9 = '#6db6ff',
    refused = '#b66dff',
    other = '#b6dbff',
    dontknow = '#ffb6db',
    na = '#ffff6d',
    red = '#920000',
    black = '#000000'
)


## ---- colseq11 ----

col_seq11 <- as.list( colorRampPalette( brewer.pal( 9, "PuBu" ) )( 11 ) )
names( col_seq11 ) <- as.character( 0:10 )


## ---- coldiv11 ----

col_div11 <- list(
    neg5 = '#a50026',
    neg4 = '#d73027',
    neg3 = '#f46d43',
    neg2 = '#fdae61',
    neg1 = '#fee090',
    ntrl = '#e6f5c9',
    pos1 = '#e0f3f8',
    pos2 = '#abd9e9',
    pos3 = '#74add1',
    pos4 = '#4575b4',
    pos5 = '#313695'
)
