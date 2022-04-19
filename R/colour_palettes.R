
## ---- colqua15 ----

col_qua15 <- c(
    'c1' = '#db6d00',
    'c2' = '#009292',
    'c3' = '#004949',
    'c4' = '#490092',
    'c5' = '#006ddb',
    'c6' = '#924900',
    'c7' = '#24ff24',
    'c8' = '#ff6db6',
    'other' = '#b66dff',
    'cantsay' = '#ffff6d',
    'na' = '#b6dbff',
    'black' = '#000000',
    'red' = '#920000',
    'light1' = '#ffb6db',
    'light2' = '#6db6ff'
)


## ---- colseq11 ----

col_seq11 <- colorRampPalette( brewer.pal( 9, "PuBu" ) )( 11 )
names( col_seq11 ) <- as.character( 0:10 )
col_seq11


## ---- coldiv11 ----

col_div11 <- c(
    neg5 = '#a50026',
    neg4 = '#d73027',
    neg3 = '#f46d43',
    neg2 = '#fdae61',
    neg1 = '#fee090',
    ntrl = '#ffffbf',
    pos1 = '#e0f3f8',
    pos2 = '#abd9e9',
    pos3 = '#74add1',
    pos4 = '#4575b4',
    pos5 = '#313695'
)
