
# TODO: get rid of this?
library( tibble )

categories <- list(

    'gender' = tibble(
        name = c( 'male', 'female' ),
        mapping = 1:2,
        plotlabel = c( 'Male', 'Female' ),
        colorhex = col_qua15[c( 'c2', 'c1' )]
    ),
    'employersec' = tibble(
        name = c(
            'manufacturing', 'private', 'state', 'municipal', 'other' ),
        mapping = c( 1:4, 9 ),
        plotlabel = c(
            'Manufacturing', 'Private services',
            'State services', 'Municipal services', 'Other'
        ),
        colorhex = col_qua15[c( 'c1', 'c2', 'c3', 'c4', 'other' )]
    ),
    'wp_location' = tibble(
        name = c(
            'capital', 'south', 'southwest', 'east',
            'west', 'north', 'lapland', 'aland'
        ),
        mapping = 0:7,
        plotlabel = c(
            'Capital area',
            'Southern Finland',
            'South-Western Finland',
            'Eastern Finland',
            'Western Finland and Inland',
            'Northern Finland', 'Lapland', 'Åland'
        ),
        colorhex = col_qua15[c(
            'c1', 'c2', 'c3', 'c4',
            'c5', 'c6', 'c7', 'c8'
        )]
    ),
    'wp_nofpeople' = tibble(
        name = c(
            'fr1to4', 'fr5to9', 'fr10to19', 'fr20to29', 'fr30to49',
            'fr50to99', 'fr100to199', 'fr200to249', 'fr250to499',
            'fr500to999', 'over1000', 'cantsay'
        ),
        mapping = c( 1:11, 99 ),
        plotlabel = c(
            '1–4', '5–9', '10–19', '20–29', '30–49',
            '50–99', '100–199', '200–249', '250–499',
            '500–999', '1000 or more', "Can't say"
        ),
        colorhex = c(
            col_seq11, col_qua15['cantsay'] )
    ),
    'stmt_manytasks' = tibble(
        name = c(
            'stragree', 'agree', 'disagree', 'strdisagree',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay', 'cantsay'
        ),
        mapping = c( 1:5, 8, 9 ),
        plotlabel = c(
            'Strongly agree', 'Agree', 'Disagree', 'Strongly disagree',
            "Can't say",
            "Can't or don't want to say", "Can't or don't want to say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'neg3', 'neg5')],
            col_qua15[c( 'cantsay', 'cantsay', 'cantsay' )]
        )
    ),
    'stmt_wellinformed' = tibble(
        name = c(
            'stragree', 'agree', 'disagree', 'strdisagree',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay', 'cantsay'
        ),
        mapping = c( 1:5, 8, 9 ),
        plotlabel = c(
            'Strongly agree', 'Agree', 'Disagree', 'Strongly disagree',
            "Can't say",
            "Can't or don't want to say", "Can't or don't want to say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'neg3', 'neg5')],
            col_qua15[c( 'cantsay', 'cantsay', 'cantsay' )]
        )
    ),
    'stmt_treatedfair' = tibble(
        name = c(
            'stragree', 'agree', 'disagree', 'strdisagree',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay', 'cantsay'
        ),
        mapping = c( 1:5, 8, 9 ),
        plotlabel = c(
            'Strongly agree', 'Agree', 'Disagree', 'Strongly disagree',
            "Can't say",
            "Can't or don't want to say", "Can't or don't want to say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'neg3', 'neg5')],
            col_qua15[c( 'cantsay', 'cantsay', 'cantsay' )]
        )
    ),
    'stmt_jobsecure' = tibble(
        name = c(
            'stragree', 'agree', 'disagree', 'strdisagree',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay', 'cantsay'
        ),
        mapping = c( 1:5, 8, 9 ),
        plotlabel = c(
            'Strongly agree', 'Agree', 'Disagree', 'Strongly disagree',
            "Can't say",
            "Can't or don't want to say", "Can't or don't want to say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'neg3', 'neg5')],
            col_qua15[c( 'cantsay', 'cantsay', 'cantsay' )]
        )
    ),
    'stmt_physdmd' = tibble(
        name = c(
            'stragree', 'agree', 'disagree', 'strdisagree',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay', 'cantsay'
        ),
        mapping = c( 1:5, 8, 9 ),
        plotlabel = c(
            'Strongly agree', 'Agree', 'Disagree', 'Strongly disagree',
            "Can't say",
            "Can't or don't want to say", "Can't or don't want to say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'neg3', 'neg5')],
            col_qua15[c( 'cantsay', 'cantsay', 'cantsay' )]
        )
    ),
    'stmt_mentdmd' = tibble(
        name = c(
            'stragree', 'agree', 'disagree', 'strdisagree',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay', 'cantsay'
        ),
        mapping = c( 1:5, 8, 9 ),
        plotlabel = c(
            'Strongly agree', 'Agree', 'Disagree', 'Strongly disagree',
            "Can't say",
            "Can't or don't want to say", "Can't or don't want to say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'neg3', 'neg5')],
            col_qua15[c( 'cantsay', 'cantsay', 'cantsay' )]
        )
    ),
    'jobinsec_laidoff' = tibble(
        name = c(
            'definitely', 'could', 'notprob', 'notdef',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'notworking', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Will definitely happen', 'Could happen',
            'Probably will not happen',
            'Will definitely not happen',
            "Can't say", 'Not working at the moment', "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'neg5', 'neg3', 'pos3', 'pos5' )],
            col_qua15[c( 'cantsay', 'other', 'cantsay' )]
        )
    ),
    'jobinsec_dismissed' = tibble(
        name = c(
            'definitely', 'could', 'notprob', 'notdef',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'notworking', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Will definitely happen', 'Could happen',
            'Probably will not happen',
            'Will definitely not happen',
            "Can't say", 'Not working at the moment',
            "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'neg5', 'neg3', 'pos3', 'pos5' )],
            col_qua15[c( 'cantsay', 'other', 'cantsay' )]
        )
    ),
    'change_meaningfulness' = tibble(
        name = c(
            'better', 'swbetter', 'same', 'swworse', 'worse',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Getting considerably better',
            'Getting somewhat better',
            'Staying the same',
            'Getting somewhat worse',
            'Getting considerably worse',
            "Can't say", "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'ntrl', 'neg3', 'neg5' )],
            col_qua15[c( 'cantsay', 'cantsay' )]
        )
    ),
    'change_keptuptodate' = tibble(
        name = c(
            'better', 'swbetter', 'same', 'swworse', 'worse',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Getting considerably better',
            'Getting somewhat better',
            'Staying the same',
            'Getting somewhat worse',
            'Getting considerably worse',
            "Can't say", "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'ntrl', 'neg3', 'neg5' )],
            col_qua15[c( 'cantsay', 'cantsay' )]
        )
    ),
    'change_devopportunities' = tibble(
        name = c(
            'better', 'swbetter', 'same', 'swworse', 'worse',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Getting considerably better',
            'Getting somewhat better',
            'Staying the same',
            'Getting somewhat worse',
            'Getting considerably worse',
            "Can't say", "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'ntrl', 'neg3', 'neg5' )],
            col_qua15[c( 'cantsay', 'cantsay' )]
        )
    ),
    'change_influencestatus' = tibble(
        name = c(
            'better', 'swbetter', 'same', 'swworse', 'worse',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Getting considerably better',
            'Getting somewhat better',
            'Staying the same',
            'Getting somewhat worse',
            'Getting considerably worse',
            "Can't say", "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'ntrl', 'neg3', 'neg5' )],
            col_qua15[c( 'cantsay', 'cantsay' )]
        )
    ),
    'change_finance' = tibble(
        name = c(
            'better', 'swbetter', 'same', 'swworse', 'worse',
            # In FWLB, different years have
            # different coding for "Can't say"
            'cantsay', 'cantsay'
        ),
        mapping = c( 1:6, 9 ),
        plotlabel = c(
            'Getting considerably better',
            'Getting somewhat better',
            'Staying the same',
            'Getting somewhat worse',
            'Getting considerably worse',
            "Can't say", "Can't say"
        ),
        colorhex = c(
            col_div11[c( 'pos5', 'pos3', 'ntrl', 'neg3', 'neg5' )],
            col_qua15[c( 'cantsay', 'cantsay' )]
        )
    ),
    'finance_grade' = tibble(
        name = c( 0:10, 'cantsay' ),
        mapping = c( 0:10, 99 ),
        plotlabel = as.character(
            c( 0:10, "Can't say" ) ),
        colorhex = c(
            col_seq11, col_qua15['cantsay'] )
    )
)
