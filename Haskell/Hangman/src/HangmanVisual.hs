module HangmanVisual where

one   = "_________              "

two   = "____|___________       "

three = "    |    "++
        "____|___________       "
        
four  = "    |    "++
        "    |    "++
        "____|___________       "

five  = "    |    "++
        "    |    "++
        "    |    "++
        "____|___________       "
        
six   = "    |    "++
        "    |    "++
        "    |    "++
        "    |    "++
        "____|___________       "

seven = "    |    "++
        "    |    "++
        "    |    "++
        "    |    "++
        "    |    "++
        "____|___________       "

eight = "    |______     "++
        "    |/          "++
        "    |           "++
        "    |           "++
        "    |           "++
        "____|___________       "

nine  = "    |______     "++
        "    |/     |    "++
        "    |           "++
        "    |           "++
        "    |           "++
        "____|___________       "

ten   = "    |______     "++
        "    |/     |    "++
        "    |      ()   "++
        "    |           "++
        "    |           "++
        "____|___________       "
        
elvn  = "    |________    "++
        "    |/      |    "++
        "    |     \\()//   "++
        "    |           "++
        "____|___________       "

twlv  = "    |________    "++
        "    |/     |    "++
        "    |    \\()//   "++
        "    |      ||   "++
        "    |           "++
        "____|___________       "
        
theen  = "   |________          hanged! ha! "++
        "    |/     |           ___|___   "++
        "    |      ()        [  o  o   ] "++
        "    |    //||\\      [    i    ] "++
        "    |      \\        [//----\\ ] "++
        "____|___________     [_________] "
        
saved = "   saved! yeeh!   " ++
        "                   "++
        "       \\()//      "++
        "         ||        "++
        "        _//        "++
        "___________________"        
        
hangover = [one,two,three, four, five, six, seven, eight, nine, ten, elvn, twlv, theen]