Translate
==========

Haskell binding to Google translate

    ghci
    
    > :m Text.Translate
    > translate "en" "fr" "hello"
    
    Just "bonjour"
    
    > :m Text.Language.Detect
    > detect "Program testing can be used to show the presence of bugs, but never to show their absence!"

    Just ("en",True,0.87355334)
