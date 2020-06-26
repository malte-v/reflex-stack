{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import RIO
import Reflex.Dom
import Common.TH (chooseAndIncludeFileInSource)

-- To make both `stack build` and the editor happy.
$(chooseAndIncludeFileInSource ["styles/styles.css", "frontend/styles/styles.css"] "css")

frontend :: IO ()
frontend = mainWidgetWithCss css $ el "h1" $ text "Hello, Reflex!"
