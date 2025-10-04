module Retirement (retirement) where

import Common
import Simulation
import Homepage (layoutM)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text

data SimulationForm = SimulationForm
    { formAge :: !Int
    , formSex :: !Sex
    , formWorkBegin :: !Int
    , formWorkEnd :: !Int
    , formExpectedSalary :: !Double
    }

retirement :: ScottyT App ()
retirement = do
    Scotty.get "/retirement/new-simulation" newSimulationForm
    Scotty.post "/retirement/new-simulation" createSimulation
    Scotty.get "/retirement/simulation/:id/dashboard" dashboard

newSimulationForm :: Handler ()
newSimulationForm = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        h1 "Parametry symulacji"
        form ! method "POST" $ do
            label $ do
                "Wiek"
                input ! required "required" ! name "age" ! type_ "number" ! step "1" ! value "35"
            label $ do
                "Płeć"
                select ! name "sex" ! required "true" $ do
                    option ! value "female" $ "Kobieta"
                    option ! value "male" $ "Mężczyzna"
            label $ do
                "Wiek rozpoczęcia pracy"
                input ! required "required" ! name "start_age" ! type_ "number" ! step "1" ! value "20"
            label $ do
                "Planowany wiek zakończenia pracy"
                input ! required "required" ! name "end_age" ! type_ "number" ! step "1" ! value "65"
            label $ do
                "Oczekiwana wysokość emerytury"
                input ! required "required" ! name "expected_salary" ! type_ "number" ! step "1" ! value "5000"
            button ! type_ "submit" $ "Start"

createSimulation :: Handler ()
createSimulation = undefined

dashboard :: Handler ()
dashboard = undefined
