{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Retirement (retirement) where

import Common
import Types
import Simulation
import SimulationTypes
import Session
import Homepage (layoutM)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import Fmt.Internal.Numeric (baseF)

data SimulationForm = SimulationForm
    { formAge :: !Int
    , formSex :: !Sex
    , formWorkBegin :: !Int
    , formWorkEnd :: !Int
    , formSalary :: !Double
    , formExpectedSalary :: !Double
    }

retirement :: ScottyT App ()
retirement = do
    Scotty.get "/retirement/new-simulation" newSimulationForm
    Scotty.post "/retirement/new-simulation" createSimulationHandler
    Scotty.get "/retirement/simulation/:id/dashboard" dashboardHandler

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
                "Wynagrodzenie"
                input ! required "required" ! name "salary" ! type_ "number" ! step "1" ! value "4000"
            label $ do
                "Oczekiwana wysokość emerytury"
                input ! required "required" ! name "expected_salary" ! type_ "number" ! step "1" ! value "2000"
            button ! type_ "submit" $ "Start"

createSimulationHandler :: Handler ()
createSimulationHandler = do
    MkSessionData{..} <- ensureSession'
    formData <- SimulationForm
        <$> Scotty.formParam "age"
        <*> Scotty.formParam "sex"
        <*> Scotty.formParam "start_age"
        <*> Scotty.formParam "end_age"
        <*> Scotty.formParam "salary"
        <*> Scotty.formParam "expected_salary"
    AppEnv{..} <- lift ask
    simId <- liftIO $ withResource connPool $ \conn -> createSimulation conn sessionUserId formData
    Scotty.redirect $ "/retirement/simulation/" +| baseF 10 (unSimulationId simId) +| "/dashboard"

createSimulation :: Connection -> UserId -> SimulationForm -> IO SimulationId
createSimulation conn userId SimulationForm{..} = do
    [Only simId] <- query conn stmt (userId, formAge, sexToText formSex, formWorkBegin)
    return simId
    where
        stmt = [sql|
            INSERT INTO simulations (user_id, age, sex, work_start, created_at) VALUES (?, ?, ?, ?, transaction_timestamp()) RETURNING id;
        |]

dashboardHandler :: Handler ()
dashboardHandler = do
    paramId <- SimulationId <$> Scotty.captureParam @Int64 "id"
    logInfo $ "Simualtion id: " +| baseF 10 (unSimulationId paramId) |+ ""
    layout <- layoutM
    AppEnv{..} <- lift ask
    Simulation{..} <- liftIO $ withResource connPool $ \conn -> getSimulation conn paramId
    Scotty.html . renderHtml $ layout $ do
        h1 "Wynik symulacji"
        div $ toMarkup simAge
        div $ toMarkup simSex
        div $ toMarkup simWorkStart

getSimulation :: Connection -> SimulationId -> IO Simulation
getSimulation conn simId = do
    [sim] <- query conn stmt $ Only simId
    return sim
    where
        stmt = [sql| SELECT id, age, sex, work_start FROM simulations WHERE id = ?; |]
