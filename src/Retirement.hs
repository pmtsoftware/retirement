{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}

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
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Blaze.Html.Renderer.Text
import Fmt.Internal.Numeric (baseF, fixedF)
import Data.Scientific (toRealFloat)

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
    Scotty.post "/retirement/simulation/run" runSimulationHandler
    Scotty.get "/retirement/simulation/:id/history" simulationHistoryHandler

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
                input ! required "required" ! name "start_age" ! type_ "number" ! step "1" ! value "20" ! Attr.min "18" ! Attr.max "29"
            label $ do
                "Planowany wiek zakończenia pracy"
                input ! required "required" ! name "end_age" ! type_ "number" ! step "1" ! value "65" ! Attr.min "40" ! Attr.max "80"
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
    formData@SimulationForm{..} <- SimulationForm
        <$> Scotty.formParam "age"
        <*> Scotty.formParam "sex"
        <*> Scotty.formParam "start_age"
        <*> Scotty.formParam "end_age"
        <*> Scotty.formParam "salary"
        <*> Scotty.formParam "expected_salary"
    AppEnv{..} <- lift ask
    simId <- liftIO $ withResource connPool $ \conn -> do
        simid <- createSimulation conn sessionUserId formData
        paramid <- createParams conn simid formSalary formExpectedSalary formWorkEnd
        updateParamId conn simid paramid
        pure simid
    Scotty.redirect $ "/retirement/simulation/" +| baseF 10 (unSimulationId simId) +| "/dashboard"

createSimulation :: Connection -> UserId -> SimulationForm -> IO SimulationId
createSimulation conn userId SimulationForm{..} = do
    [Only simId] <- query conn stmt (userId, formAge, sexToText formSex, formWorkBegin)
    return simId
    where
        stmt = [sql|
            INSERT INTO simulations (user_id, age, sex, work_start, created_at) VALUES (?, ?, ?, ?, transaction_timestamp()) RETURNING id;
        |]

createParams :: Connection -> SimulationId -> Double -> Double -> Int -> IO ParameterId
createParams conn simId salary expectedSalary workEnd = do
    [Only paramId] <- query conn stmt (simId, workEnd, salary, expectedSalary)
    return paramId
    where
        stmt = [sql|
            INSERT INTO simulation_params (simulation_id, work_end, salary, expected_salary, created_at)
            VALUES (?, ?, ?, ?, transaction_timestamp()) RETURNING id;
        |]

updateParamId :: Connection -> SimulationId -> ParameterId -> IO ()
updateParamId conn simId paramId = do
    _ <- execute conn stmt (paramId, simId)
    return ()
    where
        stmt = [sql| UPDATE simulations SET param_id = ? WHERE id = ?; |]

dashboardHandler :: Handler ()
dashboardHandler = do
    paramId <- SimulationId <$> Scotty.captureParam @Int64 "id"
    -- logInfo $ "Simualtion id: " +| baseF 10 (unSimulationId paramId) |+ ""
    layout <- layoutM
    AppEnv{..} <- lift ask
    sim@Simulation{..} <- liftIO $ withResource connPool $ \conn -> getSimulation conn paramId
    Scotty.html . renderHtml $ layout $ do
        simulationResult False sim
        hr
        form $ do
            input ! type_ "hidden" ! name "simulation_id" ! value (toValue (unSimulationId paramId))
            label $ do
                "Planowany wiek zakończenia pracy (w latach)"
                input ! required "required" ! name "end_age" ! type_ "number" ! step "1" ! value (toValue simWorkEnd)
            label $ do
                "Wynagrodzenie"
                input ! required "required" ! name "salary" ! type_ "number" ! step "1" ! value (toValue (toRealFloat @Double simSalary))
            label $ do
                "Oczekiwana wysokość emerytury"
                input ! required "required" ! name "expected_salary" ! type_ "number" ! step "1" ! value (toValue (toRealFloat @Double simExpectedSalary))
            button ! hx_post ! hx_swap_none $ "Przelicz ponownie"

            hr
            a ! href (historyUri paramId) $ "Pokaż całą historię"
    where
        historyUri :: SimulationId -> AttributeValue
        historyUri simId = toValue @Text $ "/retirement/simulation/" +| baseF 10 (unSimulationId simId) +| "/history"
        hx_post = customAttribute "hx-post" "/retirement/simulation/run"
        hx_swap_none = customAttribute "hx-swap" "none"

simulationResult :: Bool -> Simulation -> Html
simulationResult oob Simulation{..} = table ! id "results" !? (oob, hx_oob) $ do
    caption $ h2 "Wynik symulacji"
    tbody $ do
        tr $ do
            th ! scope "row" $ "Płeć"
            td $ renderSex simSex
        tr $ do
            th ! scope "row" $ "Wiek"
            td $ toMarkup simAge <> " lat"
        tr $ do
            th ! scope "row" $ "Wiek rozpoczęcia pracy"
            td $ toMarkup simWorkStart <> " lat"
        tr $ do
            th ! scope "row" $ "Prognozowana emerytura"
            td $ toMarkup (fixedF 2 simValue) <> " zł"
        tr $ do
            td ! colspan "2" $ comment simValue (toRealFloat simExpectedSalary) simulatedAge
    where
        f = simulate Female (toRealFloat simSalary) simWorkStart
        simValue = f simWorkEnd
        simulatedAge = simulateAge f (simWorkEnd + 1) $ toRealFloat simExpectedSalary
        hx_oob = customAttribute "hx-swap-oob" "true"

        comment :: Double -> Double -> Int -> Html
        comment simulatedSalary expectedSalary simulatedAge
            | simulatedSalary >= expectedSalary = code "😀 Wyliczona kwota jest równa lub większa od spodziewanej!"
            | otherwise = code $ "😟 Niestety aby osiągnąć spodziewaną kwotę emerytury będziesz musiał pracowac do " <> toMarkup simulatedAge <> " lat!"

getSimulation :: Connection -> SimulationId -> IO Simulation
getSimulation conn simId = do
    [sim] <- query conn stmt $ Only simId
    return sim
    where
        stmt = [sql|
            SELECT s.id, s.age, s.sex, s.work_start, p.work_end, p.salary, p.expected_salary FROM simulations AS s JOIN simulation_params AS p ON s.param_id = p.id WHERE s.id = ?;
        |]

runSimulationHandler :: Handler ()
runSimulationHandler = do
    simulationId <- SimulationId <$> Scotty.formParam @Int64 "simulation_id"
    salary <- Scotty.formParam @Double "salary"
    endAge <- Scotty.formParam @Int "end_age"
    expectedSalary <- Scotty.formParam @Double "expected_salary"
    AppEnv{..} <- lift ask
    liftIO $ withResource connPool $ \conn -> do
        paramid <- createParams conn simulationId salary expectedSalary endAge
        updateParamId conn simulationId paramid
    sim <- liftIO $ withResource connPool $ \conn -> getSimulation conn simulationId
    Scotty.html . renderHtml $ simulationResult True sim

simulationHistoryHandler :: Handler ()
simulationHistoryHandler = do
    simulationId <- SimulationId <$> Scotty.captureParam @Int64 "id"
    AppEnv{..} <- lift ask
    allSimulations <- liftIO $ withResource connPool $ \conn -> do
        query conn stmt $ Only simulationId
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        a ! href (backUri simulationId) $ "Wróć"
        hr
        table $ do
            thead $ tr $ do
                th ! scope "col" $ "Wiek"
                th ! scope "col" $ "Płeć"
                th ! scope "col" $ "Początek"
                th ! scope "col" $ "Koniec"
                th ! scope "col" $ "Wynagrodzenie"
                th ! scope "col" $ "Oczekiwana emerytura"
                th ! scope "col" $ "Wyliczona emerytura"
            tbody $ forM_ allSimulations toRow
    where
        toRow :: Simulation -> Html
        toRow Simulation{..} = tr $ do
            td $ toMarkup simAge
            td $ text simSex
            td $ toMarkup simWorkStart
            td $ toMarkup simWorkEnd
            td $ toMarkup . toRealFloat @Double $ simSalary
            td $ toMarkup . toRealFloat @Double $ simExpectedSalary
            td $ toMarkup (fixedF 2 (simulate Female (toRealFloat simSalary) simWorkStart simWorkEnd)) <> " zł"
        stmt = [sql|
            SELECT s.id, s.age, s.sex, s.work_start, p.work_end, p.salary, p.expected_salary FROM simulation_params AS p LEFT JOIN simulations AS s ON p.simulation_id = s.id WHERE p.simulation_id = ?;
        |]
        backUri :: SimulationId -> AttributeValue
        backUri simId = toValue @Text $ "/retirement/simulation/" +| baseF 10 (unSimulationId simId) +| "/dashboard"

renderSex :: Text -> Html
renderSex "male" = "Mężczyzna"
renderSex "female" = "Kobieta"
renderSex _ = mempty
