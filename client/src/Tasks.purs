module Tasks where

import Prelude  (const, discard, show, ($), (<$>), (<<<), (<>))
import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Foldable (for_)
import Type.Trout.Client (asClients)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (li, div, h1, button, ol)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!), text)
import Data.Either (Either(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception ( Error)
import Site (Task(..), site)

type Tasks = Array Task

data Event = RequestTasks
           | RequestTask
           | ReceiveTasks (Either Error Tasks)
           | ReceiveTask (Either Error Task)

type State =
  { tasks :: Tasks
  , status :: String }

init :: State
init = { tasks: [], status: "Nothing loaded yet"}

-- | Our update function requests data from the server, and handles data
-- | received from input.
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (ReceiveTasks t) state =
  noEffects $ case t of
    Left err -> state { tasks = [], status = "Error Fetching Tasks:" <> show err }
    Right tasks -> state { tasks = tasks, status = "Tasks:" }
foldp (ReceiveTask t) state =
  noEffects $ case t of
    Left err -> state { tasks = [], status = "Error Fetching Task:" <> show err }
    Right task -> state { tasks = singleton task, status = "Task:" }
foldp (RequestTasks) state =
  { state: state { status = "Fetching tasks..." }
  , effects: [ (Just <<< ReceiveTasks) <$> attempt tasks."GET" ]
  }
  where {tasks} = asClients site 

foldp (RequestTask) state = 
  { state: state { status = "Fetching task..." }
  , effects: [ ( Just <<< ReceiveTask ) <$> attempt (task 5)."GET" ]
  } where {task} = asClients site
          
view :: State -> HTML Event
view state =
  div do
    h1 $ text state.status
    div do
      button #! onClick (const RequestTasks) $ text "Fetch Tasks"
      button #! onClick (const RequestTask) $ text "Fetch Task 5"
      ol $ for_ state.tasks viewTask

viewTask :: Task -> HTML Event
viewTask (Task id title) =
  li ! key (show id) ! className "task" $ text title
