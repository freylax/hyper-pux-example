module Site where

import Prelude
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Generic (class Generic, gShow)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type TaskId = Int

data Task = Task TaskId String

derive instance genericTask :: Generic Task

instance showTask :: Show Task where show = gShow
instance encodeJsonTask :: EncodeJson Task where encodeJson = gEncodeJson
instance decodeJsonTask :: DecodeJson Task where decodeJson = gDecodeJson

type TasksResource = Resource (Get (Array Task) JSON)

type TaskResource = Resource (Get Task JSON)

type Site =
  "tasks" :/ ( "tasks" := TasksResource
               :<|> "task"  := Capture "id" TaskId :> TaskResource)

site :: Proxy Site
site = Proxy
