data IntentTypes = QuickMetrics | TopPerformingPorduct
newtype Intent = Intent { getIntentType :: IntentTypes  }

data EntityTypes = Item | Measure | Location | TimePeriod | SearchOption
newtype Entity = Entity { getEntityType :: EntityTypes }

newtype Utterance = Utterance { getUtterance :: String }

main :: IO()
main = do
  let intent = Intent QuickMetrics
      entity = Entity Item
  putStrLn "HI"