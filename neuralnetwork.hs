import Tensor

type WeightVector a = [a]
type WeightMatrix a = [WeightVector a]
type Input a = [a]
type Label a = [a]

data NN a = NN { activators :: [(a -> a)]
               , parameters :: [[[a]]]
               }

showNN :: (Show a) => NN a -> String
showNN network = "updateNetwork\n" ++ (show $ parameters network)

instance (Show a) => Show (NN a) where
  show = showNN

--------------------------------------------------
-- Activation Functions

softmax :: Floating a => [a] -> a -> a
softmax domain x = (exp x) / (sum $ map exp domain)

sigmoid :: Floating a => a -> a
sigmoid x = 1 / ( 1 + exp (- x) )

--------------------------------------------------
-- Forward Propagation

fire :: Num a => [a] -> (a -> a) -> [a] -> a
fire inputVector activator weightVector = activator (weightVector -*| inputVector)

fires :: Num a => [a] -> (a -> a) -> [[a]] -> [a]
fires inputVector activator weightMatrix = map (fire inputVector activator) weightMatrix

dump :: Num a => [a] -> [(a -> a)] -> [[[a]]] -> [[a]]
dump inputVector activators parameters = reverse $ result
  where
    fn (v:vs) (x, y) = (fires v x y) : (v:vs)
    result = foldl fn [inputVector] (zip activators parameters)

predict :: Num a => ([a] -> [a]) -> [a] -> [(a -> a)] -> [[[a]]] -> [a]
predict predictor inputVector activators parameters = predictor result
  where
    fn v (x, y) = fires v x y
    result = foldl fn inputVector (zip activators parameters)

--------------------------------------------------
-- Back Propagation

-- previousOutput = presentIntput
backPropagate :: Num a => [a] -> [a -> a] -> [[a]] -> [[[a]]] -> [[a]]
backPropagate label derivatives outputs parameters =
  let outputLayerError  = (last outputs) |-| label
  in backPropagateHelper [outputLayerError] (tail $ reverse derivatives) (tail $ reverse outputs) (reverse parameters) 
    where
      backPropagateHelper :: Num a => [[a]] -> [a -> a] -> [[a]] -> [[[a]]] -> [[a]]
      backPropagateHelper acc _ _ (_:[]) = acc
      backPropagateHelper (previousE:acc) (presentD:ds) (previousO:oss) (nextW:presentW:wsss) =
        let presentE  = (map presentD $ presentW =*| previousO) |*| ((transpose nextW) =*| previousE)
        in backPropagateHelper (presentE:previousE:acc) ds oss (presentW:wsss)

updateParameters :: Num a => a -> [[a]] -> [[a]] -> [[[a]]] -> [[[a]]]
updateParameters eps errors outputs parameters = map fn (zip3 errors outputs parameters)
  where
    fn (error, output, parameter) = parameter =-= (eps .*= (error |*- output))

deviate :: Num a => (a -> a) -> (a -> a)
deviate fn = id

updateNetwork :: Num a => a -> NN a -> (Input a, Label a) -> NN a 
updateNetwork eps network (input, label) = NN originalActivators updatedParameters
  where
    originalActivators = activators network
    originalParameters = parameters network
    derivatives        = map deviate originalActivators
    neuronOutputs      = dump input originalActivators originalParameters
    errors             = backPropagate label derivatives neuronOutputs originalParameters
    updatedParameters  = updateParameters eps errors neuronOutputs originalParameters

learn :: Num a => a -> NN a -> [(Input a, Label a)] -> NN a
learn eps initialNetwork trainingSets = foldl (updateNetwork eps) initialNetwork trainingSets

--------------------------------------------------
-- Main

main :: IO()
main = do
  let input = [1, 2, 3]
      label = [1, 0, 0]
      weights = [1, 2, 3]
      activator = sigmoid
      n = 4
      activators = take n $ repeat sigmoid
      derivatives = map deviate activators
      parameter = take 3 $ repeat weights
      parameters = take n $ repeat parameter
      outputs = dump input activators parameters
      errors = backPropagate label derivatives outputs parameters
      network = NN activators parameters

  putStrLn (show $ fire input activator weights)
  putStrLn (show $ fires input activator parameter)
  putStrLn (show $ predict (map id) input activators parameters)
  putStrLn (show $ updateParameters 1.0 errors outputs parameters)
  putStrLn (show $ updateNetwork 1.0 network (input, label))



