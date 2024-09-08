module Demo.Analyze where

import Analyze


analyze_science_file :: IO ()
analyze_science_file = do
  file <- lines <$> readFile "data/science.org"
  putStrLn "=== properties ==="
  mapM_ (putStrLn . show . is_properties) $ file
  putStrLn "=== ids ==="
  mapM_ (putStrLn . show . id_if_id)      $ file
  let Just titleLine = countTitleLine file
  putStrLn $ "=== titleLine: " ++ show titleLine
  putStrLn "=== linesOfIds ==="
  mapM_ (putStrLn . show) $ linesOfIds titleLine file
