module Main where

import System.Environment (getArgs,getProgName)
import Parser(parserComm)
import Control.Exception (catch)   
import System.IO.Error  

-- El modulo Parser exporta una funcion 
-- 
--   parserComm :: String -> Comm
--
-- que convierte una cadena de caracteres que representa un programa LIS en una 
-- expresion de tipo Comm.


-- Modificar este import para usar diferentes evaluadores
import Eval3
---------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          case args of
             []   -> printHelp
             args -> mapM_ run args
  
-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile = catch (tryRun ifile) (ioErrorCatcher ifile)
  where
    tryRun ifile = do putStrLn ("Ejecutando el archivo " ++ ifile ++ "...")
                      s <- readFile ifile
                      (print . eval) (parserComm s)

ioErrorCatcher :: FilePath -> IOError -> IO ()
ioErrorCatcher name e
  | isDoesNotExistError e = putStrLn ("Error: el archivo "++name++" no existe.")
  | isPermissionError e = putStrLn ("Error: permiso denegado para acceder al archivo "++name++".")
  | otherwise = putStrLn ("Error: "++ show (e :: IOError) ++".")             
  
printHelp :: IO ()
printHelp = do name <- getProgName
               if name /= "<interactive>" then
                  putStrLn ("Intérprete de LIS (TP4).\n" ++
                         "Pase como argumentos los nombres de los archivos a ejecutar.\n"++
                         "Por ejemplo: "++name++" fact.lis Ejemplos/fact.lis \n")
                                         else
                  putStrLn ("Intérprete de LIS (TP4) en modo interactivo.\n" ++
                         "Pase como argumento a la función run "++
                         "una cadena con el nombre del archivo a ejecutar.\n"++
                         "Por ejemplo: run \"fact.lis\"\n")
