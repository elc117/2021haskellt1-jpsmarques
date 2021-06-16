--Trabalho 1 - Generative Art com Programação Funcional em Haskell
--Nome: João Pedro da Silva Marques

import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)



rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = [(x,y,z) | x <- take n $ cycle [60,67..], y <- take n $ cycle [70,77..], z <- take n $ cycle [80,87..]]



-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10

genRectsInLine2 :: Int -> [Rect]
genRectsInLine2 n  = [((m*(w+gap), 290.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles



svgCircle :: Int -> Int -> Int -> String -> String --Recebe as caracteristicas do circulo e printa ele
svgCircle x y r style = 
  printf "<circle cx='%d' cy='%d' r='%d' fill='%s' />\n" x y r style

-- Gera SVG com 2 círculos, um verde e um vermelho, com 0.4 de opacidade.
-- A opacidade pode não ser suportada em alguns visualizadores de SVG.
svgAll :: String --Chama o svgBegin, manda as caracteriscas do circulo para os svgCircle, depois chama o svgEnd.
svgAll = 
  svgBegin 1500 1100 ++ 
  (svgCircle 50 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 150 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 250 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 350 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 450 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 550 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 650 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 750 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 850 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 950 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 1050 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 1150 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 1250 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 1350 110 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 1450 110 50 "rgb(250, 130, 115, 1.0)") ++ 
  svgEnd

svgAll2 = 
  svgBegin 1500 1100 ++ 
  (svgCircle 50 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 150 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 250 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 350 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 450 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 550 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 650 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 750 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 850 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 950 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 1050 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 1150 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 1250 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  (svgCircle 1350 230 50 "rgb(250, 130, 115, 1.0)") ++ 
  (svgCircle 1450 230 50 "rgb(255, 20, 145, 1.0)") ++ 
  svgEnd
--------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "generative_art.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgAll ++ svgAll2 ++ svgfigs2 ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        svgfigs2 = svgElements svgRect rects2 (map svgStyle palette)
        rects = genRectsInLine nrects
        rects2 = genRectsInLine2 nrects
        palette = rgbPalette nrects
        nrects = 30
        (w,h) = (1500,500) -- width,height da imagem SVG