{-# LANGUAGE MultiWayIf #-}
module Day20 ( runDay20 ) where

import Data.Attoparsec.Text
import Data.Text (pack)
import Lib (readData')
import System.IO
import Debug.Trace
import Data.List(elemIndex)

runDay20 :: [String] -> IO String
runDay20 [] = return "Usage: 20 <filename>"
runDay20 (filename:_) =
    do lines <- withFile filename ReadMode readData'
       --return $ show $ fmap (fmap (`translate` 3) . parse parseLine . pack) lines
       --return $ show $ fmap (fmap (borderPoint . rvMin) . parse parseLine . pack) lines -- Внешний fmap для списка и внутренний — для монады Parser
       --return $ show $ fmap w1 <$> parsed lines -- Внешний fmap для списка и внутренний — для монады Parser
       return $ show $ result =<< sequenceA (maybeResult <$> parsed lines) -- Внешний fmap для списка и внутренний — для монады Parser
       --return $ show $ translate testCase 3
    where parsed = fmap (parse parseLine . pack) :: [String] -> [Result Particle]
          w1 = borderPoint . rvMin :: Particle -> Int
          turnPoint = maximum . fmap w1 :: [Particle] -> Int
          result :: [Particle] -> Maybe Int
          result ps =
              loop minTime
              where minTime = turnPoint ps :: Int
                    distances t = distFromZero t <$> ps :: [Int]
                    full = translate
                    filtered t = filter (== trace'(minimum (distances t))) (distances t) :: [Int]
                    loop t = if trace' (length (filtered t)) > 1
                                then loop (t + 1)
                                else elemIndex (head (filtered t)) (trace' (distances t))

-- Example of record
-- p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>

testCase = Particle (3,0,0) (2,0,0) (-1,0,0)

trace' it = trace (show it) it

data Particle = Particle { position :: (Int, Int, Int)
                         , velocity :: (Int, Int, Int)
                         , acceleration :: (Int, Int, Int)
                         } deriving Show

data Triple a = Triple a a a

instance Functor Triple where
    fmap f (Triple x y z) = Triple (f x) (f y) (f z)

parseLine :: Parser Particle
parseLine =
    do string (pack "p=")
       pos <- parseTriple
       skipWhile (inClass ", ")
       string (pack "v=")
       vel <- parseTriple
       skipWhile (inClass ", ")
       string (pack "a=")
       acc <- parseTriple
       return $ Particle pos vel acc

parseTriple :: Parser (Int, Int, Int)
parseTriple =
    do char '<'
       x <- signed decimal
       skip (== ',')
       y <- signed decimal
       skip (== ',')
       z <- signed decimal
       char '>'
       return (x, y, z)

-- Вычисляет позицию частицы через time интервалов
translate :: Int -> Particle -> Particle
translate time (Particle (x, y, z) (vx, vy, vz) a@(ax, ay, az)) =
    let evolve co vel acc = co + time*vel + div (acc*time*(1 + time)) 2
        accel vel acc = vel + time*acc
     in Particle (evolve x vx ax, evolve y vy ay, evolve z vz az)
                 (accel vx ax, accel vy ay, accel vz az) a

evolution :: Int -- | Время
          -> Int -- | Начальная координата
          -> Int -- | Начальная скорость
          -> Int -- | Ускорение
          -> Int
--evolution co vel acc time = co + time*vel + div (acc*time*(1 + time)) 2
evolution time co vel acc = co + time*(vel + div acc 2) + (time*time*acc `div` 2)

stopPoint :: Particle -> Maybe Int
stopPoint (Particle co (vx,vy,vz) (ax,ay,az)) =
    let tx = - vx `div` ax
        ty = - vy `div` ay
        tz = - vz `div` az
        max = maximum [tx,ty,tz]
     in if max >= 0
           then Just max
           else Nothing

scalarProd :: (Int, Int, Int) -> (Int,Int,Int) -> Int
scalarProd (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

distFromZero :: Int -> Particle -> Int
distFromZero time (Particle (x, y, z) (vx,vy,vz) (ax, ay, az)) =
    let xt = evolution time x vx ax
        yt = evolution time y vy ay
        zt = evolution time z vz az
     in abs xt + abs yt + abs zt

-- | Вычисляет минимум скалярного произведения ускорения на радиус-вектор.
-- А ещё вычисляет позднее прохождение через 0, если оно существует.
-- Это позволяет найти момент времени, когда угол между радиус-вектором и вектором
-- ускорения начинает уменьшаться. А также момент времени, когда этот угол
-- становится меньше 90 градусов
rvMin :: Particle -> (Int, Maybe Int)
rvMin (Particle (x, y, z) (vx, vy, vz) a@(ax, ay, az)) =
    let input = fmap (fmap toEnum) [Triple x vx ax, Triple y vy ay, Triple z vz az]
        --input = [(x, vx, ax), (y, vy, ay), (z, vz, az)]
        --c1 = (ax*x + ay*y + az*z)
        f1 (Triple c _ a) = a*c
        f2 (Triple _ v a) = a*(v + a / 2)
        f3 (Triple _ _ a) = a^2 / 2
        -- (c1 + c2 * x + c3 * x^2)
        foo = (sum $ fmap f1 input, sum $ fmap f2 input, sum $ fmap f3 input) :: (Float, Float, Float)
        (c1, c2, c3) = {-trace'-} foo
        pointMin = ceiling $ (-c2) / (2*c3)
        discriminant = c2^2 - 4 * c3*c1
        root = if | discriminant > 0 -> Just . ceiling $ ((-c2) + sqrt discriminant) / (2 * c3)
                  | discriminant == 0 -> Just pointMin
                  | otherwise -> Nothing
     in {-trace'-} (max 0 pointMin, root)

borderPoint :: (Int, Maybe Int) -> Int
borderPoint (t1, m) =
    case m of
         Just t2 -> max t2 t1
         Nothing -> t1

