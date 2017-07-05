-- code from Composing Contracts: An Adventure in Financial Engineering
-- Simon Peyton Jones, Jean-Marc Eber, Julian Seward
-- copyright is of the authors

-- transcribing the code for the presentation, then later filling
-- in the holes with an actual implementation

c1 :: Contract
c1 = zcb t1 100 GBP

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = scaleK x (get (truncate t (one k))) -- p. "282"

date :: String -> Date
-- implementation here

t1,t2 :: Date
t1 = date "1530GMT 1 Jan 2010"
t2 = date "1200GMT 1 Feb 2010"

type Days = Double -- A time difference
diff :: Date -> Date -> Days
add :: Date -> Days -> Date

and :: Contract -> Contract -> Contract
-- implementation here

c2,c3 :: Contract
c2 = zcb t2 200 GBP
c3 = c1 `and` c2

give :: Contract -> Contract
-- implementation here

c4 = c1 `and` give c2

andGive :: Contract -> Contract -> Contract
andGive c d = c `and` give d

c4 = c1 `andGive` c2 -- redefining the prev c4

c5 = one GBP

c6 = get (truncate t1 (one GBP))

c7 = scaleK 100 (get (truncate t1 (one GBP)))

scaleK :: Double -> Contract -> Contract
scaleK x c = scale (konst x) c -- p. "283"


noonTempInLA :: Obs Double
libor3m :: Obs Double

c8 = scale noonTempInLA (one USD)

konst :: a -> Obs a

ntLAinKelvin :: Obs Double
ntLAinKelvin = noonTempInLA + konst 373

or :: Contract -> Contract -> Contract
-- implementation here

european :: Date -> Contract -> Contract
european t u = get (truncate t (u `or` zero)) -- p. "284"


c5 = european (date "24 Apr 2003") (
  zcb (date "12 May 2003") 0.4 GBP `and`
  zcb (date "12 May 2004") 9.3 GBP `and`
  zcb (date "12 May 2005") 109.3 GBP `and`
  give (zcb (date "26 Apr 2003") 100 GBP)
) 

perhaps :: Date -> Contract -> Contract
perhaps t u = truncate t (u `or` zero)

anytime :: Contract -> Contract

american :: (Date,Date) -> Contract -> Contract
american (t1,t2) u = get (truncate t1 opt) `then` opt
  where
    opt :: Contract
    opt = anytime (perhaps t2 u)

data Contract = One Date Currency
  | Give Contract
-- ...
-- p. "290"

eval :: Model -> Currency -> Contract -> ValProc

type ValProc = (TimeStep, [Slice])
type Slice = [Double]
