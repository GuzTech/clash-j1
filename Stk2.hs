module Stack where
import CLaSH.Prelude

type Value = Signed 16
type SizeB = 3
type Size  = 2^SizeB
type SP    = Unsigned SizeB
data Instr = Push Value
           | Pop
           | PushPop Value
           | None
           deriving (Show)

--st :: (Unsigned 1, Vec 2 Value) -> Instr -> ((Unsigned 1, Vec 2 Value), Value)
st (sp, mem) instr = ((sp', mem'), o)
  where
    (sp', mem') = case instr of
      Push v    -> (sp+1, replace sp v mem)
      Pop       -> (sp-1, mem)
      PushPop v -> (sp, replace (sp-1) v mem)
      None      -> (sp, mem)
    o = case instr of
      Pop       -> mem!!sp'
      PushPop _ -> mem!!(sp-1)
      _         -> 0

getOutput ((sp, mem), o) = (sp, mem)

instrs = (Push 4 :> Push 16 :> PushPop 3 :> Pop :> Pop :> None :> Nil)

cpu = map (st (0 :: SP, repeat 0 :: Vec Size Value)) instrs
o0 = st (0 :: SP, repeat 0 :: Vec Size Value) (Push 4)
o1 = st (getOutput o0) (Push 16)
o2 = st (getOutput o1) (PushPop 3)
o3 = st (getOutput o2) Pop
o4 = st (getOutput o3) Pop

--stack = mealy st (0 :: Unsigned 1, (0 :> 0:> Nil))
--stack = mealy st (0 :: Unsigned 1, replicate d2 0)
stack = mealy st (0 :: SP, repeat 0 :: Vec Size Value)
--stack = mealy st (0 :: Unsigned 1, $(listToVecTH [0 :: Value, 0])) -- Not allowed to use "Value" as type in listToVecTH

--cpu2 = map stack instrs

{-# ANN topEntity
  (defTop
    { t_name    = "stack"
    , t_inputs  = ["i_instr"]
    , t_outputs = ["o_data"]
--    , t_extraIn = [("i_clk", 1)
--                  ,("i_rst", 1)]
}) #-}
topEntity = stack

testInput = stimuliGenerator instrs
expectedOutput = outputVerifier ((0 :: Value) :> 0 :> 16 :> 3 :> 4 :> 0 :> Nil)

test = sampleN 6 $ expectedOutput (topEntity testInput)

data StInsn = SPush (Signed 16)
            | SPop
            | SPopPush (Signed 16)
            deriving Show

st2 (sp, mem) insn = ((sp', mem'), o)
  where
    (sp', mem') = case insn of
      SPush v    -> (sp + 1, replace sp v mem)
      SPop       -> (sp - 1, mem)
      SPopPush v -> (sp, replace (sp - 1) v mem)
    o = case insn of
      SPop       -> mem !! sp'
      SPopPush _ -> mem !! (sp - 1)
      _          -> 0

getState ((sp, mem), o) = (sp, mem)

stack2 = mealy st2 (0 :: Unsigned 1, repeat 0 :: Vec 2 (Signed 16))
i = stimuliGenerator (SPush 4 :> SPush 24 :> SPopPush 3 :> SPop :> SPop :> Nil)
e = outputVerifier ((0 :: Signed 16) :> 0 :> 24 :> 3 :> 4 :> Nil)
t = sampleN 5 $ e (stack2 i)
