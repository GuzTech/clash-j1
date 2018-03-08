module Stk where
import CLaSH.Prelude
import CLaSH.Sized.Internal.BitVector
import qualified Data.List as L

type Value  = Signed 16
type SizeB  = 3
type Depth  = 2^SizeB
type SP     = Unsigned SizeB
type StkMem = Vec Depth Value

data SInstr = Push Value
            | Pop
            | PopPush Value
            deriving Show

instance BitPack SInstr where
  type BitSize SInstr = 18
  pack i = case i of
    Push v    -> pack (0 :: Unsigned 2) ++# pack v
    Pop       -> pack (1 :: Unsigned 2) ++# pack (0 :: Value)
    PopPush v -> pack (2 :: Unsigned 2) ++# pack v

  unpack bv = insn
    where
      s = split bv :: (BitVector 2, BitVector 16)
      (i, v) = (unpack (fst s) :: Unsigned 2, unpack (snd s) :: Value)
      insn = case i of
        0 -> Push v
        1 -> Pop
        2 -> PopPush v

stk (sp, mem) instr = ((sp', mem'), o)
  where
    (sp', mem') = case instr of
      Push v    -> (sp + 1, replace sp v mem)
      Pop       -> (sp - 1, mem)
      PopPush v -> (sp, replace (sp - 1) v mem)
    o = case instr of
      Pop       -> mem !! sp'
      PopPush _ -> mem !! (sp - 1)
      _         -> 0

stack = mealy stk (0 :: SP, repeat 0 :: StkMem)

topEntity i = stack i

--sim f s [] = []
--sim f s (x:xs) = o:sim f s' xs
--  where
--    (_, o) = f s x

--sim f s (x:xs) = (s', o):sim f s' xs
--  where
--    (s', o) = f s x

--sim f s (x:xs) = s':sim f s' xs
--  where
--    (s'@(sp', mem'), o) = f s x

instrs = [Push 24, Push 31, PopPush 6, Pop, Pop]
--test = sim stk (0 :: SP, repeat 0 :: StkMem) instrs

-- To test the mealy version, the inputs should be Signals.
-- You can convert a type to a Signal by using "pure".
-- If in this case you did "stack (pure Pop))" you would get
-- an infinite list of zeroes. They are not a list, so you
-- cannot use "fst" or "head" on them, but you can use "sampleN".
--
-- sampleN :: (Control.DeepSeq.NFData a, Foldable f) => Int -> f a -> [a]
-- Control.DeepSeq.NFData is the infinite stream
-- f is some function that is foldable
--
-- It applies function f on your input "a" and gives you a Data.List of "a".

-- Doesn't work.
--testM = sim stk (0 :: SP, repeat 0 ::StkMem) (map pure instrs)
