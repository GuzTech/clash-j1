module Stack3 where
import Clash.Prelude

--
-- Types
--
type Value  = Signed 16
type SizeB  = 3
type Depth  = 2^SizeB
type SP     = Unsigned SizeB
type StkMem = Vec Depth Value

data SInstr = Push
            | Pop
            | PopPush
            deriving (Show, Enum)

instance BitPack SInstr where
  type BitSize SInstr = 2
  pack i = case i of
    Push    -> pack (0 :: Unsigned 2)
    Pop     -> pack (1 :: Unsigned 2)
    PopPush -> pack (2 :: Unsigned 2)
  unpack bv = case bv of
    0 -> Push
    1 -> Pop
    2 -> PopPush

--
-- Functions
--
stk (sp, mem) (instr, v) = ((sp', mem'), o)
  where
    (sp', mem') = case instr of
      Push    -> (sp + 1, replace sp v mem)
      Pop     -> (sp - 1, mem)
      PopPush -> (sp, replace (sp - 1) v mem)
    o = case instr of
      Pop     -> mem !! sp'
      PopPush -> mem !! (sp - 1)
      _       -> 0

stack = mealy stk (0 :: SP, repeat 0 :: StkMem)

type Dom50 = Dom "System" 20000

{-# ANN topEntity
  (Synthesize
    { t_name   = "Stack3"
    , t_inputs = [PortName "i_clk", PortName "i_rst", PortName "i_instr", PortName "i_value"]
    , t_output = PortName "o_value"
    }) #-}
topEntity
--  :: SystemClockReset
  :: Clock Dom50 Source
  -> Reset Dom50 Synchronous
--  -> Signal Dom50 (SInstr, Value)
  -> Signal Dom50 SInstr
  -> Signal Dom50 Value
  -> Signal Dom50 Value
--topEntity = exposeClockReset stack
topEntity clk rst i v = o
  where
    s = bundle (i, v)
    o = exposeClockReset (stack s) clk rst
--    o = withClockReset clk rst stack s

--
-- Simulation
--
sim f s [] = []
sim f s (x:xs) = s':sim f s' xs
  where
    (s'@(sp', mem'), o) = f s x

instrs = [(Push, 24), (Push, 31), (PopPush, 6), (Pop, 0), (Pop, 0)]
test = sim stk (0 :: SP, repeat 0 :: StkMem) instrs

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
