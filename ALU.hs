module ALU where
import Clash.Prelude

import Types

data ALU = T        -- Top of stack (tos)
         | N        -- Next on stack (nos)
         | TplusN   -- T + N
         | TandN    -- T & N
         | TorN     -- T | N
         | TxorN    -- T ^ N
         | NegateT  -- ~T
         | NeqT     -- N == T
         | NltT     -- N < T
         | NrshiftT -- N >> T
         | TMinOne  -- T - 1
         | R        -- Top of return stack
         | TRef     -- [T]
         | NlshiftT -- N << T
         | Depth    -- depth
         | Nu_ltT   -- (unsigned)N < T
         deriving (Show, Eq)

instance BitPack ALU where
  type BitSize ALU = 4
  pack a = case a of
    T        -> 0  :: ALUType
    N        -> 1  :: ALUType
    TplusN   -> 2  :: ALUType
    TandN    -> 3  :: ALUType
    TorN     -> 4  :: ALUType
    TxorN    -> 5  :: ALUType
    NegateT  -> 6  :: ALUType
    NeqT     -> 7  :: ALUType
    NltT     -> 8  :: ALUType
    NrshiftT -> 9  :: ALUType
    TMinOne  -> 10 :: ALUType
    R        -> 11 :: ALUType
    TRef     -> 12 :: ALUType
    NlshiftT -> 13 :: ALUType
    Depth    -> 14 :: ALUType
    Nu_ltT   -> 15 :: ALUType
  unpack bv = case bv of
    0  -> T
    1  -> N
    2  -> TplusN
    3  -> TandN
    4  -> TorN
    5  -> TxorN
    6  -> NegateT
    7  -> NeqT
    8  -> NltT
    9  -> NrshiftT
    10 -> TMinOne
    11 -> R
    12 -> TRef
    13 -> NlshiftT
    14 -> Depth
    15 -> Nu_ltT

alu :: Value -> Value -> Address -> Value -> ALU -> Value
alu tos nos r tref op = case op of
  T        -> tos
  N        -> nos
  TplusN   -> (tos + nos)
  TandN    -> (.&.) tos nos
  TorN     -> (.|.) tos nos
  TxorN    -> (xor) tos nos
  NegateT  -> complement tos
  NeqT     -> (unpack $ boolToBV (tos == nos)) :: Value
  NltT     -> (unpack $ boolToBV (tos < nos)) :: Value
  NrshiftT -> (shiftR nos 1)
  TMinOne  -> tos - 1
  R        -> unpack $ (v2bv (repeat 0)) ++# (pack r)
  TRef     -> tref
  NlshiftT -> (shiftL nos 1)
  Depth    -> tos
  Nu_ltT   -> tos

--cpu :: Unsigned 1 -> ALU -> (Unsigned 1, Signal (Signed 16))
--cpu sp alu = (sp', z)
--  where
--    sp' = case alu of
--      TplusN -> (sp - 1)
----    (tos, nos) = unbundle $ stack (pure (sp, False, 0))
--    (tos, nos) = case alu of
--      TplusN -> unbundle $ stack (bundle (pure sp', pure True, z))
--    z = satPlus SatWrap <$> tos <*> nos
--

--cpu :: (Address, Unsigned 16, Bv16) -> Bv16 -> Bv16 -> (Bv16, Bv16, Bv16, Bool)
--cpu (pc, dsp, ) instr tref = o
--  where
--    (sp, push) = mealyB decoder 0
--    (tos, nos) = mealyB stk (repeat 0 :: Vec 8 (Signed 16)) (sp, push, v)
--    v = satPlus SatWrap <$> tos <*> nos
--
-- sampleN_lazy 10 $ (Data.List.!!) (sampleN_lazy 1 (cpuM (pure TplusN))) 0
-- sampleN_lazy 10 $ Data.List.head (sampleN_lazy 1 (cpuM (pure TplusN)))
