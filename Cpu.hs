module Cpu where
import CLaSH.Prelude

import Stack3

--
-- Types
--
type LitSize = 15
type LitBV   = BitVector LitSize
type Literal = Unsigned LitSize

type AddrSize = 13
type AddrBV   = BitVector AddrSize
type Address  = Unsigned AddrSize

type ALUtype  = BitVector 4
data ALU = T         -- Top of stack
         | N         -- Next on stack
         | TplusN    -- T + N
         | TandN     -- T & N
         | TorN      -- T | N
         | TxorN     -- T ^ N
         | NegateT   -- ~T
         | NeqT      -- N == T
         | NltT      -- N < T
         | NrshiftT  -- N right shift by T
         | TminusOne -- T - 1
         | R         -- R
         | RefT      -- [T]
         | NlshiftT  -- N left shift by T
         | Depth     -- depth
         | Nu_ltT    -- N unsigned < T
         deriving (Show, Eq)

instance BitPack ALU where
  type BitSize ALU = 4
  pack a = case a of
    T         ->  0 :: ALUtype
    N         ->  1 :: ALUtype
    TplusN    ->  2 :: ALUtype
    TandN     ->  3 :: ALUtype
    TorN      ->  4 :: ALUtype
    TxorN     ->  5 :: ALUtype
    NegateT   ->  6 :: ALUtype
    NeqT      ->  7 :: ALUtype
    NltT      ->  8 :: ALUtype
    NrshiftT  ->  9 :: ALUtype
    TminusOne -> 10 :: ALUtype
    R         -> 11 :: ALUtype
    RefT      -> 12 :: ALUtype
    NlshiftT  -> 13 :: ALUtype
    Depth     -> 14 :: ALUtype
    Nu_ltT    -> 15 :: ALUtype
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
    10 -> TminusOne
    11 -> R
    12 -> RefT
    13 -> NlshiftT
    14 -> Depth
    15 -> Nu_ltT

data Instr = ILit Literal
           | IJmp Address
           | ICJmp Address
           | ICall Address
           | IALU (Bool,       -- R -> PC
                   ALU,        -- ALU opcode, replaces T
                   Bool,       -- T -> N
                   Bool,       -- T -> R
                   Bool,       -- N -> T
                   (Signed 2), -- Rstack +-
                   (Signed 2)) -- Dstack +-
           deriving (Show, Eq)

instance BitPack Instr where
  type BitSize Instr = 16
  pack i = case i of
    ILit l  -> (1 :: Bit) ++# pack l
    IJmp a  -> (0 :: BitVector 3) ++# pack a
    ICJmp a -> (1 :: BitVector 3) ++# pack a
    ICall a -> (2 :: BitVector 3) ++# pack a
    IALU (rpc, t, tn, tr, nt, rst, dst) ->
      (3 :: BitVector 3) ++# pack (rpc, t, tn, tr, nt, (0 :: Bit), rst, dst)
  unpack bv = i
    where
      (type1, lit) = (split bv) :: (Bit, LitBV)          -- Encoding Type 1: Literal
      (type2, a)   = (split bv) :: (BitVector 3, AddrBV) -- Encoding Type 2: (C)Jmp, Call, ALU
      (rpc, t, tn, tr, nt, u, rst, dst) = (unpack a) :: (Bool, ALU, Bool, Bool, Bool, Bool, Signed 2, Signed 2)
      addr = (unpack a) :: Address
      i = case (type1, type2) of
        (1 :: Bit, _)         -> (ILit ((unpack lit) :: Literal))
        (_, 0 :: BitVector 3) -> (IJmp addr)
        (_, 1 :: BitVector 3) -> (ICJmp addr)
        (_, 2 :: BitVector 3) -> (ICall addr)
        (_, 3 :: BitVector 3) -> (IALU (rpc, t, tn, tr, nt, rst, dst))

cpu :: (Address, SP, SP) -> (BitVector 16) -> ((Address, SP, SP), (Address, SP, SP))
cpu (pc, rsp, dsp) bv = ((pc', rsp', dsp'), (pc', rsp', dsp'))
  where
    i = (unpack bv) :: Instr
    (pc', rsp', dsp') = case i of
      ILit l  -> (pc + 1, rsp, dsp + 1)
      IJmp a  -> (a, rsp, dsp)
      ICJmp a -> (a, rsp, dsp)
      ICall a -> (a, rsp + 1, dsp)
      IALU (rpc, t, tn, tr, nt, rst, dst) -> (pc + 1, r, d)
        where
          se_r = signExtend rst :: Signed 3
          se_d = signExtend dst :: Signed 3
          r = ((unpack (pack se_r)) :: Unsigned 3) + rsp
          d = ((unpack (pack se_d)) :: Unsigned 3) + dsp

cpuM = mealy cpu (0 :: Address, 0 :: SP, 0 :: SP)

topEntity = cpuM
