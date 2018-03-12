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

data Instr = ILit Literal
           | IJmp Address
           | ICJmp Address
           | ICall Address
           | IALU (Bool,         -- R -> PC
                   (Unsigned 4), -- ALU opcode, replaces T
                   Bool,         -- T -> N
                   Bool,         -- T -> R
                   Bool,         -- N -> T
                   (Signed 2),   -- Rstack +-
                   (Signed 2))   -- Dstack +-
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
      (rpc, t, tn, tr, nt, u, rst, dst) = (unpack a) :: (Bool, Unsigned 4, Bool, Bool, Bool, Bool, Signed 2, Signed 2)
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
