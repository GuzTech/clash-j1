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
           | IALU Bool (Unsigned 4) Bool Bool Bool (Signed 2) (Signed 2)
           deriving (Show, Eq)

instance BitPack Instr where
  type BitSize Instr = 16
  pack i = case i of
    ILit l  -> (1 :: Bit) ++# pack l
    IJmp a  -> (0 :: BitVector 3) ++# pack a
    ICJmp a -> (1 :: BitVector 3) ++# pack a
    ICall a -> (2 :: BitVector 3) ++# pack a
    IALU rpc t tn tr nt rst dst ->
      (3 :: BitVector 3) ++# pack rpc ++# pack t ++# pack tn ++# pack tr ++# pack nt ++# pack False ++# pack rst ++# pack dst
  unpack bv = i
    where
      (type1, lit) = (split bv) :: (Bit, LitBV)          -- Encoding Type 1: Literal
      (type2, a)   = (split bv) :: (BitVector 3, AddrBV) -- Encoding Type 2: (C)Jmp, Call, ALU
      (rpc, rest0) = (split a) :: (Bit, BitVector 12)    -- R->PC field
      (t, rest1)   = (split rest0) :: (BitVector 4, BitVector 8) --ALU operation, replaces T
      (tn, rest2)  = (split rest1) :: (Bit, BitVector 7) -- T->N
      (tr, rest3)  = (split rest2) :: (Bit, BitVector 6) -- T->R
      (nt, rest4)  = (split rest3) :: (Bit, BitVector 5) -- N->T
      (u, rest5)   = (split rest4) :: (Bit, BitVector 4) -- Unused bit field
      (rst, dst)   = (split rest5) :: (BitVector 2, BitVector 2)
      --(rpc, t, tn, tr, nt, u, rst, dst) = (split a) :: (Bit, BitVector 4, Bit, Bit, Bit, Bit, BitVector 2, BitVector 2)
      (rpc', t', tn', tr', nt', rst', dst') = ((unpack rpc) :: Bool, (unpack t) :: Unsigned 4, (unpack tn) :: Bool, (unpack tr) :: Bool, (unpack nt) :: Bool, (unpack rst) :: Signed 2, (unpack dst) :: Signed 2)
      addr = (unpack a) :: Address
      i = case (type1, type2) of
        (1 :: Bit, _)         -> (ILit ((unpack lit) :: Literal))
        (_, 0 :: BitVector 3) -> (IJmp addr)
        (_, 1 :: BitVector 3) -> (ICJmp addr)
        (_, 2 :: BitVector 3) -> (ICall addr)
        (_, 3 :: BitVector 3) -> (IALU rpc' t' tn' tr' nt' rst' dst')

cpu :: (Address, SP) -> (BitVector 16) -> ((Address, SP), (Address, SP))
cpu (pc, sp) bv = ((pc', sp'), (pc', sp'))
  where
    i = (unpack bv) :: Instr
    (pc', sp') = case i of
      ILit l  -> (pc + 1, sp + 1)
      IJmp a  -> (a, sp)
      ICJmp a -> (a, sp)
      ICall a -> (a, sp)
      IALU rpc t tn tr nt rst dst -> (pc + 1, sp)

cpuM = mealy cpu (0 :: Address, 0 :: SP)
