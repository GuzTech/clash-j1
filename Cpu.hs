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
--           | IALU Address
           deriving (Show, Eq)

instance BitPack Instr where
  type BitSize Instr = 16
  pack i = case i of
    ILit l  -> (1 :: BitVector 1) ++# pack l
    IJmp a  -> (1 :: BitVector 3) ++# pack a
    ICJmp a -> (2 :: BitVector 3) ++# pack a
    ICall a -> (3 :: BitVector 3) ++# pack a
  unpack bv = i
    where
      (iLit, lit) = (split bv) :: (BitVector 1, LitBV)
      (iJmp, a)   = (split bv) :: (BitVector 3, AddrBV)
      addr = (unpack a) :: Address
      i = case (iLit, iJmp) of
        (1 :: BitVector 1, _) -> (ILit ((unpack lit) :: Literal))
        (_, 1 :: BitVector 3) -> (IJmp addr)
        (_, 2 :: BitVector 3) -> (ICJmp addr)
        (_, 3 :: BitVector 3) -> (ICall addr)

cpu :: (Address, SP) -> (BitVector 16) -> ((Address, SP), (Address, SP))
cpu (pc, sp) bv = ((pc', sp'), (pc', sp'))
  where
    i = (unpack bv) :: Instr
    (pc', sp') = case i of
      ILit l  -> (pc + 1, sp + 1)
      IJmp a  -> (a, sp)
      ICJmp a -> (a, sp)
      ICall a -> (a, sp)

cpuM = mealy cpu (0 :: Address, 0 :: SP)
