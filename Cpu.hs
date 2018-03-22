module Cpu where
import CLaSH.Prelude

import Types
import Stack
import ALU

--
-- Types
--
type LitSize = 15
type LitBV   = BitVector LitSize
type Literal = Unsigned LitSize

type ALUFields = (Bool,     -- R -> PC
                  ALU,      -- ALU opcode
                  Bool,     -- T -> N
                  Bool,     -- T -> R
                  Bool,     -- N -> T
                  Signed 2, -- Rstack +-
                  Signed 2) -- Dstack +-
data Instr = ILit Literal
           | IJmp Address
           | ICJmp Address
           | ICall Address
           | IALU ALUFields
           deriving (Show, Eq)

instance BitPack Instr where
  type BitSize Instr = 16
  pack i = case i of
    ILit l  -> (1 :: Bit) ++# pack l
    IJmp a  -> (0 :: BitVector 3) ++# pack a
    ICJmp a -> (1 :: BitVector 3) ++# pack a
    ICall a -> (2 :: BitVector 3) ++# pack a
    IALU (r_pc, t, t_n, t_r, nt, rst, dst) ->
      (3 :: BitVector 3) ++# pack (r_pc, t, t_n, t_r, nt, (0 :: Bit), rst, dst)
  unpack bv = i
    where
      (type1, lit) = (split bv) :: (Bit, LitBV)          -- Encoding Type 1: Literal
      (type2, a)   = (split bv) :: (BitVector 3, AddrBV) -- Encoding Type 2: (C)Jmp, Call, ALU
      (r_pc, t, t_n, t_r, nt, u, rst, dst) = (unpack a) :: (Bool, ALU, Bool, Bool, Bool, Bool, Signed 2, Signed 2)
      addr = (unpack a) :: Address
      i = case (type1, type2) of
        (1 :: Bit, _)         -> (ILit ((unpack lit) :: Literal))
        (_, 0 :: BitVector 3) -> (IJmp addr)
        (_, 1 :: BitVector 3) -> (ICJmp addr)
        (_, 2 :: BitVector 3) -> (ICall addr)
        (_, 3 :: BitVector 3) -> (IALU (r_pc, t, t_n, t_r, nt, rst, dst))

--
-- Functions
--
decode_pc :: Address -> (Instr, Bool) -> (Address, Address)
decode_pc pc (instr, t) = (pc', pc')
  where
    pc' = case instr of
      IJmp a  -> a
      ICall a -> a
      ICJmp a -> case t of
                   True -> a
                   False -> pc + 1
      _       -> pc + 1

calc_sptrs :: (SP, SP) -> (Signed 2, Signed 2) -> ((SP, SP), (SP, SP))
calc_sptrs (rsp, dsp) (rst, dst) = ((rsp', dsp'), (rsp', dsp'))
  where
    se_rst = signExtend rst :: Signed SizeB
    se_dst = signExtend dst :: Signed SizeB
    r = (unpack (pack se_rst)) + rsp
    d = (unpack (pack se_dst)) + dsp
    (rsp', dsp') = (r, d)

decode_st0N st0 (instr, t) = (st0N, st0N)
  where
    st0N = case instr of
      ILit l  -> unpack ((0 :: Bit) ++# (pack l)) :: Value
      IALU _  -> t
      _       -> st0

system :: BitV -> Value -> Signal (SP, SP, DstMem, Address, Value, Value, Value, Bool)
system instr_bv tref = o
  where
    -- Decode the instruction
    instr = (unpack instr_bv) :: Instr

    -- Get bit fields if the instruction is an ALU instruction
    (r_pc, alu_op, t_n, t_r, n_t, rst, dst) = case instr of
      ILit _                              -> (False, T, False, False, False, 0, 1)
      ICall _                             -> (False, T, False, False, False, 1, 0)
      IALU (rpc, t, tn, tr, nt, rst, dst) -> (rpc, t, tn, tr, nt, rst, dst)
      _                                   -> (False, T, False, False, False, 0, 0)

    -- Calculate new rsp and dsp
    (rsp, dsp) = unbundle $ mealy calc_sptrs (-1, -1) (pure (rst, dst))

    -- Determine if we want to push on to the data stack
    dstkW = case instr of
      ILit _ -> True
      IALU _ -> t_n
      _      -> False

    -- Determine if we want to push on to the return stack
    rstkW = case instr of
      ICall _ -> True
      IALU _  -> t_r
      _       -> False

    -- Determine the data to push onto the return stack
    tosAsAddrBV16 = pack <$> tos
    tosAsAddrBV13 = (resize <$> tosAsAddrBV16) :: Signal (BitVector 13)
    rstkD = case instr of
      ICall a -> pure a
      _       -> unpack <$> tosAsAddrBV13

    -- Calculate the new program counter
    pc = mealy decode_pc 0 (bundle (pure instr, unpack <$> (lsb <$> (pack <$> tos))))

    -- Get the top of stack and next on stack values for the data stack, and top of the return stack
    (dmem, tos, nos) = unbundle $ mealy st (repeat 4 :: DstMem) $ bundle (dsp, pure dstkW, st0N)
    (_, r, _)     = unbundle $ mealy st (repeat 0 :: RstMem) $ bundle (rsp, pure rstkW, (pc + 1))

    -- Perform the ALU operation
    tos' = alu <$> tos <*> nos <*> r <*> (pure tref) <*> (pure alu_op)

    -- Determine data stack value to write
    st0N = mealy decode_st0N (0 :: Value) $ bundle (pure instr, tos')

    -- Output
    o = bundle (rsp, dsp, dmem, pc, st0N, tos, nos, pure dstkW)

{-# ANN topEntity
  (defTop
    { t_name    = "cl_j1a"
    , t_inputs  = ["i_instr", "i_tref"]
    , t_outputs = ["o_rsp", "o_dsp", "o_pc", "o_tosN", "o_tos", "o_nos", "o_dstkW"]
}) #-}
topEntity = system

x1 = pack (ILit 15)
x2 = pack (IALU (False, TplusN, False, False, False, 0, -1))
x3 = pack (IJmp 24)
z1 = system x1 0
z2 = system x2 0
z3 = system x3 0
