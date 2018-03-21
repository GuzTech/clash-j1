module Types where
import CLaSH.Prelude

-- Generic
type IWidth = 16
type BitV   = BitVector IWidth
type Value  = Signed IWidth
type AddrSize = 13
type AddrBV   = BitVector AddrSize
type Address  = Unsigned AddrSize

-- Stack
type SizeB  = 3
type Size   = 2^SizeB
type SP     = Unsigned SizeB
type DstMem = Vec Size Value
type RstMem = Vec Size Address

-- ALU
type ALUSize = 4
type ALUType = BitVector ALUSize
