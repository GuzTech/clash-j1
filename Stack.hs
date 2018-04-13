module Stack where
import Clash.Prelude
import Types

st mem (sp, push, v) = (mem', (mem', tos, nos))
  where
    mem' = case push of
      True -> replace sp v mem
      _    -> mem
    tos = mem' !! sp
    nos = mem' !! (sp - 1)

--decode sp i = (sp', sp')
--  where
--    sp' = case i of
--      True -> sp + 1
--      False -> sp

--topEntity :: Signal Bool -> Signal Value -> Signal (SP, DstMem, Value, Value)
--topEntity i v = o
--  where
--    (mem, o1, o2) = unbundle $ mealy st (repeat 0 :: DstMem) $ bundle (sp, i, v)
--    sp = mealy decode (-1 :: SP) i
--    o = bundle (sp, mem, o1, o2)
