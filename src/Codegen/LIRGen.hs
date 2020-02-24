module Codegen.LIRGen where

{-|

For now, this module will only do codegen for loads, stores, calls, and 'ops' generally.
This is because we're doing TV of the register allocator; the semantics of most of the
instructions don't matter.
https://xavierleroy.org/publi/validation-regalloc.pdf

This is the RTL from the above language that they use for TV of the regalloc:

CFGs:
  g ::= p -> I (finite map)

CFG nodes:
  p,s E N

RTL instructions:

  I ::= nop(s)                     -- no operation
    | op(op, x, xd, s)             -- arith
    | load(k, mode, x, xd, s)      -- load
    | store(k, mode, x, xs, s)     -- store
    | call(T, id, x, xd, s)        -- call
    | cond(cond, x, strue, sfalse) -- branch
    | return(x)                    -- return


We obviously have a bunch more instructions in the LIR (many different kinds of
store, for example), but this is the general set for which we'll be modeling
semantics.

-}
