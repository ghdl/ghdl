# Issue #1762 — Crash on a type conversion in a port association

https://github.com/ghdl/ghdl/issues/1762

## High-level summary

The design instantiates `reg` with a type conversion on the actual:

```vhdl
reg : entity work.reg
  port map (input => bv(s_result), ...);
```

`input` is an unconstrained `bit_vector` port and `s_result` is a `data_word`, i.e. `unsigned(15 downto 0)`. Elaborating that association crashed the gcc and llvm backends with an internal `ASSERT_FAILURE` in `trans-chap4.adb`. mcode is unaffected: it does not use the `src/vhdl/translate` translator this lives in.

## What is actually wrong in GHDL's implementation

`Elab_Conversion` (`src/vhdl/translate/trans-chap4.adb`) elaborates the conversion. `Elab_In_Conversion` passes the actual as its input and the formal as its output, so here the output is the unconstrained `input` port — unbounded — and the input is the bounded `data_word`. When the output is unbounded the code asserted that the input must be unbounded too:

```ada
if Out_Tinfo.Type_Mode in Type_Mode_Unbounded then
   --  The only reason why the output is unbounded is type conversion
   --  between two unbounded ports.
   pragma Assert (In_Tinfo.Type_Mode in Type_Mode_Unbounded);
```

That is not the only reason the output can be unbounded. A conversion between two unbounded ports is one case; an unconstrained formal that takes its constraint from the conversion is another, and that is this design. The premise fails and the assertion fires.

The assertion guards the branch that builds the destination bounds, and with assertions disabled GHDL does not stop — it continues into that branch with the precondition violated.

## What the fix does

Removes the assertion and corrects the comment to state both ways the output can be unbounded.

Nothing else is needed, because the branch already handles a bounded source. `Chap3.Get_Composite_Bounds` returns the bounds from the static layout for a `Type_Mode_Bounded_Arrays` operand, and `Chap7.Translate_Type_Conversion_Array_Bounds` works on Mnodes and types generically. The assumption was over-strict, not the code — checked before removing the assertion rather than deleting it because it was in the way.

## Testing

`testsuite/gna/issue1762/` covers two cases; both crash on master without the fix.

`test.vhd` is the design from the report. It is elaborated and simulated rather than only analyzed, because analysing alone cannot tell a correct conversion from one built on a violated precondition — which is what a `--disable-checks` build does here. `tb.vhd` writes `16#BEEF#` into `r1` and `16#1234#` into `r5` through the converted port and reads them back:

```
tb.vhd:41:5:@70ns:(report note): PASS a_out=48879 b_out=4660
```

`conv_range.vhdl` covers what the report's design cannot show. Its conversion targets `bit_vector(data_word'RANGE)`, which has exactly the range of the actual, so it cannot reveal where the bounds of the formal come from. The added case converts an `unsigned(15 downto 0)` actual to a `bit_vector(0 to 15)` target — a different range *and* direction — and checks the value that arrives, read from `'LEFT` to `'RIGHT` so the result does not depend on how the bounds are labelled:

```
pos(left..right)=1111000000000001 len=16
```

`16#F001#` is not a palindrome, so a reversal or misalignment would show. The bounds labels themselves are deliberately not asserted: the backends disagree about them for this construct, which is a separate matter from this crash.

Both cases pass on mcode, llvm and gcc, and with this fix alone applied to master. Full `sanity gna vests synth` pass on all three backends.
