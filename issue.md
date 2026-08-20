# Issue #2955 — Spurious bound-check failure on an element-wise port association

Analysis by an AI agent.

## High-level summary

The reporter's design associates the elements of an array port individually:

```vhdl
i_dut : entity work.dummy
  generic map (g_bit_width => c_bit_width, g_num_chnl => c_num_chnl)
  port map (i_data(0) => i_data,
            o_data(0) => o_data);
```

`i_data` is declared `signed_vector(0 to g_num_chnl - 1)`, where `signed_vector` is `array (natural range <>) of signed` — the index range is constrained but the *element* is not; it takes its constraint from the actual. On the gcc and llvm backends this fails elaboration:

```
error: bound check failure at repro.vhd:55
error: error during elaboration
```

mcode accepts it, and the reporter noted it *"does compile fine in Modelsim and Riviera"*. Constraining the element in the port declaration, or associating the port as a whole, both work — which is why this shape is the one that matters.

## What is actually wrong in GHDL's implementation

For an individual (element-wise) association, `Finish_Individual_Assoc_Array` (`src/vhdl/vhdl-sem_assocs.adb`) synthesizes the subtype of the actual. It computes the *index* constraints from the individual associations, and `Finish_Individual_Association1` marks the synthesized subtype `Fully_Constrained`:

```ada
Ntype := Create_Array_Subtype (Atype, Get_Location (Assoc));
Set_Index_Constraint_Flag (Ntype, True);
Set_Constraint_State (Ntype, Fully_Constrained);
```

Nothing ever constrains the element. When the interface's element type is unconstrained the synthesized subtype keeps the base type's unconstrained element while claiming to be fully constrained, so the element's bounds are never elaborated and the run-time check on the element association compares against bounds that were never set.

The record case is already handled: `Finish_Individual_Assoc_Record` builds a `Record_Element_Constraint` for each element that is not fully constrained, taking its subtype from the associated actual. The array case simply had no equivalent for its element.

## What the fix does

Gives the array case the same treatment as the record case: when the element of the interface is not fully constrained, take the element subtype from the individual associations — they all share one — and set it on the synthesized subtype, which makes the `Fully_Constrained` marking true. `Element_Subtype` is a `Ref` field, so this refers to the actual's subtype without transferring ownership.

## Testing

`testsuite/gna/issue2955/repro.vhd` is the reporter's own design, elaborated and simulated. `chk.vhd` drives values across the element-wise association and checks them, because not crashing does not show the element bounds are right:

```
PASS b=9876 len=16
```

Identical on mcode, which accepts the design without the fix and so serves as a reference for the expected values.

The trigger is the unconstrained element, not the generic: with the element constrained in the port declaration the design elaborates on every backend, with or without this fix, whether the index range comes from a generic or a literal.

Full `sanity gna vests synth` pass on mcode, llvm and gcc.
