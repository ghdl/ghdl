# Issue #3129 — Discriminant assertion failed

Analysis by an AI agent.

## High-level summary

`ghdl --synth` crashed with a "constraint / discriminant" assertion failure on a design with an unstatic loop bound (`ceillog2`-style recursive/loop computation), reported while targeting a PolarFire FPGA part.

## Investigation / current status: already fixed and already tested upstream

Checked `master` for a pre-existing test first: `testsuite/synth/issue3129/` already exists (`unstatic_loop2.vhdl`, `unstatic_loop2_orig.vhdl`, `tb_unstatic_loop2.vhdl`), matching this issue, and it passes in full — simulated pre-synthesis, synthesized, the netlist re-analyzed and simulated again, and the original (un-modified) version simulated too, all producing consistent `ceillog2(...)` results with no crash.

So this is fixed and already has official regression coverage. No source change or new test needed.

## What the fix does

No source change and no new test file in this commit — `master` already has a complete, passing regression test for this issue at `testsuite/synth/issue3129/`.
