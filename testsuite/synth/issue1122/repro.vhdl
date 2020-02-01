entity repro is
  port (clk : bit;
        rst : bit;
        d : bit_vector (7 downto 0);
        q : out bit_vector (7 downto 0));
end repro;

architecture behav of repro is
  constant c : bit_vector (7 downto 0) := x"7e";
  signal s : bit_vector (7 downto 0) := c;
begin
  process (clk)
  begin
    if rst = '1' then
      s <= c;
    elsif clk = '1' and clk'event then
      s <= d;
    end if;
  end process;

  q <= s;
end behav;

--  For this design:
--    %3:$o[8]{n7w8} := 8'uh7e
--    \s:$o[8]{n8w8} := $isignal{i7} (
--      .$i: %8:$o[8]{n12w8} := $mux2{i11} (
--          .$s: \rst{n2w1},
--          .$i0: %7:$o[8]{n11w8} := $mux2{i10} (
--              .$s: %6:$o{n10w1} := $edge{i9} (
--                  .$i: \clk{n1w1}),
--              .$i0: \s:$o{n8w8},
--              .$i1: \d{n3w8}),
--          .$i1: %3:$o{n7w8}),
--      .$init: %3:$o{n7w8})
--    \q := \s:$o{n8w8}

--  For repro2:
--    \s:$o{n8w1} := $isignal{i7} (
--      .$i: %9:$o{n13w1} := $mux2{i12} (
--          .$s: \rst{n2w1},
--          .$i0: %8:$o{n12w1} := $mux2{i11} (
--              .$s: %7:$o{n11w1} := $edge{i10} (
--                  .$i: \clk{n1w1}),
--              .$i0: \s:$o{n8w1},
--              .$i1: \d{n3w1}),
--          .$i1: %6:$o{n10w1} := 1'uh1),
--      .$init: %3:$o{n7w1} := 1'uh1)
--    \q := \s:$o{n8w1}

-->

--    %3:$o{n7w1} := 1'uh1
--    \s:$o{n8w1} := $isignal{i7} (
--      .$i: %10:$q{n14w1} := $iadff{i13} (
--          .$clk: \clk{n1w1},
--          .$d: \d{n3w1},
--          .$rst: \rst{n2w1},
--          .$rst_val: %6:$o{n10w1} := 1'uh1,
--          .$init: %3:$o{n7w1}),
--      .$init: %3:$o{n7w1})
--    \q := \s:$o{n8w1}
