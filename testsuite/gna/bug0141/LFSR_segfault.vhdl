-- LFSR_Segfault.vhdl
-- created mar. 06 mai 2025 23:26:35 CEST by whygee@f-cpu.org
--
-- Reproducer for an "undesired behaviour" of GHDL
-- suspected to be related to a clogged stack due to
-- inaccurate evaluation of an array's time-of-life
-- by the compiler. Concatenation is expected to be
-- "in place" instead of allocating more room each time.

Library ieee;
    use ieee.std_logic_1164.all;

entity LFSR_Segfault is
  generic (
    maxloop : integer := 250000;
    test_branch : integer := 0
  );
end LFSR_Segfault;

architecture plop of LFSR_Segfault is
begin

  bench: process
    variable l : std_logic;
    variable LFSR : std_logic_vector(16 downto 0) := "10011000010010101";

  begin
    report character'val(13) & " Trying to SEGFAULT the program with "
        & integer'image(maxloop) & " loops.         ";

    for i in 0 to maxloop loop
      if test_branch=0 then
        -- the offending code that does not free the old version of LFSR
        LFSR:=LFSR(15 downto 0) & (LFSR(16) xor LFSR(13));
      else
        -- In-place, scalar version that performs
        -- x^{17}+x^{14}+1 without exploding the stack
        l := LFSR(16) xor LFSR(13);
        for n in LFSR'high downto LFSR'low+1 loop
          LFSR(n):=LFSR(n-1);
        end loop;
        LFSR(0):=l;
      end if;
    end loop;

    report "OK";
    wait;
  end process;

end plop;
