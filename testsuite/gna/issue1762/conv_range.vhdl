--  The conversion in the report's design targets BIT_VECTOR(DATA_WORD'RANGE),
--  which has exactly the range of the actual, so it cannot show where the
--  bounds of the formal come from.  Here the target subtype is deliberately
--  given a different range and direction from the actual, which makes the
--  distinction observable -- and which crashes in the same place without the
--  fix.
library ieee;
use ieee.numeric_bit.all;

entity probe_range is
  port (input : in bit_vector);
end entity;

architecture behav of probe_range is
begin
  process
    variable s   : string(1 to 16);
    variable idx : integer;
  begin
    wait for 1 ns;
    --  Walk the vector from 'LEFT to 'RIGHT, so the reported sequence does
    --  not depend on how the bounds are labelled.  The labels themselves are
    --  deliberately not checked here: the backends disagree about them for
    --  this construct, which is a separate matter from this crash.
    for i in 0 to input'length - 1 loop
      if input'ascending then idx := input'left + i; else idx := input'left - i; end if;
      if input (idx) = '1' then s (i + 1) := '1'; else s (i + 1) := '0'; end if;
    end loop;
    report "pos(left..right)=" & s & " len=" & integer'image (input'length);
    wait;
  end process;
end architecture;

library ieee;
use ieee.numeric_bit.all;

entity tb_conv_range is
end entity;

architecture test of tb_conv_range is
  subtype data_word is unsigned (15 downto 0);   --  descending
  subtype bv_asc    is bit_vector (0 to 15);     --  ascending target
  --  16#F001# is not a palindrome, so a reversal would be visible.
  signal s_result : data_word := to_unsigned (16#F001#, 16);
begin
  u : entity work.probe_range port map (input => bv_asc (s_result));
end architecture;
