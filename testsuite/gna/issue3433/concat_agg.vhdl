library ieee;
use ieee.std_logic_1164.all;

entity concat_agg is
end entity;

architecture a of concat_agg is
  signal i       : std_ulogic_vector(31 downto 0) :=
    (12 => '1', 6 => '1', others => '0');
  signal direct  : std_ulogic_vector(5 downto 0);
  signal via_agg : std_ulogic_vector(11 downto 0);

  --  Concatenation whose only type is the aggregate type, even though the
  --  element type is itself an array.  This is not the #2650 ambiguity.
  type bv_arr is array (natural range <>) of std_ulogic_vector;
  signal part  : bv_arr(0 to 1)(7 downto 0) := (x"A5", x"5A");
  signal whole : bv_arr(0 to 3)(7 downto 0);
begin
  direct  <= i(12) & i(6 downto 2);
  via_agg <= std_ulogic_vector'(11 downto 6 => i(12),
                                5 downto 0 => i(12) & i(6 downto 2));
  whole   <= (0 to 3 => part & part);

  process
  begin
    wait for 1 ns;
    assert direct = "110000" severity failure;
    assert via_agg = x"FF0" severity failure;
    assert whole(0) = x"A5" and whole(1) = x"5A"
      and whole(2) = x"A5" and whole(3) = x"5A"
      severity failure;
    wait;
  end process;
end architecture;
