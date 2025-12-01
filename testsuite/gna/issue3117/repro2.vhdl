library ieee;
use ieee.std_logic_1164.all;

entity repro2_sub is
    generic (bit_count: positive);
    port (sum : in std_logic_vector(bit_count - 1 downto 0));
end entity;

architecture structural of repro2_sub is
begin
    -- ...
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2 is
  generic (width : positive := 4);
end entity;

architecture tb of repro2 is
  component repro2_sub is
    generic (bit_count: positive := 4);
    port (sum : in std_logic_vector(bit_count - 1 downto 0));
  end component;
  signal sum : unsigned(width -1 downto 0) := (others => '0');
begin
  UUT : repro2_sub
--    generic map(bit_countx => width)
    port map (
      sum => std_logic_vector(sum(width - 1 downto 0)));
end architecture;


configuration config of repro2 is
  for tb
    for UUT : repro2_sub
    use entity work.repro2_sub
    generic map (bit_countx => 3);
  end for;
  end for;
end configuration;
