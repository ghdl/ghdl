library ieee;
use ieee.std_logic_1164.all;

entity A2 is
  port (clk : in std_logic);

  signal C : std_logic_vector(7 downto 0);
end A2;

architecture str of A2 is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
end;

architecture sim of repro2 is
  signal clk : std_logic := '1';
begin

  DUT : entity work.A2(str)
    port map(clk => clk);

  b1: block
    alias sig is << signal ^.DUT.C : std_logic_vector >>;
    constant l : natural := sig'length;
  begin
    sig <= x"ff";
  end block;
end architecture;
