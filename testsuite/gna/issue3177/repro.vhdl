library ieee;
use ieee.std_logic_1164.all;

entity A is
  port (clk : in std_logic);
end A;

architecture str of A is
  signal C : std_logic_vector(7 downto 0);
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

architecture sim of repro is
  signal clk : std_logic := '1';
begin

  DUT : entity work.A(str)
    port map(clk => clk);

  b1: block
    alias sig is << signal ^.DUT.Cerr : std_logic_vector >>;
    constant l : natural := sig'length;
  begin
    sig <= x"ff";
  end block;
end architecture;
