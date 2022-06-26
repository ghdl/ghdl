library ieee;
use ieee.std_logic_1164.all;

entity top is
  port (
    clk : in std_logic;
    sum : out std_logic
  );
end entity;

architecture rtl of top is
begin

  adder : entity work.adder(comb)
  port map(
    a => clk,
    b => '1',
    o => sum,
    c => open
  );

end architecture;
