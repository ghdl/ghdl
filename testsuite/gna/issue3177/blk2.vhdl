library ieee;
use ieee.std_logic_1164.all;

entity blk2 is
end;

architecture sim of blk2 is
  signal clk : std_logic := '1';
  function w return integer is
  begin
    return 5;
  end;
  signal s1 : std_logic_vector (w downto 0);
begin
  b1: block
    port (s : in std_logic_vector (3 downto 0));
    port map (s => s1);
  begin
    clk <= '1';
  end block;
end architecture;
