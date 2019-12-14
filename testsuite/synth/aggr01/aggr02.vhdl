library ieee;
use ieee.std_logic_1164.all;

entity aggr02 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end aggr02;

architecture behav of aggr02 is
  function gen_mask (len : natural) return std_logic_vector is
    variable res : std_logic_vector (len - 1 downto 0);
  begin
    res := (0 => '0', others => '1');
    return res;
  end gen_mask;

begin
  b <= a and gen_mask (8);
end behav;
