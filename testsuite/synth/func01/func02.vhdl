library ieee;
use ieee.std_logic_1164.all;

entity func02 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end func02;

architecture behav of func02 is
  function gen_mask (len : natural) return std_logic_vector is
    variable res : std_logic_vector (len - 1 downto 0);
  begin
    res := (0 => '1', others => '0');
    return res;
  end gen_mask;

begin
  b <= a and gen_mask (8);
end behav;
    
