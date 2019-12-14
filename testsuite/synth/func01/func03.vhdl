library ieee;
use ieee.std_logic_1164.all;

entity func03 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end func03;

architecture behav of func03 is
  function gen_mask (len : natural) return std_logic_vector is
    variable res : std_logic_vector (len - 1 downto 0);
  begin
    res := (0 => '1', others => '0');
    return res;
  end gen_mask;
  constant mask : std_logic_vector(7 downto 0) := gen_mask (8);
begin
  b <= a and mask;
end behav;
    
