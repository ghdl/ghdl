library ieee;
use ieee.std_logic_1164.all;

entity func04 is
  port (a : std_logic_vector (7 downto 0);
        b : std_logic_vector (7 downto 0);
        r : out std_logic_vector (7 downto 0));
end func04;

architecture behav of func04 is
  function gen_mask (len : natural) return std_logic_vector is
    variable res : std_logic_vector (len - 1 downto 0);
  begin
    res := (0 | 1 => '0', others => '1');
    return res;
  end gen_mask;
  constant mask : std_logic_vector(7 downto 0) := not gen_mask (8);
begin
  r <= (a and mask) or (b and gen_mask (8));
end behav;
    
