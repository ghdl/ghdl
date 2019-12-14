library ieee;
use ieee.std_logic_1164.all;

entity func01 is
  generic (l : natural := 3);
  port (a : std_logic_vector (7 downto 0);
        sel : std_logic;
        b : out std_logic_vector (7 downto 0));
end func01;

architecture behav of func01 is
  function gen_mask (len : natural) return std_logic_vector is
    variable res : std_logic_vector (7 downto 0);
  begin
    res := (others => '0');
    res (len downto 0) := (others => '1');
    return res;
  end gen_mask;

begin
  process (a, sel)
  begin
    if sel = '1' then
      b <= a and gen_mask (l);
    else
      b <= a;
    end if;
  end process;
end behav;
    
