library ieee;
use ieee.std_logic_1164.all;

entity func01 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end func01;

architecture behav of func01 is
  function gen_and (v : std_logic_vector (7 downto 0);
                    len : natural := 6) return std_logic_vector is
    variable res : std_logic_vector (7 downto 0);
  begin
    res := (others => '0');
    res (len - 1 downto 0) := (others => '1');
    return res and v;
  end gen_and;

begin
  b <= gen_and (a);
end behav;

