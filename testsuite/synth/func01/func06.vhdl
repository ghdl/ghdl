library ieee;
use ieee.std_logic_1164.all;

entity func06 is
  port (s : natural;
        r : out std_logic_vector (15 downto 0));
end func06;

architecture behav of func06 is
  function mapv (sel : natural) return std_logic_vector
  is
    variable res : std_logic_vector(15 downto 0) := (others => '0');
  begin
    case sel is
      when 2 =>
        res := x"1234";
      when 3 =>
        res := x"5678";
      when others =>
        null;
    end case;
    return res;
  end mapv; 
begin
  r <= mapv (s);
end behav;
    
