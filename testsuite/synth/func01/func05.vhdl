library ieee;
use ieee.std_logic_1164.all;

entity func05 is
  port (s : natural;
        r : out std_logic_vector (15 downto 0));
end func05;

architecture behav of func05 is
  function mapv (sel : natural) return std_logic_vector
  is
    variable res : std_logic_vector(15 downto 0) := (others => '0');
  begin
    if sel = 2 then
      res := x"1234";
    end if;
    return res;
  end mapv; 
begin
  r <= mapv (s);
end behav;
    
