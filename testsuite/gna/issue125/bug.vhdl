library ieee;
use ieee.std_logic_1164.all;
entity ent is end entity;
architecture a of ent is
begin
  process is
    alias logic is std_ulogic;
    function fun return string is
        variable v : std_ulogic_vector(0 to 3);
    begin
        if ( v = x"7" ) then return "was 7";
        else return "not 7"; end if;
    end function;
  begin
    report "yo: " & fun; wait;
  end process;
end architecture;
