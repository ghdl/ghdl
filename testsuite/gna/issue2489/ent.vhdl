library ieee ;
  use ieee.std_logic_1164.all ;
  use ieee.numeric_std.all ;
entity ent is
end entity;

architecture a of ent is
begin
  TestProc : process
    variable A_sv4 : signed(3 downto 0) ; 
    variable D : integer ; 
  begin
  
    report "test 1;  to_signed(11, 4)" ;
    A_sv4 := to_signed(11, 4);
 
    D := 11 ; 
    report "test 1;  D := " & to_string(D) & " to_signed(D, 4)" ;
    A_sv4 := to_signed(D, 4);
    
    -- std.env.stop;
    wait;
  end process;
end;

