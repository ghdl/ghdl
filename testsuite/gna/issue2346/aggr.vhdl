library ieee ;
  use ieee.std_logic_1164.all ;
  use ieee.numeric_std_unsigned.all ;

entity AggregateWithDelay is
end AggregateWithDelay ;
architecture test of AggregateWithDelay is
  constant tpd         : time := 1 ns ;
  signal Clk           : std_logic := '0' ;
  signal A1, A0  : std_logic ;
begin

  Clk <= not Clk after 5 ns ; 

  DriveDataProc : process
    variable A : std_logic_vector(1 downto 0) ; 
  begin
    wait until Clk = '1';

    for i in 7 downto 0 loop
      (A1, A0) <= to_slv(i, 2) after tpd ;
-- The following is the work around
--      A := to_slv(i, 2) ;
--     A1 <= A(1) after tpd ;
--     A0 <= A(0) after tpd ;
      wait until Clk = '1' ; 
      wait until Clk = '1' ; 
    end loop ;
    std.env.stop ;
    wait ;
  end process ;
end;
