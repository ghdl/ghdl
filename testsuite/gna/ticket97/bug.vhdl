library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity foo is end entity;
architecture arch of foo is
begin
  process is
    procedure xyzzy( v : inout unsigned ) is
    begin
      v := v + 1;
    end procedure;

    variable x : std_ulogic_vector( 7 downto 0 );
  begin
    -- trying to do an inout conversion triggers the bug:
    xyzzy( std_ulogic_vector( v ) => unsigned( x ) );
    wait;
  end process;
end architecture;
