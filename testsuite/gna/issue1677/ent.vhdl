library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity saturatingadd is
  port(
	  result : out unsigned(7 downto 0)
  );
end saturatingadd;

architecture synth of saturatingadd is

begin

    process is
    begin
        result <= "00000000";
    end process;

end;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

entity ent is
end ent;

architecture test of ent is

    component saturatingadd is
      port(
    	  result : out unsigned(7 downto 0)
      );
    end component;

    signal result : unsigned(7 downto 0);

begin

  dut : saturatingadd port map(result);

  process is
  begin
    wait for 10 ns;
    write (output, "TEST PASSED." & LF);
    wait;
  end process;
end test;
