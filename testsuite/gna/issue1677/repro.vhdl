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
