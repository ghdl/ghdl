library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity case02 is
  port (
    sel : in unsigned(3 downto 0);
    det : out std_logic_vector(1 downto 0)
    );
end case02;

architecture behavior of case02 is
begin
  tc: process(sel)
  begin
    case to_integer(sel) is
      when 0 to 1 =>
        det <= "00";
      when 2 | 7 downto 4 =>
        det <= "01";
      when 3 | 12 downto 10 | 8 to 9 =>
        det <= "10";
      when others =>
        det <= "11";
    end case;
  end process;
end behavior;
