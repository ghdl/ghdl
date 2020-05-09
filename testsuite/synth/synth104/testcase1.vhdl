library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testcase1 is
  port (
    sel : in unsigned(1 downto 0);
    det : out std_logic
    );
end testcase1;

architecture behavior of testcase1 is
begin
  tc: process(sel)
  begin
    case to_integer(sel) is
      when 0 to 1 =>
        det <= '0';
      when others =>
        det <= '1';
    end case;
  end process;
end behavior;
