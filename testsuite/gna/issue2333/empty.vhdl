library ieee;
use ieee.std_logic_1164.all;

entity abcdefg is
end entity abcdefg;

architecture arch of abcdefg is
begin
end architecture arch;

-------------------------------------------


library ieee;
--use ieee.numeric_std.all; --using a package from IEEE (even a different one) "fixes" the crash

library Bugtests;
use Bugtests.abcdefg;
entity abcdefg_tb is

end entity;
architecture rtl of abcdefg_tb is
begin
end architecture;
