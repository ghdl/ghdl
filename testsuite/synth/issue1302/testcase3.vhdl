library ieee;
use ieee.std_logic_1164.all;

entity testcase3 is
   generic (
             edge    : std_logic := '1'
           );
   port (
          clk    : in  std_logic;
          D      : in  std_logic;
          Q      : out std_logic
        );
end testcase3;

architecture behavior of testcase3 is
begin

tc3: process(clk)
     begin
        if (clk'event and clk=edge) then
           Q    <= D;
        end if;
     end process;

end behavior;
