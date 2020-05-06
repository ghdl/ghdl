library ieee;
use ieee.std_logic_1164.all;

entity testcase is
   port (
          din    : in  std_logic_vector(3 downto 0);
          dout   : out std_logic_vector(1 downto 0)
        );
end testcase;

architecture behavior of testcase is
   signal testidx : natural range 3 downto 2;
begin

--------------------------------------------------------------
-- tc0 does not cause an overflow error
--tc0: process(din)
--     begin
--        if (din(3)='1') then
--           dout <= din(2 downto 1);
--        else
--           dout <= din(1 downto 0);
--        end if;
--     end process;
--------------------------------------------------------------

--------------------------------------------------------------
-- tc1 with the dout assignment does cause an overflow error
tc1: process(din)
     begin
        if (din(3)='1') then
           testidx <= 3;
        else
           testidx <= 2;
        end if;
     end process;

   dout   <= din(testidx-1 downto testidx-dout'length);
--------------------------------------------------------------

end behavior;
