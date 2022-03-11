--
-- test for overloaded "/" function
-- or signals of different size 
--

library ieee;
use ieee.std_logic_1164.all;
use work.t1_p.all;

entity tc is
        port (
     a : in std_logic_vector(1 downto 0);
     b : in std_logic_vector(3 downto 0);
     c : in std_logic_vector(2 downto 0);
     o : out std_logic_vector(3 downto 0));
end entity tc;


architecture rtl of tc is
begin
        --o <= a / b;   -- this works !
        o <=  a / b / c;   -- overloaded or function  <----  tc.vhd:19:17 
        -- o <= ("00" & a) or b or ("0" & C);
end architecture rtl;
