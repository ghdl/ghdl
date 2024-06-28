library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bugr is
    port ( a : in  std_logic_vector(1 downto 0);
           we : in  std_logic;
           rd : in  std_logic;
           d : in  std_logic_vector(1 downto 0);
           q : out  std_logic_vector(1 downto 0));
end;

architecture beh of bugr is
    type mem_type is array (1 to 0) of std_logic_vector(1 downto 0);
    signal mem : mem_type;

begin
    process(a, we, rd)
    begin
        if rd = '1' then
            q <= mem(to_integer(unsigned(a)));
        end if;
    end process;
end beh;
