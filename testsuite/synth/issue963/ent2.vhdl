library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent2 is
    port (
        clk : in std_logic;
        set_7 : in std_logic;
        set_f : in std_logic;
        set_a : in std_logic;
        set_0 : in std_logic;
        q : out std_logic_vector(3 downto 0)
    );
end;

architecture a of ent2 is
    signal s : unsigned(3 downto 0);
begin
    process(clk, set_0, set_a, set_f, set_7)
    begin
        if set_0 = '1' then
            s <= x"0";
        elsif set_a = '1' then
            s <= x"a";
        elsif set_f = '1' then
            s <= x"f";
        elsif set_7 = '1' then
            s <= x"7";
        elsif rising_edge(clk) then
            s <= s + 1;
        end if;
    end process;
    q <= std_logic_vector(s);
end;
