library ieee;
use ieee.std_logic_1164.all;

entity test is
    generic (
        W: positive := 16;
        package P is new work.test_pkg
        generic map (W => W);
        X: integer := 7
    );
    port (
        clk: in std_logic;
        din: in P.word_t;
        dout: out P.word_t
    );
end entity;

architecture rtl of test is
begin
    process(clk)
    begin
        if rising_edge(clk) then
            dout <= P.inc(din);
        end if;
    end process;
end;
