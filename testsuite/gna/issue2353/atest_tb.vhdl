library ieee;
use ieee.std_logic_1164.all;

library osvvm;
library work;

entity atest_tb is
    generic (
        G_TB            : integer   := 1
    );
end entity;

architecture behv of atest_tb is

    package b_inst is new work.b generic map (G_TB);

    signal s_scoreboard : b_inst.scoreboard.ScoreboardIDType;

begin

    p1 : process is
    begin
        s_scoreboard    <= b_inst.scoreboard.NewID("FIFO");
        wait;
    end process p1;

end architecture;