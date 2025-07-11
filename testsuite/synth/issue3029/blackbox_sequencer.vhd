library ieee;
use ieee.std_logic_1164.all;

entity blackbox_sequencer is
    port (
        clk : in  std_logic;
        a   : out std_logic;
        b   : out std_logic;
        c   : out std_logic;
        d   : out std_logic;
        e   : out std_logic
    );
end entity blackbox_sequencer;


architecture struct of blackbox_sequencer is

begin

    --                                          012345678901
    seq_a : entity work.sequencer generic map ("_-____-_______") port map (clk, a);
    seq_b : entity work.sequencer generic map ("____---__---___") port map (clk, b);
    seq_c : entity work.sequencer generic map ("__--___--_____") port map (clk, c);
    seq_d : entity work.sequencer generic map ("__-_-__-_-____") port map (clk, d);
    seq_e : entity work.sequencer generic map ("___-____-_____") port map (clk, e);


end architecture struct;
