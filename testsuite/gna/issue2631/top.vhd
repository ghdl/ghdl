library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.protocol_pkg.T_ARRAY;

entity top is
  generic ( COUNTER_BITS : NATURAL );
end entity;

architecture impl of top is
  constant N_NODES : NATURAL := 6;

  signal counter_int : T_ARRAY(0 to 5)(COUNTER_BITS-1 downto 0);
begin

  node_inst: entity work.Node
    generic map (
      N_NODES => N_NODES,
      COUNTER_BITS => COUNTER_BITS )
    port map (
      -- bug occurs because counter_int'high is used. Replace counter_int'high with N_NODES-1 and it works.
      other_ctrs => counter_int(0 to 2) & counter_int(4 to counter_int'high),
      count => counter_int(3));

  process
  begin
    report "counter_int'high: " & INTEGER'image(counter_int'high) severity NOTE;
    report "N_NODES-1: " & NATURAL'image(N_NODES-1) severity NOTE;
    wait;
  end process;
end architecture;
