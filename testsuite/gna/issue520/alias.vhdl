library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alias_extname_driving_signal is
  port(
    clk : in std_logic
  );
end alias_extname_driving_signal;

architecture primary of alias_extname_driving_signal is
  signal counter : unsigned(15 downto 0) := (others => '0');
begin
 counter <= (counter + 1) when rising_edge(clk);
end architecture primary;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alias_tb is
end alias_tb;

architecture primary of alias_tb is
  signal clk : std_logic := '0';
  signal vector16 : unsigned(15 downto 0);
begin
  clk <= not clk after 10 ns;

  uut : entity work.alias_extname_driving_signal
    port map(
      clk => clk
    );

  blk: block
    alias counter_alias is << signal .alias_tb.uut.counter : unsigned(15 downto 0) >>;
  begin
    vector16 <= counter_alias;
  end block;
end architecture primary;
