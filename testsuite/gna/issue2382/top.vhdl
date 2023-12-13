library ieee;
use ieee.std_logic_1164.all;

entity my_entity is
  generic (type my_type);
  port (in1 : in my_type;
  out1 : out my_type);
end entity my_entity;

architecture bot of my_entity is
begin
  out1  <= in1;
end bot;

library ieee;
use ieee.std_logic_1164.all;
use work.all;

entity top is
end entity;

architecture tb of top is
  signal a : std_logic;
  signal b : std_logic;
  signal c : std_logic;
  signal d : std_logic;

  signal g : std_logic;
  signal h : bit;

begin

  my_inst1 : entity work.my_entity(bot)
    generic map (my_type => std_logic)
    port map(in1 => a, out1 => b);

  process
  begin
    for i in 0 to 10 loop
      g <= '1';
      a <= '0';
      wait for 15 ns;
      a <= '1';
      wait for 15 ns;
    end loop;
    wait;
  end process;
  
  process
  begin
    wait for 3 ns;
    report std_logic'image(a) & " < in:out > " & std_logic'image(b) & " cout ";
    wait on b;
  end process;
end tb;
