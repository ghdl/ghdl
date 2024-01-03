library ieee;
use ieee.std_logic_1164.all;

entity my_entity is
  generic (function my_func(i:std_logic) return std_logic);
  port (in1 : in std_logic;
  out1 : out std_logic);
end entity my_entity;

architecture bot of my_entity is
begin
  out1 <= my_func(in1);
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
  
  function my_not (i:std_logic) return std_logic is
  begin
    return not i;
  end function;

begin

  my_inst1 : entity work.my_entity(bot)
    generic map (my_func => my_not)
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
    report "Test passed ...";
    wait;
  end process;
  
  process
  begin
    wait for 3 ns;
     assert a = not b
      report "Output should be 'not' of input"
      severity failure;
    wait on b;
  end process;
  
end tb;
