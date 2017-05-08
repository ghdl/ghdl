entity repro is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Test case architecture
architecture func of repro is

  signal s : std_logic := 'Z';

  procedure write (signal s : inout std_logic) is
  begin
    null;
  end write;
begin
  b: block
    port (s1 : out std_logic := '0');
    port map (s1 => s);
  begin
    process
    begin
      wait for 2 ns;
      s1 <= 'Z';
      wait;
    end process;
  end block;
  
  process
  begin
    write(s);
    wait for 1 ns;
    assert s = '0' severity failure;
    wait for 2 ns;
    assert s = 'Z' severity failure;
    wait;
  end process;
end func;
