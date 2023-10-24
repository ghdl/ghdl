library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity type1 is
end;

architecture behav of type1 is
  signal s : std_logic;
  signal b : bit;

  signal i32 : integer := -5;
  signal n32 : natural;
  signal t : time;
  signal c : character;
  signal se : severity_level;
  signal r : real;

  type mem_t is array (0 to 7) of std_logic_vector (15 downto 0);
  signal m1 : mem_t;

  type rec_t is record
    s : std_logic;
  end record;

  signal rec1 : rec_t;
begin
  process
  begin
    for i in 0 to 16 loop
      if s = '-' then
        s <= 'U';
      else
        s <= std_logic'Rightof (s);
      end if;
      b <= not b;
      i32 <= i32 + 1;
      n32 <= i;
      t <= now;
      c <= character'val (28 + i);
      m1 (i mod 8) <= std_logic_vector(to_unsigned(i, 16));
      r <= real (i) / 4.0;
      wait for 1 ns;
    end loop;
    wait;
  end process;
end behav;
