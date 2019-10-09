library ieee;
use ieee.std_logic_1164.all;

entity aggr02 is
  port (i0 : natural range 0 to 5;
        o : out std_logic_vector (7 downto 0));
end;

architecture behav of aggr02 is
  type my_enum5 is (e0, e1, e2, e3, e4);
  type my_rec is record
    en : my_enum5;
    cnt : std_logic_vector (3 downto 0);
    b : boolean;
    c : character;
  end record;

  type rec_arr is array (0 to 5) of my_rec;
  constant aggr : rec_arr :=
    (0 => (e0, x"0", false, '0'),
     1 => (e1, x"1", false, '1'),
     2 => (e2, x"2", false, '2'),
     3 => (e3, x"3", false, '3'),
     4 => (e4, x"4", false, '4'),
     5 => (e3, x"5", true, '5'));
begin
  process (i0)
    variable v : my_rec;
    variable r : my_enum5;
  begin
    v := aggr (i0);
    o(3 downto 0) <= v.cnt;
    r := e1;
    o(7 downto 4) <= x"0";
    if v.en = r then
      o (7) <= '1';
    end if;
  end process;
end behav;
