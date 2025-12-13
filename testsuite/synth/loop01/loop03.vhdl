library ieee;
use ieee.std_logic_1164.all;

entity loop03 is
  port (val : std_logic_vector(3 downto 0);
        res : out integer);
end;

architecture behav of loop03 is
  function ffs (v : std_logic_vector) return integer
  is
    alias av : std_logic_vector(v'length - 1 downto 0) is v;
    variable idx : natural := 0;
  begin
    idx := 0;
    loop
      exit when av(7) = '1';
      if av (idx) = '1' then
        return idx;
      end if;
      idx := idx + 1;
    end loop;
    return -1;
  end ffs;

  constant p : integer := ffs("0110");
begin
  res <= p;
end behav;
