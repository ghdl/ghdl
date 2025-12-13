library ieee;
use ieee.std_logic_1164.all;

entity loop06 is
  port (val : std_logic_vector(7 downto 0);
        res : out integer);
end;

architecture behav of loop06 is
  constant mask : std_logic_vector(7 downto 0) := b"0100_0010";

  function ffs (v : std_logic_vector(7 downto 0)) return integer
  is
    alias av : std_logic_vector(v'length - 1 downto 0) is v;
    variable idx : natural;
  begin
    L1: for i in 0 to 1 loop
      for j in 0 to 3 loop
        idx := 4*4+j;
        next L1 when mask(idx) = '1';
        if av (idx) = '1' then
          return idx;
        end if;
      end loop;
    end loop;
    return -1;
  end ffs;
begin
  res <= ffs(val);
end behav;
