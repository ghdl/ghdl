library ieee;
use ieee.std_logic_1164.all;

entity loop04 is
  port (val : std_logic_vector(3 downto 0);
        res : out integer);
end;

architecture behav of loop04 is
  constant mask : std_logic_vector(3 downto 0) := "0100";

  function ffs (v : std_logic_vector) return integer
  is
    alias av : std_logic_vector(v'length - 1 downto 0) is v;
  begin
    L1: for i in av'reverse_range loop
      if av (i) = '1' then
        return i;
      end if;
      exit L1 when mask(i) = '1';
    end loop;
    return -1;
  end ffs;
begin
  res <= ffs(val);
end behav;
