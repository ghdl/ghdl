library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro1 is
  port (a : std_logic_vector(31 downto 0);
        o : out std_logic_vector(5 downto 0));
end;

architecture behav of repro1 is
  function log2 (v : natural) return natural is
  begin
    for i in 0 to 31 loop
      if v <= 2**i then
        return i;
      end if;
    end loop;
    return 32;
  end log2;
  
  function clz(v : std_logic_vector) return unsigned is
    variable op : std_logic_vector(v'length - 1 downto 0) := v;
    variable cnt_top : integer := log2(op'length);
    variable cnt     : unsigned(cnt_top downto 0);
  begin
    if v'length = 1 then
      cnt(0) := not op(0);
    else
      if op(op'left / 2 downto 0) /= (op'left / 2 downto 0 => '0') then
        cnt := "0" & clz(op(op'left / 2 downto 0));
      else
        cnt := clz(op(op'left downto op'left / 2 + 1))
               + to_unsigned(cnt_top, cnt_top + 1);
      end if;
    end if;
    return cnt;
  end clz;

  function my_conv (v : std_logic_vector(31 downto 0)) return std_logic_vector
  is
    variable r : unsigned(5 downto 0) := clz (v);
    variable r2 : std_logic_vector(5 downto 0) := std_logic_vector(r);
  begin
    return r2;
  end my_conv;
  
begin
  o <= my_conv(a);
end behav;

