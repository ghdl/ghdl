library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_unaries is
end;

architecture behav of tb_unaries is
  type sl_map_type is array (std_ulogic) of character;
  constant sl_map : sl_map_type := "UX01ZWLH-";

  function to_string(v : std_logic_vector) return string
  is
    alias av : std_logic_vector(1 to v'length) is v;
    variable res : string (1 to v'length);
  begin
    for i in res'range loop
      res (i) := sl_map (av (i));
    end loop;
    return res;
  end to_string;

  signal li : integer;
  signal l4        : std_logic_vector (3 downto 0);
  signal plus_u4u  : std_logic_vector (3 downto 0);
  signal plus_s4s  : std_logic_vector (3 downto 0);
  signal minus_s4s : std_logic_vector (3 downto 0);
  signal abs_s4s   : std_logic_vector (3 downto 0);
  signal plus_u4v  : std_logic_vector (3 downto 0);
  signal plus_s4v  : std_logic_vector (3 downto 0);
  signal minus_s4v : std_logic_vector (3 downto 0);
  signal abs_s4v   : std_logic_vector (3 downto 0);
begin
  dut: entity work.unaries
    port map (
      l4        => l4,
      plus_u4u  => plus_u4u,
      plus_s4s  => plus_s4s,
      minus_s4s => minus_s4s,
      abs_s4s   => abs_s4s,

      plus_u4v  => plus_u4v,
      plus_s4v  => plus_s4v,
      minus_s4v => minus_s4v,
      abs_s4v   => abs_s4v);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      wait for 1 ns;
      report "u4u: + " & integer'image(i) & " = " & to_string(plus_u4u);
      report "s4s: + " & integer'image(i) & " = " & to_string(plus_s4s);
      report "s4s: - " & integer'image(i) & " = " & to_string(minus_s4s);
      report "s4s: abs " & integer'image(i) & " = " & to_string(abs_s4s);

      report "u4v: + " & integer'image(i) & " = " & to_string(plus_u4v);
      report "s4v: + " & integer'image(i) & " = " & to_string(plus_s4v);
      report "s4v: - " & integer'image(i) & " = " & to_string(minus_s4v);
      report "s4v: abs " & integer'image(i) & " = " & to_string(abs_s4v);
    end loop;
    wait;
  end process;
end behav;
