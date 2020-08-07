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
  signal plus_v  : std_logic_vector (3 downto 0);
  signal minus_v  : std_logic_vector (3 downto 0);
  signal abs_v  : std_logic_vector (3 downto 0);
begin
  dut: entity work.unaries
    port map (
      l4        => l4,
      plus_v  => plus_v,
      minus_v => minus_v,
      abs_v   => abs_v);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      wait for 1 ns;
      report "v4: + " & integer'image(i) & " = " & to_string(plus_v);
      report "v4: - " & integer'image(i) & " = " & to_string(minus_v);
      report "v4: abs " & integer'image(i) & " = " & to_string(abs_v);
    end loop;
    wait;
  end process;
end behav;
