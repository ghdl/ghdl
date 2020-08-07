library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_exts is
end;

architecture behav of tb_exts is
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

  signal li : integer := 0;

  signal l3        : std_logic_vector (2 downto 0) := "000";
  signal ext_u3 : std_logic_vector (4 downto 0);
  signal sxt_s3 : std_logic_vector (4 downto 0);
begin

  dut: entity work.exts
    port map (
      l3        => l3,
      ext_u3 => ext_u3,
      sxt_s3 => sxt_s3);

  process
  begin
    for i in -4 to 3 loop
      li <= i;
      l3 <= conv_std_logic_vector (i, 3);
      wait for 1 ns;
      report "u3: ext " & integer'image(i) & " = " & to_string(ext_u3);
      report "s3: sxt " & integer'image(i) & " = " & to_string(sxt_s3);
    end loop;
    wait;
  end process;
end behav;
