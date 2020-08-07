library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_shrs is
end;

architecture behav of tb_shrs is
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
  signal ri : integer := 0;

  signal l3       : std_logic_vector (2 downto 0) := "000";
  signal r4       : std_logic_vector (3 downto 0) := "0000";
  signal shl_v3v4 : std_logic_vector (2 downto 0);
  signal shr_v3v4 : std_logic_vector (2 downto 0);
begin

  dut: entity work.shrs
    port map (
      l3        => l3,
      r4        => r4,
      shl_v3v4 => shl_v3v4,
      shr_v3v4 => shr_v3v4);

  process
  begin
    for i in -4 to 3 loop
      li <= i;
      l3 <= conv_std_logic_vector (i, 3);
      for j in 0 to 5 loop
        r4 <= conv_std_logic_vector (j, 4);
        ri <= j;
        wait for 1 ns;
        report "v3v4: " & integer'image(i) & " shl " & integer'image(j) & " = "
          & to_string(shl_v3v4);
        report "v3v4: " & integer'image(i) & " shr " & integer'image(j) & " = "
          & to_string(shr_v3v4);
      end loop;
    end loop;
    wait;
  end process;
end behav;
