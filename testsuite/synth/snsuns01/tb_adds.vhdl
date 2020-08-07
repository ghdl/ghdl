library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_adds is
end;

architecture behav of tb_adds is
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
  signal l4 : std_logic_vector (3 downto 0) := "0000";
  signal r3 : std_logic_vector (2 downto 0) := "000";

  signal add_v4v3 : std_logic_vector (3 downto 0);
  signal add_v4i  : std_logic_vector (3 downto 0);
  signal add_iv3  : std_logic_vector (2 downto 0);
  signal add_v4l  : std_logic_vector (3 downto 0);
  signal add_lv3  : std_logic_vector (2 downto 0);
begin

  dut: entity work.adds
    port map (
      li       => li,
      ri       => ri,
      l4       => l4,
      r3       => r3,
      add_v4v3 => add_v4v3,
      add_v4i  => add_v4i,
      add_iv3  => add_iv3,
      add_v4l  => add_v4l,
      add_lv3  => add_lv3);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "v4v3: " & to_string(l4) & " + " & to_string(r3) & " = "
          & to_string(add_v4v3);
        report "v4i:  " & to_string(l4) & " + " & integer'image(j) & " = "
          & to_string(add_v4i);
        report "iv3:  " & integer'image(i) & " + " & to_string(r3) & " = "
          & to_string(add_iv3);
        report "v4l:  " & to_string(l4) & " + " & sl_map(r3(0)) & " = "
          & to_string(add_v4l);
        report "lv3:  " & sl_map (l4(0)) & " + " & to_string(r3) & " = "
          & to_string(add_lv3);
      end loop;
    end loop;
    wait;
  end process;
end behav;
