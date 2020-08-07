library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_muls is
end;

architecture behav of tb_muls is
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
  signal mul_u4u3u : std_logic_vector (6 downto 0);
  signal mul_s4s3s : std_logic_vector (6 downto 0);
  signal mul_u4s3s : std_logic_vector (7 downto 0);
  signal mul_s4u3s : std_logic_vector (7 downto 0);

  signal mul_u4u3v : std_logic_vector (6 downto 0);
  signal mul_s4s3v : std_logic_vector (6 downto 0);
  signal mul_u4s3v : std_logic_vector (7 downto 0);
  signal mul_s4u3v : std_logic_vector (7 downto 0);
begin

  dut: entity work.muls
    port map (
      l4 => l4,
      r3 => r3,
      mul_u4u3u => mul_u4u3u,
      mul_s4s3s => mul_s4s3s,
      mul_u4s3s => mul_u4s3s,
      mul_s4u3s => mul_s4u3s,

      mul_u4u3v => mul_u4u3v,
      mul_s4s3v => mul_s4s3v,
      mul_u4s3v => mul_u4s3v,
      mul_s4u3v => mul_s4u3v);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "u4u3u: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_u4u3u);
        report "s4s3s: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_s4s3s);
        report "u4s3s: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_u4s3s);
        report "s4u3s: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_s4u3s);

        ------

        report "u4u3v: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_u4u3v);
        report "s4s3v: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_s4s3v);
        report "u4s3v: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_u4s3v);
        report "s4u3v: " & integer'image(i) & " * " & integer'image(j) & " = "
          & to_string(mul_s4u3v);
      end loop;
    end loop;
    wait;
  end process;
end behav;
