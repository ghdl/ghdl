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
  signal add_u4u3u : std_logic_vector (3 downto 0) := "0000";
  signal add_s4s3s : std_logic_vector (3 downto 0) := "0000";
  signal add_u4s3s : std_logic_vector (4 downto 0) := "00000";
  signal add_s4u3s : std_logic_vector (3 downto 0) := "0000";
  signal add_u4iu : std_logic_vector (3 downto 0) := "0000";
  signal add_iu3u : std_logic_vector (2 downto 0) := "000";
  signal add_s4is : std_logic_vector (3 downto 0) := "0000";
  signal add_is3s : std_logic_vector (2 downto 0) := "000";
  signal add_u4lu : std_logic_vector (3 downto 0) := "0000";
  signal add_lu3u : std_logic_vector (2 downto 0) := "000";
  signal add_s4ls : std_logic_vector (3 downto 0) := "0000";
  signal add_ls3s : std_logic_vector (2 downto 0) := "000";

  signal add_u4u3v : std_logic_vector (3 downto 0) := "0000";
  signal add_s4s3v : std_logic_vector (3 downto 0) := "0000";
  signal add_u4s3v : std_logic_vector (4 downto 0) := "00000";
  signal add_s4u3v : std_logic_vector (3 downto 0) := "0000";
  signal add_u4iv : std_logic_vector (3 downto 0) := "0000";
  signal add_iu3v : std_logic_vector (2 downto 0) := "000";
  signal add_s4iv : std_logic_vector (3 downto 0) := "0000";
  signal add_is3v : std_logic_vector (2 downto 0) := "000";
  signal add_u4lv : std_logic_vector (3 downto 0) := "0000";
  signal add_lu3v : std_logic_vector (2 downto 0) := "000";
  signal add_s4lv : std_logic_vector (3 downto 0) := "0000";
  signal add_ls3v : std_logic_vector (2 downto 0) := "000";
begin

  dut: entity work.adds
    port map (
      l4 => l4,
      r3 => r3,
      li => li,
      ri => ri,
      add_u4u3u => add_u4u3u,
      add_s4s3s => add_s4s3s,
      add_u4s3s => add_u4s3s,
      add_s4u3s => add_s4u3s,
      add_u4iu => add_u4iu,
      add_iu3u => add_iu3u,
      add_s4is => add_s4is,
      add_is3s => add_is3s,
      add_u4lu => add_u4lu,
      add_lu3u => add_lu3u,
      add_s4ls => add_s4ls,
      add_ls3s => add_ls3s,

      add_u4u3v => add_u4u3v,
      add_s4s3v => add_s4s3v,
      add_u4s3v => add_u4s3v,
      add_s4u3v => add_s4u3v,
      add_u4iv => add_u4iv,
      add_iu3v => add_iu3v,
      add_s4iv => add_s4iv,
      add_is3v => add_is3v,
      add_u4lv => add_u4lv,
      add_lu3v => add_lu3v,
      add_s4lv => add_s4lv,
      add_ls3v => add_ls3v);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "u4u3u: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_u4u3u);
        report "s4s3s: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_s4s3s);
        report "u4s3s: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_u4s3s);
        report "s4u3s: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_s4u3s);
        report "u4iu:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_u4iu);
        report "iu3u:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_iu3u);
        report "s4is:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_s4is);
        report "is3s:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_is3s);
        report "u4lu:  " & integer'image(i) & " + " & integer'image(j mod 2) & " = "
          & to_string(add_u4lu);
        report "lu3u:  " & integer'image(i mod 2) & " + " & integer'image(j) & " = "
          & to_string(add_lu3u);
        report "s4ls:  " & integer'image(i) & " + " & integer'image(j mod 2) & " = "
          & to_string(add_s4ls);
        report "ls3s:  " & integer'image(i mod 2) & " + " & integer'image(j) & " = "
          & to_string(add_ls3s);

        ------

        report "u4u3v: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_u4u3v);
        report "s4s3v: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_s4s3v);
        report "u4s3v: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_u4s3v);
        report "s4u3v: " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_s4u3v);
        report "u4iv:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_u4iv);
        report "iu3v:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_iu3v);
        report "s4iv:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_s4iv);
        report "is3v:  " & integer'image(i) & " + " & integer'image(j) & " = "
          & to_string(add_is3v);
        report "u4lv:  " & integer'image(i) & " + " & integer'image(j mod 2) & " = "
          & to_string(add_u4lv);
        report "lu3v:  " & integer'image(i mod 2) & " + " & integer'image(j) & " = "
          & to_string(add_lu3v);
        report "s4lv:  " & integer'image(i) & " + " & integer'image(j mod 2) & " = "
          & to_string(add_s4lv);
        report "ls3v:  " & integer'image(i mod 2) & " + " & integer'image(j) & " = "
          & to_string(add_ls3v);
      end loop;
    end loop;
    wait;
  end process;
end behav;
