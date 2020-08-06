library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_subs is
end;

architecture behav of tb_subs is
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
  signal sub_u4u3u : std_logic_vector (3 downto 0) := "0000";
  signal sub_s4s3s : std_logic_vector (3 downto 0) := "0000";
  signal sub_u4s3s : std_logic_vector (4 downto 0) := "00000";
  signal sub_s4u3s : std_logic_vector (3 downto 0) := "0000";
  signal sub_u4iu : std_logic_vector (3 downto 0) := "0000";
  signal sub_iu3u : std_logic_vector (2 downto 0) := "000";
  signal sub_s4is : std_logic_vector (3 downto 0) := "0000";
  signal sub_is3s : std_logic_vector (2 downto 0) := "000";
  signal sub_u4lu : std_logic_vector (3 downto 0) := "0000";
  signal sub_lu3u : std_logic_vector (2 downto 0) := "000";
  signal sub_s4ls : std_logic_vector (3 downto 0) := "0000";
  signal sub_ls3s : std_logic_vector (2 downto 0) := "000";

  signal sub_u4u3v : std_logic_vector (3 downto 0) := "0000";
  signal sub_s4s3v : std_logic_vector (3 downto 0) := "0000";
  signal sub_u4s3v : std_logic_vector (4 downto 0) := "00000";
  signal sub_s4u3v : std_logic_vector (3 downto 0) := "0000";
  signal sub_u4iv : std_logic_vector (3 downto 0) := "0000";
  signal sub_iu3v : std_logic_vector (2 downto 0) := "000";
  signal sub_s4iv : std_logic_vector (3 downto 0) := "0000";
  signal sub_is3v : std_logic_vector (2 downto 0) := "000";
  signal sub_u4lv : std_logic_vector (3 downto 0) := "0000";
  signal sub_lu3v : std_logic_vector (2 downto 0) := "000";
  signal sub_s4lv : std_logic_vector (3 downto 0) := "0000";
  signal sub_ls3v : std_logic_vector (2 downto 0) := "000";
begin

  dut: entity work.subs
    port map (
      l4 => l4,
      r3 => r3,
      li => li,
      ri => ri,
      sub_u4u3u => sub_u4u3u,
      sub_s4s3s => sub_s4s3s,
      sub_u4s3s => sub_u4s3s,
      sub_s4u3s => sub_s4u3s,
      sub_u4iu => sub_u4iu,
      sub_iu3u => sub_iu3u,
      sub_s4is => sub_s4is,
      sub_is3s => sub_is3s,
      sub_u4lu => sub_u4lu,
      sub_lu3u => sub_lu3u,
      sub_s4ls => sub_s4ls,
      sub_ls3s => sub_ls3s,

      sub_u4u3v => sub_u4u3v,
      sub_s4s3v => sub_s4s3v,
      sub_u4s3v => sub_u4s3v,
      sub_s4u3v => sub_s4u3v,
      sub_u4iv => sub_u4iv,
      sub_iu3v => sub_iu3v,
      sub_s4iv => sub_s4iv,
      sub_is3v => sub_is3v,
      sub_u4lv => sub_u4lv,
      sub_lu3v => sub_lu3v,
      sub_s4lv => sub_s4lv,
      sub_ls3v => sub_ls3v);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "u4u3u: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_u4u3u);
        report "s4s3s: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_s4s3s);
        report "u4s3s: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_u4s3s);
        report "s4u3s: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_s4u3s);
        report "u4iu:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_u4iu);
        report "iu3u:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_iu3u);
        report "s4is:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_s4is);
        report "is3s:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_is3s);
        report "u4lu:  " & integer'image(i) & " - " & integer'image(j mod 2) & " = "
          & to_string(sub_u4lu);
        report "lu3u:  " & integer'image(i mod 2) & " - " & integer'image(j) & " = "
          & to_string(sub_lu3u);
        report "s4ls:  " & integer'image(i) & " - " & integer'image(j mod 2) & " = "
          & to_string(sub_s4ls);
        report "ls3s:  " & integer'image(i mod 2) & " - " & integer'image(j) & " = "
          & to_string(sub_ls3s);

        ------

        report "u4u3v: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_u4u3v);
        report "s4s3v: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_s4s3v);
        report "u4s3v: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_u4s3v);
        report "s4u3v: " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_s4u3v);
        report "u4iv:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_u4iv);
        report "iu3v:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_iu3v);
        report "s4iv:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_s4iv);
        report "is3v:  " & integer'image(i) & " - " & integer'image(j) & " = "
          & to_string(sub_is3v);
        report "u4lv:  " & integer'image(i) & " - " & integer'image(j mod 2) & " = "
          & to_string(sub_u4lv);
        report "lu3v:  " & integer'image(i mod 2) & " - " & integer'image(j) & " = "
          & to_string(sub_lu3v);
        report "s4lv:  " & integer'image(i) & " - " & integer'image(j mod 2) & " = "
          & to_string(sub_s4lv);
        report "ls3v:  " & integer'image(i mod 2) & " - " & integer'image(j) & " = "
          & to_string(sub_ls3v);
      end loop;
    end loop;
    wait;
  end process;
end behav;
