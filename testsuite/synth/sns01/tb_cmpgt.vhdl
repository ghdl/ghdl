library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_cmpgt is
end;

architecture behav of tb_cmpgt is
  signal li : integer := 0;
  signal ri : integer := 0;
  signal l4 : std_logic_vector (3 downto 0) := "0000";
  signal r3 : std_logic_vector (2 downto 0) := "000";
  signal gt_u4u3 : boolean;
  signal gt_s4s3 : boolean;
  signal gt_u4s3 : boolean;
  signal gt_s4u3 : boolean;
  signal gt_u4i :  boolean;
  signal gt_iu3 :  boolean;
  signal gt_s4i :  boolean;
  signal gt_is3 :  boolean;
begin

  dut: entity work.cmpgt
    port map (
      l4 => l4,
      r3 => r3,
      li => li,
      ri => ri,
      gt_u4u3 => gt_u4u3,
      gt_s4s3 => gt_s4s3,
      gt_u4s3 => gt_u4s3,
      gt_s4u3 => gt_s4u3,
      gt_u4i => gt_u4i,
      gt_iu3 => gt_iu3,
      gt_s4i => gt_s4i,
      gt_is3 => gt_is3);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "u4u3: " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_u4u3);
        report "s4s3: " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_s4s3);
        report "u4s3: " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_u4s3);
        report "s4u3: " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_s4u3);
        report "u4i:  " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_u4i);
        report "iu3:  " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_iu3);
        report "s4i:  " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_s4i);
        report "is3:  " & integer'image(i) & " > " & integer'image(j) & " = "
          & boolean'image(gt_is3);
      end loop;
    end loop;
    wait;
  end process;
end behav;
