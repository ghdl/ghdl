library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity tb_cmplt is
end;

architecture behav of tb_cmplt is
  signal li : integer := 0;
  signal ri : integer := 0;
  signal l4 : std_logic_vector (3 downto 0) := "0000";
  signal r3 : std_logic_vector (2 downto 0) := "000";
  signal lt_u4u3 : boolean;
  signal lt_s4s3 : boolean;
  signal lt_u4s3 : boolean;
  signal lt_s4u3 : boolean;
  signal lt_u4i :  boolean;
  signal lt_iu3 :  boolean;
  signal lt_s4i :  boolean;
  signal lt_is3 :  boolean;
begin

  dut: entity work.cmplt
    port map (
      l4 => l4,
      r3 => r3,
      li => li,
      ri => ri,
      lt_u4u3 => lt_u4u3,
      lt_s4s3 => lt_s4s3,
      lt_u4s3 => lt_u4s3,
      lt_s4u3 => lt_s4u3,
      lt_u4i => lt_u4i,
      lt_iu3 => lt_iu3,
      lt_s4i => lt_s4i,
      lt_is3 => lt_is3);

  process
  begin
    for i in -8 to 7 loop
      li <= i;
      l4 <= conv_std_logic_vector (i, 4);
      for j in -4 to 3 loop
        r3 <= conv_std_logic_vector (j, 3);
        ri <= j;
        wait for 1 ns;
        report "u4u3: " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_u4u3);
        report "s4s3: " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_s4s3);
        report "u4s3: " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_u4s3);
        report "s4u3: " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_s4u3);
        report "u4i:  " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_u4i);
        report "iu3:  " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_iu3);
        report "s4i:  " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_s4i);
        report "is3:  " & integer'image(i) & " < " & integer'image(j) & " = "
          & boolean'image(lt_is3);
      end loop;
    end loop;
    wait;
  end process;
end behav;
