library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity gen is
  generic(
    type tyi;
    impure function mux(i1 : tyi;
                 i2 : tyi;
                 en : boolean) return tyi
  );
  port(
    i1  : in tyi;
    i2  : in tyi;
    en : in boolean;
    po  : out tyi
  );
end entity;

architecture bhv of gen is
begin

  po <= mux(i1,i2,en);

end bhv;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dut is
  port(
    d1 : in bit_vector(7 downto 0);
    d2 : in bit_vector(7 downto 0);
    sel : in boolean;
    do : out bit_vector(7 downto 0)
  );
  
end entity;

architecture bhv of dut is

  impure function bvmux(d1 : bit_vector; d2 : bit_vector; sel : boolean) return bit_vector is
  begin
    return d2 xor d1;
  end function;

begin
  u1: entity work.gen(bhv)
    generic map(tyi => bit_vector(7 downto 0), mux => bvmux
    )
    port map(
      i1 => d1,
      i2 => d2,
      en => sel,
      po => do
    );
end bhv;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
end entity;

architecture bhv of top is

  signal iin1 : integer;
  signal iin2 : integer;
  signal cout1 : integer;
  signal my_sel : boolean;

  signal is1 : bit_vector(7 downto 0) := "00111100";
  signal is2 : bit_vector(7 downto 0) := "01010101";
  signal is3 : bit_vector(7 downto 0) := "00011101";
  signal cout2 : bit_vector(7 downto 0);
  
  
  function my_mux(i1 : integer;
                 i2 : integer;
                 en : boolean) return integer is
  begin
    if en then
      return i1 + i2;
    else
      return 0;
    end if;
  end function my_mux;

  function my_bmux(i1 : bit_vector;
                 i2 : bit_vector;
                 en : boolean) return bit_vector is
  begin
    if en then
      return i1;
    else
      return i2;
    end if;
  end function my_bmux;

begin

  process
    variable tout : integer;
  begin
    wait for 1 ns;
    for i in is1'range loop
      report bit'image(is1(i));
    end loop;
    report integer'image(my_mux(10, 14, my_sel));
    wait for 1 ns;
    for i in is1'range loop
      report bit'image(is1(i));
    end loop;
    report integer'image(my_mux(10, 14, my_sel));
    wait for 1 ns;
     for i in is1'range loop
      report bit'image(is1(i));
    end loop;
    report integer'image(my_mux(10, 14, my_sel));
     wait for 1 ns;
     for i in is1'range loop
      report bit'image(is1(i));
    end loop;
    report integer'image(my_mux(10, 14, my_sel));
   wait;
  end process;
  
  dut2: entity work.gen
    generic map(
      tyi => bit_vector,
      mux => my_bmux
    )
    port map(
      i1  => is1,
      i2  => is2,
      en  => my_sel,
      po  => cout2
    );

  dut1: entity work.gen
    generic map(
      tyi => integer,
      mux => my_mux
    )
    port map(
      i1  => iin1,
      i2  => iin2,
      po  => cout1,
      en => my_sel
    );
  
  process
  begin
    my_sel <= true;
    iin1 <= 5;
    iin2 <= 4;
    wait for 1 ns;
    report integer'image(cout1);
    my_sel <= false;
    wait for 1 ns;
    report integer'image(cout1);
    is1 <=  is3;
    my_sel <= true;
    wait for 1 ns;
    iin2 <= 3;
    is1 <=  is2;
    wait for 1 ns;
    report integer'image(cout1);
    wait;
  end process;

end bhv;

