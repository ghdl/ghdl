entity generic_mux2 is
generic ( type data_type );
  port ( sel : in bit;
         a, b : in data_type;
         z : out data_type
        );
end entity generic_mux2;

architecture rtl of generic_mux2 is
begin
  z <= a when sel = '1' else b;
  
  process(a,b,sel)
  begin
    report sel'path_name;
  end process;
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package gtypes is

type msg_packet is record
  src, dst : unsigned(7 downto 0);
  pkt_type : bit_vector(2 downto 0);
  length : unsigned(4 downto 0);
  payload : integer_vector(0 to 31);
  checksum : unsigned(7 downto 0);
end record msg_packet;

type int_file is file of integer;

end package;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.gtypes.all;


entity top is
end entity;

architecture bhv of top is

  signal sel: bit;
  signal int1: integer;
  signal int2: integer;
  signal into: integer;
  
  signal b1: bit;
  signal b2: bit;
  signal bo: bit;
  
  signal r1: msg_packet;
  signal r2: msg_packet;
  signal ro: msg_packet;

begin

  sel1: entity work.generic_mux2(rtl)
    generic map(data_type => int_file)
    port map(
      sel => sel,
      a => b1,
      b => b2,
      z => bo
    );

  sel2: entity work.generic_mux2(rtl)
    generic map(data_type => msg_packet)
    port map(
      sel => sel,
      a => r1,
      b => r2,
      z => ro
    );

  sel3: entity work.generic_mux2(rtl)
    generic map(data_type => integer)
    port map(
      sel => sel,
      a => int1,
      b => int2,
      z => into
    );

  process
  begin
    int1  <=  2;
    int2  <=  4;
    sel <= '0';
    b2 <= '1';
    r1.src <= (others => '1');
    r2.src <= (others => '0');
    r1.payload <= (others => 5);
    wait for 1 ns;
    sel <= '1';
    wait for 1 ns;
    int2  <=  5;
    wait for 1 ns;
    sel <= '1';
    wait for 1 ns;
    b2 <= '0';
    wait for 1 ns;
    sel <= '0';
    wait for 1 ns;
    sel <= '1';
    wait;
  end process;

  process(ro)
  begin
    report to_string(ro.src);
  end process;
  
  process(bo)
  begin
    report to_string(bo);
  end process;

  process(into)
  begin
    report to_string(into);
  end process;

end bhv;

