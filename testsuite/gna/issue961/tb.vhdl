library ieee;
context ieee.ieee_std_context;
use ieee.math_complex.all;

package testpkg is
  type SFIXED is array(integer range <>) of std_logic;
  type SFIXED_VECTOR is array(integer range <>) of SFIXED;
  type CFIXED is record RE, IM : SFIXED; end record;
  type CFIXED_VECTOR is array(integer range <>) of CFIXED;
  type CFIXED_MATRIX is array(integer range <>) of CFIXED_VECTOR;
end package testpkg;

library ieee;
context ieee.ieee_std_context;
use ieee.math_complex.all;
use work.testpkg.all;

entity dut is
  port(CLK : in  std_logic;
       I   : in  CFIXED_VECTOR;
       VI  : in  boolean;
       SI  : in  unsigned;
       O   : out CFIXED_VECTOR;
       VO  : out boolean;
       SO  : out unsigned);
end dut;

architecture test of dut is
begin
  process(CLK)
  begin
    if rising_edge(CLK) then
      VO <= false;
      if VI then
        VO <= true;
        SO <= resize(SI, SO'length);

        for a in 0 to O'length-1 loop
          O(a) <= (RE => '0' & I(a mod I'length).RE, IM => '0' & I(a mod I'length).IM);
        end loop;
      end if;
    end if;
  end process;
end architecture;

library ieee;
context ieee.ieee_std_context;
use ieee.math_complex.all;
use work.testpkg.all;

entity tb is
end entity tb;

architecture tb of tb is
  signal CLK : std_logic := '0';
  signal I   : cfixed_vector(3 downto 0)(RE(15 downto 0), IM(15 downto 0));
  signal VI  : boolean   := false;
  signal SI  : unsigned(7 downto 0);
  signal O   : cfixed_vector(4 downto 0)(RE(16 downto 0), IM(16 downto 0));
  signal VO  : boolean;
  signal SO  : unsigned(14 downto 0);
begin
  CLK <= not CLK after 2 ns;

  process
  begin
    wait for 10 ns;
    I(0) <= (RE => x"ABCD", IM => x"0123");
    I(1) <= (RE => x"ABDD", IM => x"5123");
    I(2) <= (RE => x"ACCD", IM => x"6123");
    I(3) <= (RE => x"BBCD", IM => x"7123");
    SI   <= x"88";
    VI   <= true;
    wait until rising_edge(CLK);
    VI   <= false;
    wait for 20 ns;
    std.env.finish;
  end process;


  inst_dut : entity work.dut
    port map (
      CLK => CLK,
      I   => I,
      VI  => VI,
      SI  => SI,
      O   => O,
      VO  => VO,
      SO  => SO
      );

  process(CLK)
  begin
    if rising_edge(CLK) then
      if VO then
        assert SO = 14x"0888" report "SO wrong";
        assert O(0) = (RE => 17x"0ABCD", IM => 17x"00123") report "O0 wrong";
        assert O(1) = (RE => 17x"0ABCD", IM => 17x"00123") report "O0 wrong";
        assert O(2) = (RE => 17x"0ABCD", IM => 17x"00123") report "O0 wrong";
        assert O(3) = (RE => 17x"0ABCD", IM => 17x"00123") report "O0 wrong";
        assert O(4) = (RE => 17x"0ABCD", IM => 17x"00123") report "O0 wrong";
      end if;
    end if;
  end process;
end architecture tb;
