package repro1_pkg is
  type bv_array_t is array(natural range <>) of bit_vector;
end package ;

use work.repro1_pkg.all;

entity repro1 is
  generic (
    WINDOW_COUNT  : integer;   -- 4
    WINDOW_SIZE   : integer;    -- 3
    BIT_WIDTH     : integer;
    WINDOW_STRIDE : integer   -- 1
    );
  port (
    i_in 	: in bv_array_t;--(0 to INPUT_SIZE-1)(BIT_WIDTH-1 downto 0);
    o_windows : out bv_array_t(0 to WINDOW_COUNT*WINDOW_SIZE-1)(BIT_WIDTH-1 downto 0)
    );

  constant IN_LEN1 : integer := i_in'length;
  constant WINDOW_COUNT_1 : integer :=
    1 + (IN_LEN1 - WINDOW_SIZE)/WINDOW_STRIDE;
end entity;

architecture comb of repro1 is
begin
  assert WINDOW_COUNT = 4 severity failure;
  assert IN_LEN1 = 6 severity failure;
  assert WINDOW_COUNT_1 = 4 severity failure;

  gen_w1 : for c1 in 0 to WINDOW_COUNT_1 - 1 generate
    gen_wj : for j in 0 to WINDOW_SIZE - 1 generate
      o_windows(c1*WINDOW_SIZE + j) <= i_in(c1*WINDOW_STRIDE + j);
    end generate gen_wj;
  end generate gen_w1;
end architecture;

library ieee;
use ieee.numeric_bit.all;

use work.repro1_pkg.all;
use std.textio.all;

entity repro1_tb is
  generic (
    WINDOW_SIZE   : integer := 3;
    WINDOW_STRIDE : integer := 1;
    IN_SIZE 	  : integer := 6;
    BIT_WIDTH     : positive := 4
    );

  constant WINDOW_COUNT : integer := 1 + (IN_SIZE - WINDOW_SIZE)/WINDOW_STRIDE;
end entity;

architecture behav of repro1_tb is
  signal s_in : bv_array_t(0 to IN_SIZE-1)(BIT_WIDTH-1 downto 0) := (others => (others => '1'));
  signal s_windows : bv_array_t(0 to WINDOW_COUNT*WINDOW_SIZE-1)(BIT_WIDTH-1 downto 0) := (others => (others => '0'));
begin
  s : entity work.repro1
    generic map (
      -- INPUT_SIZE => IN_SIZE,
      WINDOW_COUNT => WINDOW_COUNT,
      BIT_WIDTH => BIT_WIDTH,
      WINDOW_SIZE => WINDOW_SIZE,
      WINDOW_STRIDE => WINDOW_STRIDE
      )
    port map (
      i_in => s_in,
      o_windows => s_windows
      );

  process
  begin
    s_in(0)<=x"E";
    s_in(1)<=x"C";
    s_in(2)<=x"A";
    s_in(3)<=x"8";
    s_in(4)<=x"6";
    s_in(5)<=x"4";

    wait for 10 ns;

    for i in s_in'range loop
      write(output,
            "s_in(" & natural'image(i) & ")=" & to_hstring(s_in(i)) & LF);
    end loop;

    for i in s_windows'range loop
      write(output,
            "s_windows(" & natural'image(i) & ")="
            & to_hstring(s_windows(i)) & LF);
    end loop;

    for c1 in 0 to WINDOW_COUNT-1 loop
      for j in 0 to WINDOW_SIZE-1 loop
        assert s_windows(c1*WINDOW_SIZE + j) = s_in(c1*WINDOW_STRIDE+j)
          report "Test: incorrect SLV on output.";
      end loop;
    end loop;

    wait;
  end process;

end architecture;
