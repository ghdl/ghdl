library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity shift_register is
  generic (
    -- number of stages
    NUM_STAGES: natural := 11;
    -- number of bits
    BITS: natural := 4
  );
  port (
    clk, rst: in std_logic;
    x: in std_logic_vector (BITS - 1 downto 0);
    y: out std_logic_vector (BITS - 1 downto 0)
  );
end entity;
architecture rtl of shift_register
is
  type signed_array is array (natural range <>) of signed;
  signal shift_reg: signed_array (1 to NUM_STAGES - 1)(BITS - 1 downto 0);
begin
  process (clk, rst)
  begin
    if rst
    then
      shift_reg <= (others => (others => '0'));
    elsif rising_edge (clk)
    then
      shift_reg <= signed (x) & shift_reg (1 to NUM_STAGES - 2);
    end if;
  end process;
  y <= std_logic_vector (shift_reg (NUM_STAGES - 1));
end architecture;

