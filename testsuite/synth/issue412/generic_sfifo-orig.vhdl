-- A simple type-generic stream-style synchronous FIFO
-- may not be 100% valid VHDL code, contact ktbarrett on gitter
-- non-generic version does synthesize correctly
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity generic_SFIFO is
  generic (
    T         : type;
    MIN_DEPTH : natural);
  port (
    clk       : in    std_ulogic;
    rst       : in    std_ulogic;
    data_in   : in    T;
    valid_in  : in    std_ulogic;
    ready_out : out   std_ulogic;
    data_out  : out   T;
    valid_out : out   std_ulogic;
    ready_in  : in    std_ulogic);
end entity generic_SFIFO;

architecture rtl of generic_SFIFO is

  function clog2 (n: natural) return natural is
    variable i    : natural;
    variable test : natural;
  begin
    test := 1;
    i := 0;
    while (test < n) loop
      i := i + 1;
      test := test * 2;
    end loop;
    return i;
  end function clog2;

  constant ptr_size : natural := clog2(MIN_DEPTH);
  constant depth    : natural := 2 ** ptr_size;
  signal   rd_ptr   : unsigned(ptr_size downto 0);
  signal   wr_ptr   : unsigned(ptr_size downto 0);

  type ram_type is array(0 to depth - 1) of T;

  signal ram : ram_type;

begin

  fifo_proc : process (clk) is

    variable next_wr_ptr : wr_ptr'subtype;
    variable next_rd_ptr : rd_ptr'subtype;

  begin

    if (rising_edge(clk)) then
      if (rst /= '0') then
        rd_ptr    <= (others => '0');
        wr_ptr    <= (others => '0');
        ready_out <= '0';
        valid_out <= '0';
      else
        next_wr_ptr := wr_ptr;
        next_rd_ptr := rd_ptr;
        if ((valid_in='1') and (ready_out='1')) then
          ram(to_integer(wr_ptr(wr_ptr'left - 1 downto 0))) <= data_in;
          next_wr_ptr := wr_ptr + 1;
        end if;
        if ((valid_out='1') and (ready_in='1')) then
          next_rd_ptr := rd_ptr + 1;
        end if;
        ready_out <= '0' when
                     (next_wr_ptr(next_wr_ptr'left) /= next_rd_ptr(next_rd_ptr'left)) and
                     (next_wr_ptr(next_wr_ptr'left - 1 downto 0) = next_rd_ptr(next_rd_ptr'left - 1 downto 0)) else
                     '1';
        valid_out <= '0' when
                     (wr_ptr(wr_ptr'left) = next_rd_ptr(next_rd_ptr'left)) and
                     (wr_ptr(wr_ptr'left - 1 downto 0) = next_rd_ptr(next_rd_ptr'left - 1 downto 0)) else
                     '1';
        wr_ptr   <= next_wr_ptr;
        rd_ptr   <= next_rd_ptr;
        data_out <= ram(to_integer(next_rd_ptr(next_rd_ptr'left - 1 downto 0)));
      end if;
    end if;

  end process fifo_proc;

end architecture rtl;
