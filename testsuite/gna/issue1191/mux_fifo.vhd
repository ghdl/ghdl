library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mux_fifo_pkg.all;

entity mux_fifo is
  generic (g_enabled_channels : std_logic_vector;
           g_extend           : natural range 0 to 1);
  port (rst        : in    std_logic;
        clk        : in    std_logic;
        -- fifo if
        fifo_if_in : inout t_mux_fifo_if;
        -- out if
        dataout    : out   std_logic_vector;
        wr_en      : out   std_logic;
        full       : in    std_logic);
end entity mux_fifo;

architecture simple of mux_fifo is

  type t_state is (s_wait, s_capture, s_write);

  signal index : integer := -1;
  signal state : t_state;

  function DetectFirstNonEmpty (EmptyIn : std_logic_vector) return integer is
  begin
    for I in EmptyIn'range loop
      if EmptyIn(I) = '0' then
        return i;
      end if;
    end loop;
    return -1;
  end function;

begin

  fifo_if_in.clk <= clk;

  u_mux : process (clk)
  begin

    if rising_edge(clk) then

      if (rst = '1') then

        index         <= -1;
        dataout       <= (others => '0');
        wr_en         <= '0';
        fifo_if_in.rd <= (others => '0');
        state         <= s_wait;

      else

        case state is

          when s_wait =>

            -- index   <= DetectFirstNonEmpty(empty and enabled_channels);
            index         <= DetectFirstNonEmpty(fifo_if_in.empty);
            dataout       <= (others => '0');
            wr_en         <= '0';
            fifo_if_in.rd <= (others => '0');
            if index >= 0 then
              fifo_if_in.rd(index) <= '1';
              state                <= s_capture;
            end if;

          when s_capture =>

            dataout       <= (others => '0');
            wr_en         <= '0';
            fifo_if_in.rd <= (others => '0');
            if not(full) then
              state <= s_write;
            end if;

          when s_write =>

            index <= -1;
            if g_extend = 1 then
              dataout <= fifo_if_in.data(index) & std_logic_vector(to_signed(index, 8));
            else
              dataout <= fifo_if_in.data(index);
            end if;
            wr_en         <= '1';
            fifo_if_in.rd <= (others => '0');
            state         <= s_wait;

        end case;

      end if;

    end if;

  end process u_mux;

end architecture simple;
