library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity recordOfRecord_tb is
  generic
  (       NB_CHAN_G : positive := 2
   ;      W_ADR_G   : positive := 11
   ;      W_DAT_G   : positive := 16
  );
end recordOfRecord_tb;
architecture tb of recordOfRecord_tb  is
  type dmn_t is record
    clk: std_ulogic;
    rst: std_ulogic;
  end record;

  signal dmn_i_s : dmn_t;
  signal stop_s : std_logic := '0';

  type fifo_i_t is record
    rx_cyc   : std_ulogic;
    rx_stb   : std_ulogic;
    rx_tga   : std_ulogic_vector;
    rx_cti   : std_ulogic_vector( 1 downto 0 );
    rx_dat   : std_ulogic_vector;
    tx_ack   : std_ulogic;
    tx_stall : std_ulogic;
  end record;

  type fifo_o_t is record
    rx_ack   : std_ulogic;
    rx_stall : std_ulogic;
    tx_cyc   : std_ulogic;
    tx_stb   : std_ulogic;
    tx_tga   : std_ulogic_vector;
    tx_cti   : std_ulogic_vector( 1 downto 0 );
    tx_dat   : std_ulogic_vector;
  end record;

  type fifo_array_i_t is array ( natural range <> ) of fifo_i_t;
  type fifo_array_o_t is array ( natural range <> ) of fifo_o_t;

  signal arr_fifo_o
  : fifo_array_o_t( 0 to NB_CHAN_G - 1 )
   (   tx_tga( W_ADR_G - 1 downto 0 )
   ,   tx_dat( W_DAT_G - 1 downto 0 )
  );

  type reg_t is record

    arr_fifo_i
    : fifo_array_i_t( 0 to NB_CHAN_G - 1 )
     (   rx_tga( W_ADR_G - 1 downto 0 )
     ,   rx_dat( W_DAT_G - 1 downto 0 )
    );

  end record;

  signal a
  ,      r
  : reg_t;
begin

  process
  begin
    dmn_i_s.clk <= '0';
    wait for 5 ns;
    dmn_i_s.clk <= '1';
    wait for 5 ns;
    if stop_s = '1' then
      wait;
    end if;
  end process;

  process
  begin
    dmn_i_s.rst <= '1';
    wait for 50 ns;
    dmn_i_s.rst <= '0';
    wait;
  end process;

  process
  begin
    arr_fifo_o( 0 ).tx_cyc <= '0';
    arr_fifo_o( 0 ).tx_cti <= "00";
    wait until dmn_i_s.rst = '0';
    wait until rising_edge( dmn_i_s.clk );
    arr_fifo_o( 0 ).tx_cyc <= '1';
    wait until rising_edge( dmn_i_s.clk );
    wait until rising_edge( dmn_i_s.clk );
    arr_fifo_o( 0 ).tx_cyc <= '0';
    wait for 50 ns;
    stop_s <= '1';
    wait;
  end process;

  process( dmn_i_s.rst , r , arr_fifo_o )
    variable a_v : reg_t;
  begin

    a_v := r;

    if arr_fifo_o( 0 ).tx_cti = "00" then
      if arr_fifo_o( 0 ).tx_cyc = '1' and r.arr_fifo_i( 0 ).tx_ack = '1' then
        a_v.arr_fifo_i( 0 ).tx_ack := '0';
      else
        a_v.arr_fifo_i( 0 ).tx_ack := '1' and arr_fifo_o( 0 ).tx_cyc;
      end if;
    else
      a_v.arr_fifo_i( 0 ).tx_ack := arr_fifo_o( 0 ).tx_cyc;
    end if;

    if dmn_i_s.rst = '1' then
      a_v.arr_fifo_i( 0 ).tx_ack := '0';
    end if;

    a <= a_v;
  end process;


  process( dmn_i_s.clk )
  begin
    if rising_edge( dmn_i_s.clk ) then
      r <= a;
    end if;
  end process;

-- notpad++ script
-- cd D:\vhdl
-- set cc=.\sim\ghdl\0.36-rc1-mingw32-mcode\bin\ghdl.exe

-- set sim_param= -PD:\vhdl\sim\ghdl\simSpace --workdir=D:\vhdl\sim\ghdl\simSpace --std=08


-- $(cc) -i $(sim_param) --work=ghdl_test .\ghdl_test\*.vhd

-- $(cc) -a $(sim_param) --work=ghdl_test .\ghdl_test\recordOfRecord_tb.vhd

-- $(cc) -m $(sim_param) --work=ghdl_test recordOfRecord_tb tb

-- $(cc) -r $(sim_param) --work=ghdl_test recordOfRecord_tb tb --wave=./sim/ghdl/simSpace/out/recordOfRecord_tb.ghw
end tb;
--#######################################################################################
--#######################################################################################
--#######################################################################################
--#######################################################################################
--#######################################################################################
