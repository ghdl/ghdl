-----------------------------------------------------------------------------------
-- * Libs
-----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

LIBRARY STD;
USE     STD.TEXTIO.ALL;
USE     STD.ENV.FINISH; -- use of vhdl-2008

LIBRARY WORK;
USE WORK.ini_pkg.all;

-----------------------------------------------------------------------------------
-- * Entity
-----------------------------------------------------------------------------------
ENTITY test_9_tb IS
END test_9_tb;

-----------------------------------------------------------------------------------
-- * Architecture Begins
-----------------------------------------------------------------------------------
ARCHITECTURE str_tb OF test_9_tb IS
  -----------------------------------------------------------------------------------
  -- * Constants
  -----------------------------------------------------------------------------------
  constant C_CLK_PERIOD  : time := 20 ns; -- 50 Mhz

  -----------------------------------------------------------------------------------
  -- * Components Declaration
  -----------------------------------------------------------------------------------
  component map_synchro is
    port (
    CLK_I                : in   std_logic;
    RSTN_I               : in   std_logic;

    A_val_of_0 : in   std_logic;
    A_val_of_1 : in   std_logic;
    A_val_of_2 : in   std_logic;
    A_val_of_3 : in   std_logic;
    A_val_of_4 : in   std_logic;
    A_val_of_5 : in   std_logic;
    A_SYNC_O   : out  ini_t; 

    B_val_of_0 : in   std_logic;
    B_val_of_1 : in   std_logic;
    B_val_of_2 : in   std_logic;
    B_val_of_3 : in   std_logic;
    B_val_of_4 : in   std_logic;
    B_val_of_5 : in   std_logic;
    B_SYNC_O   : out  ini_t 
       
    );
  end component map_synchro;


  -----------------------------------------------------------------------------------
  -- * Signals
  -----------------------------------------------------------------------------------
    -- ** Structure  (Interconnections)
    -----------------------------------------------------------------------------------
    signal s_b_sync         : ini_t;    
    signal s_a_sync         : ini_t;

    -- ** Stimulus
    -----------------------------------------------------------------------------------
    signal s_clk   : std_logic := '1';
    signal s_rstn  : std_logic := '0';

    signal s_a_val_of_0    : std_logic ;
    signal s_a_val_of_1    : std_logic ;
    signal s_a_val_of_2    : std_logic ;
    signal s_a_val_of_3    : std_logic ;
    signal s_a_val_of_4    : std_logic ;
    signal s_a_val_of_5    : std_logic ;

    signal s_b_val_of_0    : std_logic ;
    signal s_b_val_of_1    : std_logic ;
    signal s_b_val_of_2    : std_logic ;
    signal s_b_val_of_3    : std_logic ;
    signal s_b_val_of_4    : std_logic ;
    signal s_b_val_of_5    : std_logic ;

  -----------------------------------------------------------------------------------
  -- * Procedures
  -----------------------------------------------------------------------------------
    -- ** Initialize Boot Fallback Signals (PG and N_FAIL)
    -----------------------------------------------------------------------------------
    procedure p_ini_signals (
      signal clk        : in std_logic;
      
      signal a_val_of_0 : out std_logic;
      signal a_val_of_1 : out std_logic;
      signal a_val_of_2 : out std_logic;
      signal a_val_of_3 : out std_logic;
      signal a_val_of_4 : out std_logic;
      signal a_val_of_5 : out std_logic;

      signal b_val_of_0 : out std_logic;
      signal b_val_of_1 : out std_logic;
      signal b_val_of_2 : out std_logic;
      signal b_val_of_3 : out std_logic;
      signal b_val_of_4 : out std_logic;
      signal b_val_of_5 : out std_logic
    ) is
    begin
      

      wait until rising_edge(clk);
        a_val_of_0 <= '1';
      wait until rising_edge(clk);
        a_val_of_1 <= '1';
      wait until rising_edge(clk);
        a_val_of_2 <= '1';
      wait until rising_edge(clk);
        a_val_of_3 <= '1';
      wait until rising_edge(clk);
        a_val_of_4 <= '1';
      wait until rising_edge(clk);
        a_val_of_5  <= '1';

      wait until rising_edge(clk);
        b_val_of_0 <= '1';
      wait until rising_edge(clk);
        b_val_of_1 <= '1';
      wait until rising_edge(clk);
        b_val_of_2 <= '1';
      wait until rising_edge(clk);
        b_val_of_3 <= '1';
      wait until rising_edge(clk);
        b_val_of_4 <= '1';
      wait until rising_edge(clk);
        b_val_of_5  <= '1';


      wait until rising_edge(clk);

    end procedure p_ini_signals;


  -----------------------------------------------------------------------------------
  -- * Architecture Structure Testbench
  -----------------------------------------------------------------------------------
BEGIN
  -----------------------------------------------------------------------------------
  -- * Clock and Reset generation
  -----------------------------------------------------------------------------------
  clk_gen:
    s_clk <= not s_clk after C_CLK_PERIOD/2;

  rst_gen :
      s_rstn <=
      '1' after (1 us);

  -----------------------------------------------------------------------------------
  -- * Process
  -----------------------------------------------------------------------------------
    -- ** Stimulus
    -----------------------------------------------------------------------------------
    stimulus: process
    -- + Procedures
    procedure ini_signals (
      signal clk        : in std_logic
    ) is
    begin
      p_ini_signals (
        clk                   => clk               ,

        a_val_of_0    => s_a_val_of_0,
        a_val_of_1    => s_a_val_of_1,
        a_val_of_2    => s_a_val_of_2,
        a_val_of_3    => s_a_val_of_3,
        a_val_of_4    => s_a_val_of_4,
        a_val_of_5    => s_a_val_of_5,

        b_val_of_0    => s_b_val_of_0,
        b_val_of_1    => s_b_val_of_1,
        b_val_of_2    => s_b_val_of_2,
        b_val_of_3    => s_b_val_of_3,
        b_val_of_4    => s_b_val_of_4,
        b_val_of_5    => s_b_val_of_5
      );

    end procedure ini_signals;


    -- + Process begin
    begin
      
      wait until s_rstn='1';
      write(output, LF & " ===========================================" & LF
                       & " +--           Starting Test 9           --+" & LF
                       & " ===========================================" & LF & LF);

      --=================================================================================

      ini_signals(s_clk);

      --=================================================================================
      
      write(output, LF & " +-- Test 9 has finished sucessfully!!" & LF);
      write(output, LF & " ===========================================" & LF
                       & " +--           Finishing Test 9          --+" & LF
                       & " ===========================================" & LF & LF);
      finish;
    end process;

  -----------------------------------------------------------------------------------
  -- * Components Instatiation
  -----------------------------------------------------------------------------------
  dut_0_boot_fallback: map_synchro
  port map (
    CLK_I              => s_clk,
    RSTN_I             => s_rstn,

    -- N_FAIL board signals
    A_val_of_0        => s_a_val_of_0   ,       
    A_val_of_1        => s_a_val_of_1   ,
    A_val_of_2        => s_a_val_of_2   ,
    A_val_of_3        => s_a_val_of_3   ,
    A_val_of_4        => s_a_val_of_4   ,
    A_val_of_5        => s_a_val_of_5   ,
    A_SYNC_O          => s_a_sync     ,

    -- PG board signals
    B_val_of_0       => s_b_val_of_0    ,
    B_val_of_1       => s_b_val_of_1    ,
    B_val_of_2       => s_b_val_of_2    ,
    B_val_of_3       => s_b_val_of_3    ,
    B_val_of_4       => s_b_val_of_4    ,
    B_val_of_5       => s_b_val_of_5    ,
    B_SYNC_O         => s_b_sync          
  );

-----------------------------------------------------------------------------------
-- * Architecture Ends
-----------------------------------------------------------------------------------
END str_tb;
