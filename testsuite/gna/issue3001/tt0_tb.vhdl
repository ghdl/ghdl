-----------------------------------------------------------------------------------
-- * Libs
-----------------------------------------------------------------------------------
LIBRARY IEEE;
  USE IEEE.STD_LOGIC_1164.ALL;
  USE IEEE.NUMERIC_STD.ALL;

LIBRARY STD;
  USE STD.ENV.FINISH;


-----------------------------------------------------------------------------------
-- * Entity
-----------------------------------------------------------------------------------
ENTITY tt0_tb IS
END tt0_tb;

-----------------------------------------------------------------------------------
-- * Architecture Begins
-----------------------------------------------------------------------------------
ARCHITECTURE str_tb OF tt0_tb IS

  -------------------------------------------------------------------
  -- * Constants
  -------------------------------------------------------------------
  -- Write Command Code Upper Case (W). To start a Board Manager Write Transaction.
  constant C_WR_CMD_UC       : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('W'),8)); 
  -- Write Command Code Lower Case (w). To start a Board Manager Write Transaction.
  constant C_WR_CMD_LC       : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('w'),8)); 
  -- Read Command Code Upper Case (R). To start a Board Manager Read Transaction.
  constant C_RD_CMD_UC       : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('R'),8));
  -- Read Command Code Lower Case (r). To start a Board Manager Read Transaction. 
  constant C_RD_CMD_LC       : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('r'),8)); 
  -- Question Mark Command (?). To mark the end of a complete command (both read and write)
  constant C_QUESTIONMARK_CMD: std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('?'),8));
  -- Comma Command (,). To mark the end of the address in a complete command (write only)
  constant C_COMMA_CMD       : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos(','),8));
  -- Slave Responses Commands (Y,N). 
  constant C_Y_CMD           : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('Y'),8));
  constant C_N_CMD           : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('N'),8));
  -- <CR> and <LF> hex codes.
  constant C_CR_CMD          : std_logic_vector(7 downto 0) := x"0D";
  constant C_LF_CMD          : std_logic_vector(7 downto 0) := x"0A";
  -- ASCII hex codes for Upper Case (UC) hexadecimal letters (A-F)
  constant C_LETTER_A_UC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('A'),8));
  constant C_LETTER_B_UC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('B'),8));
  constant C_LETTER_C_UC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('C'),8));
  constant C_LETTER_D_UC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('D'),8));
  constant C_LETTER_E_UC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('E'),8));
  constant C_LETTER_F_UC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('F'),8));
  -- ASCII hex codes for Lower Case (LC) hexadecimal letters (a-f)
  constant C_LETTER_A_LC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('a'),8));
  constant C_LETTER_B_LC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('b'),8));
  constant C_LETTER_C_LC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('c'),8));
  constant C_LETTER_D_LC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('d'),8));
  constant C_LETTER_E_LC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('e'),8));
  constant C_LETTER_F_LC     : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(character'pos('f'),8));

  -----------------------------------------------------------------------------------
  -- * Types
  -----------------------------------------------------------------------------------

  -----------------------------------------------------------------------------------
  -- * Signals
  -----------------------------------------------------------------------------------
  signal clk_uut_aux              : std_logic := '0';
  signal s_clk_uut                : std_logic := '0';
  signal s_rstn_uut               : std_logic := '0';


  signal s_dat : std_logic_vector(7 downto 0) := (others=>'0');
  signal s_data_ascii2bin_char, r_dat : std_logic_vector(3 downto 0);


  -----------------------------------------------------------------------------------
  -- * Architecture Structure Testbench
  -----------------------------------------------------------------------------------
BEGIN
  -----------------------------------------------------------------------------------
  -- * Clock/Reset generation
  -----------------------------------------------------------------------------------
    -- ** UUT CLK/Reset generation
    -----------------------------------------------------------------------------------
    clk_gen:
      clk_uut_aux <= not clk_uut_aux       after 20 ns/2;
      s_clk_uut   <= transport clk_uut_aux after 0.0123 ns;

    rst_gen:
      s_rstn_uut  <= '1' after 1 us;


-----------------------------------------------------------------------------------
  -- * Assignaments
  -----------------------------------------------------------------------------------
  with s_dat select --accepts both upper case (UC) and lower case (LC)
    s_data_ascii2bin_char <= x"a" when C_LETTER_A_LC | C_LETTER_A_UC, 
                             x"b" when C_LETTER_B_LC | C_LETTER_B_UC,
                             x"c" when C_LETTER_C_LC | C_LETTER_C_UC,
                             x"d" when C_LETTER_D_LC | C_LETTER_D_UC,
                             x"e" when C_LETTER_E_LC | C_LETTER_E_UC,
                             x"f" when others;
                             
  ----------------------------------------------------------------------------------
  -- * Process
  -----------------------------------------------------------------------------------

    -- ** MS1 Trigger Generator
    -----------------------------------------------------------------------------------
    tt: process(s_clk_uut, s_rstn_uut)
    begin
      if s_rstn_uut='0' then
        r_dat <= (others=>'0');
      elsif rising_edge(s_clk_uut) then
        r_dat <= s_data_ascii2bin_char;
      end if;

    end process;

    -- ** Simulation Start
    -----------------------------------------------------------------------------------
    sim_start: process
    -- + Process begin
    begin
      report LF & " ===========================================" &LF
                 & " +--   TT0 Test       --+" &LF
                 & " ===========================================";
      wait until s_rstn_uut='1';
      
      s_dat <= C_LETTER_A_UC;
      wait for 10 us;

      s_dat <= C_LETTER_A_LC;
      wait for 10 us;
      
      s_dat <= C_LETTER_D_LC;
      wait for 10 us;
      
      report LF & " ===========================================" & LF
              & " +--   TT0 Test        --+" & LF
                 & " ===========================================" & LF;
      finish;
    end process;


-----------------------------------------------------------------------------------
-- * Architecture Ends
-----------------------------------------------------------------------------------
END str_tb;
