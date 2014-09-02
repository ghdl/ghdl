LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

ENTITY TestBenchAutomated IS
-- Generics passed in
generic (m: integer := 3; n: integer := 5; h: integer := 4; DATA_SIZE: integer :=5);
END TestBenchAutomated;

ARCHITECTURE behavior OF TestBenchAutomated IS 

     -- Component Declaration for the Unit Under Test (UUT)
     COMPONENT TopLevelM_M
     generic (m: integer := 3; n: integer := 5; h: integer := 4; DATA_SIZE: integer :=5);
     PORT(
            clk : IN  std_logic;
            next_in : IN  std_logic; --User input
            rst_in : IN  std_logic;  --User input
            OUTPUT : OUT  SIGNED((DATA_SIZE+DATA_SIZE)+(m-1)-1 downto 0) --Calculated DATA output
          );
     END COMPONENT;


    --Inputs
    signal clk : std_logic := '0';
    signal next_in : std_logic := '0';
    signal rst_in : std_logic := '0';

    --Outputs
    signal OUTPUT : SIGNED((DATA_SIZE+DATA_SIZE)+(m-1)-1 downto 0);

    -- Clock period definitions
    constant clk_period : time := 10 ns;

    --Variable to be used in assert section
     type Vector is record
            OUTPUT_test : SIGNED((DATA_SIZE+DATA_SIZE)+(m-1)-1 downto 0);
     end record;

type VectorArray is array (natural range <>) of Vector;

constant Vectors : VectorArray := (
     -- Values to be compaired to calculated output
    (OUTPUT_test =>"000000110000"), -- 48
    (OUTPUT_test =>"000011110110"), -- 246
    (OUTPUT_test =>"000101001000"), -- 382 <--- Purposefully incorrect value, Should be '000100001000' = 264
    (OUTPUT_test =>"111111010011"), -- -45
    (OUTPUT_test =>"111101001100"), -- -180
    (OUTPUT_test =>"111111001111"), -- -49
    (OUTPUT_test =>"000000101011"), -- 43  Purposefully incorrect value, Should be '000010101011' = 171
    (OUTPUT_test =>"000000010011"), -- 19
    (OUTPUT_test =>"111111100101"), -- -27
    (OUTPUT_test =>"111110111011"), -- -69
    (OUTPUT_test =>"111110111011"), -- -69
    (OUTPUT_test =>"000000101101"), -- 45
    (OUTPUT_test =>"111011011110"), -- -290
    (OUTPUT_test =>"000001010110"), -- 86
    (OUTPUT_test =>"000011110010"), -- 242
    (OUTPUT_test =>"000000111110"),  -- 125
    (OUTPUT_test =>"111111001001"), -- -55
    (OUTPUT_test =>"000100010101"), -- 277
    (OUTPUT_test =>"111111100011"), -- -29
    (OUTPUT_test =>"111101111101"));-- -131 



BEGIN

    -- Instantiate the Unit Under Test (UUT)
    uut: TopLevelM_M PORT MAP (
             clk => clk,
             next_in => next_in,
             rst_in => rst_in,
             OUTPUT => OUTPUT
          );

    -- Clock process definitions
  clk_process :process
        begin
            clk <= '0';
            wait for clk_period/2;
            clk <= '1';
            wait for clk_period/2;
        end process;
    -- Process to simulate user input and to check output is correct
Test :process
    variable  i : integer;
        begin
            wait for 100 ns;
            rst_in <= '1';
            wait for clk_period*3;
            rst_in <= '0';

    --Loops through enough times to cover matrix and more to show it freezes in S_Wait state
    for i in 0 to 50 loop 

            for i in Vectors'range loop

                next_in <= '1';
                wait for clk_period*5;
                next_in <= '0';
                wait for clk_period*4; --Appropriate amount of clock cycles needed for calculations to be displayed at output
                --Check the output is the same as expected
                assert OUTPUT = Vectors(i).OUTPUT_test
                report "Incorrect Output on vector line" & integer'image(i) &
                lf & "Expected:" & integer'image(i)(to_integer((Vectors(i).OUTPUT_test))) --& lf &
                --"But got" & integer'image(i)(to_integer(signed(OUTPUT)))
                severity error;

            end loop;
        end loop;

        wait;

    end process;
END;
