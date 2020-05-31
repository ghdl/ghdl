-- VHDL Entity work.FPadd_normalize.symbol
--
-- Created by
-- Guillermo Marcus, gmarcus@ieee.org
-- using Mentor Graphics FPGA Advantage tools.
--
-- Visit "http://fpga.mty.itesm.mx" for more info.
--
-- 2003-2004. V1.0
--

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

ENTITY FPadd_normalize IS
   PORT( 
      EXP_in  : IN     std_logic_vector (7 DOWNTO 0);
      SIG_in  : IN     std_logic_vector (27 DOWNTO 0);
      EXP_out : OUT    std_logic_vector (7 DOWNTO 0);
      SIG_out : OUT    std_logic_vector (27 DOWNTO 0);
      zero    : OUT    std_logic
   );

-- Declarations

END FPadd_normalize ;

--
-- VHDL Architecture work.FPadd_normalize.struct
--
-- Created by
-- Guillermo Marcus, gmarcus@ieee.org
-- using Mentor Graphics FPGA Advantage tools.
--
-- Visit "http://fpga.mty.itesm.mx" for more info.
--
-- Copyright 2003-2004. V1.0
--


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ARCHITECTURE struct OF FPadd_normalize IS

   -- Architecture declarations

   -- Internal signal declarations
   SIGNAL EXP_lshift : std_logic_vector(7 DOWNTO 0);
   SIGNAL EXP_rshift : std_logic_vector(7 DOWNTO 0);
   SIGNAL SIG_lshift : std_logic_vector(27 DOWNTO 0);
   SIGNAL SIG_rshift : std_logic_vector(27 DOWNTO 0);
   SIGNAL add_in     : std_logic_vector(7 DOWNTO 0);
   SIGNAL cin        : std_logic;
   SIGNAL count      : std_logic_vector(4 DOWNTO 0);
   SIGNAL isDN       : std_logic;
   SIGNAL shift_RL   : std_logic;
   SIGNAL word       : std_logic_vector(26 DOWNTO 0);
   SIGNAL zero_int   : std_logic;
   SIGNAL denormal   : std_logic;
	SIGNAL lshift_cnt : std_logic_vector(4 DOWNTO 0);

   -- Component Declarations
   COMPONENT FPlzc
   PORT (
      word  : IN     std_logic_vector (26 DOWNTO 0);
      zero  : OUT    std_logic ;
      count : OUT    std_logic_vector (4 DOWNTO 0)
   );
   END COMPONENT;

   -- Optional embedded configurations
   -- pragma synthesis_off
   FOR ALL : FPlzc USE ENTITY work.FPlzc;
   -- pragma synthesis_on


BEGIN
   -- Architecture concurrent statements
   -- HDL Embedded Text Block 1 eb1
   -- eb1 1                                        
   SIG_rshift <= '0' & SIG_in(27 DOWNTO 2) & (SIG_in(1) AND SIG_in(0));

   -- HDL Embedded Text Block 2 eb2
   -- eb2 2                    
   add_in <= "000" & count;

	-- limit the count to the exponent value
	PROCESS(count,EXP_in)
	BEGIN
		IF (signed(count) > signed(EXP_in)) THEN
			lshift_cnt <= EXP_in(4 downto 0)-1;
			denormal <= '1';
		ELSE
			lshift_cnt <= count;
			denormal <= '0';
		END IF;
	END PROCESS;

   -- HDL Embedded Text Block 3 eb3
   -- eb3 3
   PROCESS( isDN, shift_RL, EXP_lshift, EXP_rshift, EXP_in, SIG_lshift, SIG_rshift, SIG_in, denormal)
   BEGIN
   IF (isDN='1') THEN
      EXP_out <= X"00";
      SIG_out <= SIG_in;
   ELSE
      IF (shift_RL='1') THEN
         -- Shift Right
         IF (SIG_in(27)='1') THEN
            EXP_out <= EXP_rshift;
            SIG_out <= SIG_rshift;
         ELSE
            EXP_out <= EXP_in;
            SIG_out <= SIG_in;
         END IF;
      ELSE
         -- Shift Left
			IF (denormal='1') THEN
				EXP_out <= (OTHERS => '0');
				SIG_out <= SIG_lshift;
			ELSE
				EXP_out <= EXP_lshift;
				SIG_out <= SIG_lshift;
			END IF;
      END IF;
   END IF;
   END PROCESS;

   -- HDL Embedded Text Block 4 eb4
   -- eb4 4
   zero <= zero_int AND NOT SIG_in(27);

   -- HDL Embedded Text Block 5 eb5
   -- eb5 5
   word <= SIG_in(26 DOWNTO 0);

   -- HDL Embedded Text Block 6 eb6
   -- eb6 6
   PROCESS(SIG_in,EXP_in)
   BEGIN
      IF (SIG_in(27)='0' AND SIG_in(26)='0' AND (EXP_in=X"01")) THEN
         isDN <= '1';
         shift_RL <= '0';
      ELSIF (SIG_in(27)='0' AND SIG_in(26)='0' AND (EXP_in/=X"00")) THEN
         isDN <= '0';
         shift_RL <= '0';
      ELSE
         isDN <= '0';
         shift_RL <= '1';
      END IF;
   END PROCESS;


   -- ModuleWare code(v1.1) for instance 'I3' of 'gnd'
   cin <= '0';

   -- ModuleWare code(v1.1) for instance 'I4' of 'inc'
   I4combo: PROCESS (EXP_in)
   VARIABLE t0 : std_logic_vector(8 DOWNTO 0);
   VARIABLE sum : signed(8 DOWNTO 0);
   VARIABLE din_l : std_logic_vector(7 DOWNTO 0);
   BEGIN
      din_l := EXP_in;
      t0 := din_l(7) & din_l;
      sum := (signed(t0) + '1');
      EXP_rshift <= conv_std_logic_vector(sum(7 DOWNTO 0),8);
   END PROCESS I4combo;

   -- ModuleWare code(v1.1) for instance 'I1' of 'lshift'
   I1combo : PROCESS (SIG_in, lshift_cnt)
   VARIABLE stemp : std_logic_vector (4 DOWNTO 0);
   VARIABLE dtemp : std_logic_vector (27 DOWNTO 0);
   VARIABLE temp : std_logic_vector (27 DOWNTO 0);
   BEGIN
      temp := (OTHERS=> 'X');
      stemp := lshift_cnt;
      temp := SIG_in;
      FOR i IN 4 DOWNTO 0 LOOP
         IF (i < 5) THEN
            IF (stemp(i) = '1' OR stemp(i) = 'H') THEN
               dtemp := (OTHERS => '0');
               dtemp(27 DOWNTO 2**i) := temp(27 - 2**i DOWNTO 0);
            ELSIF (stemp(i) = '0' OR stemp(i) = 'L') THEN
               dtemp := temp;
            ELSE
               dtemp := (OTHERS => 'X');
            END IF;
         ELSE
            IF (stemp(i) = '1' OR stemp(i) = 'H') THEN
               dtemp := (OTHERS => '0');
            ELSIF (stemp(i) = '0' OR stemp(i) = 'L') THEN
               dtemp := temp;
            ELSE
               dtemp := (OTHERS => 'X');
            END IF;
         END IF;
         temp := dtemp;
      END LOOP;
      SIG_lshift <= dtemp;
   END PROCESS I1combo;

   -- ModuleWare code(v1.1) for instance 'I2' of 'sub'
   I2combo: PROCESS (EXP_in, add_in, cin)
   VARIABLE mw_I2t0 : std_logic_vector(8 DOWNTO 0);
   VARIABLE mw_I2t1 : std_logic_vector(8 DOWNTO 0);
   VARIABLE diff : signed(8 DOWNTO 0);
   VARIABLE borrow : std_logic;
   BEGIN
      mw_I2t0 := EXP_in(7) & EXP_in;
      mw_I2t1 := add_in(7) & add_in;
      borrow := cin;
      diff := signed(mw_I2t0) - signed(mw_I2t1) - borrow;
      EXP_lshift <= conv_std_logic_vector(diff(7 DOWNTO 0),8);
   END PROCESS I2combo;

   -- Instance port mappings.
   I0 : FPlzc
      PORT MAP (
         word  => word,
         zero  => zero_int,
         count => count
      );

END struct;
