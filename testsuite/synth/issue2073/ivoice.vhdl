-- Massively reduced testcase - the actual file I'm attempting to build is here:
-- https://github.com/MiSTer-devel/Intv_MiSTer/blob/master/rtl/intv/ivoice.vhd

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY ivoice IS
  PORT (
    sound     : OUT signed(15 downto 0);
    clksys    : IN  std_logic; --- 43MHz ... 48MHz
    reset_na  : IN std_logic
    );
END ENTITY ivoice;

ARCHITECTURE rtl OF ivoice IS
  SUBTYPE sv16   IS signed(15  DOWNTO 0);
  SUBTYPE uv8    IS unsigned(7   DOWNTO 0);
  SUBTYPE uv16   IS unsigned(15  DOWNTO 0);
  SUBTYPE uv19   IS unsigned(18  DOWNTO 0);
  SUBTYPE uv20   IS unsigned(19  DOWNTO 0);
  SUBTYPE int16 IS integer RANGE -32768 TO 32767;
  SUBTYPE uint4  IS natural RANGE 0 TO 15;
  SUBTYPE uint5  IS natural RANGE 0 TO 31;
  SUBTYPE uint16 IS natural RANGE 0 TO 65535;

  TYPE enum_state IS (
    sIDLE,sDECODE1,sDECODE2,sDECODE3,sMICROCODE,
    sGENE1,sGENE2,sGENE3,sGENE4,
    sCALC01,sCALC02,sCALC11,sCALC12,sCALC21,sCALC22,
    sCALC31,sCALC32,sCALC41,sCALC42,sCALC51,sCALC52,
    sSOUND);
  SIGNAL state,state2 : enum_state;

  FUNCTION bswap(v : unsigned) RETURN unsigned IS
    VARIABLE u,x: unsigned(0 TO v'length-1) :=v;
  BEGIN
    FOR i IN 0 TO v'length-1 LOOP
      x(v'length-1-i):=u(i);
    END LOOP;
    return x;
  END FUNCTION;

  SIGNAL pc,ret_pc : uv19;

  FUNCTION sat(i : integer) RETURN integer IS
  BEGIN
    IF i>127 THEN RETURN 127; END IF;
    IF i<-128 THEN  RETURN -128; END IF;
    RETURN i;
  END FUNCTION;

  SIGNAL samp : int16 := 0;

  SIGNAL fifoptr : uint5;
  SIGNAL romd : uv16;
  SIGNAL fifod : uv20;
  SIGNAL rom_a : uint16;
  SIGNAL rom_dr : uv8;

BEGIN

  ------------------------------------------------------------------------------
  -- Sequencer
  Machine:PROCESS(clksys,reset_na) IS
    VARIABLE romd_v,fifod_v,imm_v,inst_v,code_v : uv8;
    VARIABLE tmp_v : uv16;
    VARIABLE len_v : uint4;
    VARIABLE pc_v : uv19;
    VARIABLE branch_v : boolean;
  BEGIN
    IF rising_edge(clksys) THEN
      ------------------------------------------------------

      romd_v:=romd(7+to_integer(pc(2 DOWNTO 0)) DOWNTO
                     to_integer(pc(2 DOWNTO 0)));

      code_v:=romd_v;

        CASE state IS

            -------------------------------------------------
          WHEN sDECODE1 =>
            inst_v:=bswap(code_v);
            state<=sDECODE2;
            IF inst_v(7 DOWNTO 4)="0000" THEN
              state<=sGENE1; -- If Zero repeat, skip instruction
            END IF;

          -----------------------------------------------
          -- Sound output.
          WHEN sSOUND =>
              sound<=to_signed(sat(samp/4)*256,16);

            when others =>
            	null;

          -----------------------------------------------
        END CASE;

      ---------------------------------------------------
    END IF;
  END PROCESS;

END ARCHITECTURE rtl;
