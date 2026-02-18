library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package lru_types is
  type log2arr is array(0 to 2048) of integer;
  constant log2   : log2arr := (
0,0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  others => 11);

  constant s012 : std_logic_vector(2 downto 0) := "000";
  constant s021 : std_logic_vector(2 downto 0) := "001";
  constant s102 : std_logic_vector(2 downto 0) := "010";
  constant s120 : std_logic_vector(2 downto 0) := "011";
  constant s201 : std_logic_vector(2 downto 0) := "100";
  constant s210 : std_logic_vector(2 downto 0) := "101";


  constant s0123 : std_logic_vector(4 downto 0) := "00011";
  constant s0132 : std_logic_vector(4 downto 0) := "00010";
  constant s0213 : std_logic_vector(4 downto 0) := "00111";
  constant s0231 : std_logic_vector(4 downto 0) := "00001";
  constant s0312 : std_logic_vector(4 downto 0) := "00110";
  constant s0321 : std_logic_vector(4 downto 0) := "00101";
  constant s1023 : std_logic_vector(4 downto 0) := "01011";
  constant s1032 : std_logic_vector(4 downto 0) := "01010";
  constant s1203 : std_logic_vector(4 downto 0) := "01111";
  constant s1230 : std_logic_vector(4 downto 0) := "01000";
  constant s1302 : std_logic_vector(4 downto 0) := "01110";
  constant s1320 : std_logic_vector(4 downto 0) := "01100";
  constant s2013 : std_logic_vector(4 downto 0) := "10011";
  constant s2031 : std_logic_vector(4 downto 0) := "10001";
  constant s2103 : std_logic_vector(4 downto 0) := "10111";
  constant s2130 : std_logic_vector(4 downto 0) := "10000";
  constant s2301 : std_logic_vector(4 downto 0) := "10101";
  constant s2310 : std_logic_vector(4 downto 0) := "10100";
  constant s3012 : std_logic_vector(4 downto 0) := "11010";
  constant s3021 : std_logic_vector(4 downto 0) := "11001";
  constant s3102 : std_logic_vector(4 downto 0) := "11110";
  constant s3120 : std_logic_vector(4 downto 0) := "11000";
  constant s3201 : std_logic_vector(4 downto 0) := "11101";
  constant s3210 : std_logic_vector(4 downto 0) := "11100";

  type lru_3way_table_vector_type is array(0 to 2) of std_logic_vector(2 downto 0);
  type lru_3way_table_type is array (0 to 7) of lru_3way_table_vector_type;

  constant lru_3way_table : lru_3way_table_type :=
    ( (s120, s021, s012),                   -- s012
      (s210, s021, s012),                   -- s021
      (s120, s021, s102),                   -- s102
      (s120, s201, s102),                   -- s120
      (s210, s201, s012),                   -- s201
      (s210, s201, s102),                   -- s210
      (s210, s201, s102),                   -- dummy
      (s210, s201, s102)                    -- dummy
      );

  type lru_4way_table_vector_type is array(0 to 3) of std_logic_vector(4 downto 0);
  type lru_4way_table_type is array(0 to 31) of lru_4way_table_vector_type;

  constant lru_4way_table : lru_4way_table_type :=
    ( (s2310, s0231, s0312, s0213),       -- "00000" (s0231/reset)
      (s2310, s0231, s0312, s0213),       -- "00001" s0231
      (s1320, s0321, s0132, s0123),       -- "00010" s0132
      (s1230, s0231, s0132, s0123),       -- "00011" s0123
      (s3210, s0321, s0312, s0213),       -- "00100" (s0321)
      (s3210, s0321, s0312, s0213),       -- "00101" s0321
      (s3120, s0321, s0312, s0123),       -- "00110" s0312
      (s2130, s0231, s0132, s0213),       -- "00111" s0213
      (s1230, s2301, s1302, s1203),       -- "01000" s1230
      (s1230, s2301, s1302, s1203),       -- "01001" (s1230)
      (s1320, s0321, s1032, s1023),       -- "01010" s1032
      (s1230, s0231, s1032, s1023),       -- "01011" s1023
      (s1320, s3201, s1302, s1203),       -- "01100" s1320
      (s1320, s3201, s1302, s1203),       -- "01101" (s1320)
      (s1320, s3021, s1302, s1023),       -- "01110" s1302
      (s1230, s2031, s1032, s1203),       -- "01111" s1203
      (s2130, s2301, s1302, s2103),       -- "10000" s2130
      (s2310, s2031, s0312, s2013),       -- "10001" s2031
      (s2130, s2031, s0132, s2013),       -- "10010" (s2013)
      (s2130, s2031, s0132, s2013),       -- "10011" s2013
      (s2310, s2301, s3102, s2103),       -- "10100" s2310
      (s2310, s2301, s3012, s2013),       -- "10101" s2301
      (s2130, s2031, s1032, s2103),       -- "10110" (s2103)
      (s2130, s2031, s1032, s2103),       -- "10111" s2103
      (s3120, s3201, s3102, s1203),       -- "11000" s3120
      (s3210, s3021, s3012, s0213),       -- "11001" s3021
      (s3120, s3021, s3012, s0123),       -- "11010" s3012
      (s3120, s3021, s3012, s0123),       -- "11011" (s3012)
      (s3210, s3201, s3102, s2103),       -- "11100" s3210
      (s3210, s3201, s3012, s2013),       -- "11101" s3201
      (s3120, s3021, s3102, s1023),       -- "11110" s3102
      (s3120, s3021, s3102, s1023)        -- "11111" (s3102)
      );
  subtype lruent is std_logic_vector(4 downto 0);
  type lruarr is array(natural range <>) of lruent;

  constant dwaysize     : integer := 4;
  constant dlinesize    : integer := 8;
  constant dphysbits    : integer := 32;
  constant DLINE_BITS   : integer := log2(dlinesize);
  constant DOFFSET_BITS : integer := 8 +log2(dwaysize) - DLINE_BITS;
  constant DTAG_HIGH    : integer := dphysbits-1;
  constant DTAG_LOW     : integer := DOFFSET_BITS + DLINE_BITS + 2;
  constant DOFFSET_HIGH : integer := DTAG_LOW - 1;
  constant DOFFSET_LOW  : integer := DLINE_BITS + 2;

end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.lru_types.all;

entity lru_reproducer is
    generic ( nways : integer range 1 to 4 := 4 );
    port (
        oent   : in  lruent;
        hway   : in  unsigned(1 downto 0);
        idx    : in  integer;
        vaddr  : in  std_logic_vector(dphysbits-1 downto 0);
        result : out lruent
    );
end entity;

architecture rtl of lru_reproducer is
    function calc_lruent(oent: lruent; hway: unsigned(1 downto 0); nways: integer) return lruent is 
        variable nent: lruent; 
    begin 
        nent := (others => '0'); 
        case nways is 
            when 1 => 
                nent := "00000"; 
            when 2 => 
                nent(4) := '0'; 
                nent(3) := not hway(0); 
                nent(2 downto 0) := "000"; 
            when 3 => 
                nent(4 downto 2) := lru_3way_table(to_integer(unsigned(oent(4 downto 2))))(to_integer(hway)); 
                nent(1 downto 0) := "00"; 
            when others => 
                nent := lru_4way_table(to_integer(unsigned(oent)))(to_integer(hway)); 
        end case; 
        return nent; 
    end function;

begin
    process(oent, hway, idx, vaddr)
    variable dlru : lruarr(0 to 2**8 - 1);
    begin
        dlru(to_integer(unsigned(vaddr(DOFFSET_HIGH downto DOFFSET_LOW)))) := calc_lruent(oent,hway,nways);
        result <= dlru(idx);
    end process;
end architecture;
