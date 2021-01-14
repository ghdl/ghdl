------------------------------------------------
--  Test intent : collection of useful functions and procs
--  Test scope  : Basic File write of type character
--  Keywords    : [function, procedure]
--!  References    [VH1993 3.4: ]
--!                [VH2019 5.5: ]
-------------------------------------------------
--  
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package subs_pkg is

  --  procedures
  procedure msg(str : in string);  -- output a system message string.

  --  functions
  function bv2str(bv: bit_vector) return string; -- convert bit_vector to string
  function slv2str(slv: std_logic_vector) return string; -- convert std_logic_vector to string
  function sulv2str(sulv: std_ulogic_vector) return string; -- convert std_ulogic_vector to string
  function klsfr(bv: bit_vector) return bit_vector;  -- do LSFR on bit_vector 8, 16 32
  function klsfr(bv: std_logic_vector) return std_logic_vector;  -- do LSFR on bit_vector 8, 16 32
  
end;

package body subs_pkg is

  -- simple message output.
  procedure msg(str : in string) is
  begin 
    report "System Message: " & str;
  end procedure;


  --  take in bit_vector return string.
  function bv2str(bv: bit_vector) return string is
    variable st_out : string(1 to bv'length);
    alias v : bit_vector(1 to bv'length) is bv;
  begin
    for i in v'range loop
      if(v(i) = '0') then
        st_out(i) := '0';
      else
        st_out(i) := '1';
      end if;
    end loop;
    return st_out;
  end function;
    
  --  take in std_logic_vector return string.
  function slv2str(slv: std_logic_vector) return string is
    variable st_out  : string(1 to slv'length);
    alias v : std_logic_vector(1 to slv'length) is slv; 
  begin 
    for i in v'range loop 
      case v(i) is 
        when 'U' => 
          st_out(i) := 'U'; 
        when 'X' => 
          st_out(i) := 'X'; 
        when '0' => 
          st_out(i) := '0'; 
        when '1' => 
          st_out(i) := '1'; 
        when 'Z' => 
          st_out(i) := 'Z'; 
        when 'W' => 
          st_out(i) := 'W'; 
        when 'L' => 
          st_out(i) := 'L'; 
        when 'H' => 
          st_out(i) := 'H'; 
        when '-' => 
          st_out(i) := '-'; 
        when others => 
          assert false report "STD_LOGIC_VALUE not in value set." severity failure; 
      end case; 
    end loop; 
    return st_out; 
  end function; 
  
  -- to 
  function sulv2str(sulv: std_ulogic_vector) return string is
    variable st_out  : string(1 to sulv'length);
    alias v : std_ulogic_vector(1 to sulv'length) is sulv; 
  begin
    for i in v'range loop 
      case v(i) is 
        when 'U' => 
          st_out(i) := 'U'; 
        when 'X' => 
          st_out(i) := 'X'; 
        when '0' => 
          st_out(i) := '0'; 
        when '1' => 
          st_out(i) := '1'; 
        when 'Z' => 
          st_out(i) := 'Z'; 
        when 'W' => 
          st_out(i) := 'W'; 
        when 'L' => 
          st_out(i) := 'L'; 
        when 'H' => 
          st_out(i) := 'H'; 
        when '-' => 
          st_out(i) := '-'; 
        when others => 
          assert false report "STD_ULOGIC_VALUE not in value set." severity failure; 
      end case; 
    end loop; 
    return st_out; 
      --return slv2str(std_logic_vector(to_unsigned(sulv)));
  end function;
    
  -- bitvector lsfr
  function klsfr(bv: bit_vector) return bit_vector is
    alias v : bit_vector(bv'high downto 0) is bv;
    variable rtn : bit_vector(bv'high downto 0);
    variable len : integer := bv'length;
  begin
    case len is
      when 8 =>
        rtn := v(6 downto 0) & ((v(7) xor v(4)) xor (v(1) xor v(2)));
      when 16 =>
        rtn := v(14 downto 0) & ((v(15) xor v(14)) xor (v(12) xor v(3)));
      when 32 =>
        rtn := v(30 downto 0) & ((v(31) xor v(6)) xor (v(5) xor v(1)));
      when others =>
        report "ERROR: LSFR size not implemented ..." severity failure;
    end case;
    return rtn;
  end function;
    
  function klsfr(bv: std_logic_vector) return std_logic_vector is
    alias v : std_logic_vector(bv'high downto 0) is bv;
    variable rtn : std_logic_vector(bv'high downto 0);
    variable len : integer := bv'length;
  begin
    for i in bv'range loop
      if (bv(i) /= '1' and
         bv(i) /= '0') then
        report "klsfr got a none logic value passed ..." severity failure;
      end if;
    end loop;
  
    case len is
      when 8 =>
        rtn := v(6 downto 0) & ((v(7) xor v(4)) xor (v(1) xor v(2)));
      when 16 =>
        rtn := v(14 downto 0) & ((v(15) xor v(14)) xor (v(12) xor v(3)));
      when 32 =>
        rtn := v(30 downto 0) & ((v(31) xor v(6)) xor (v(5) xor v(1)));
      when others =>
        report "ERROR: LSFR size not implemented ..." severity failure;
    end case;
    return rtn;
  end function;
    
end package body;
