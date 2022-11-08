library ieee;
use ieee.std_logic_1164.all;

entity case_concat is
end entity;

architecture behaviour of case_concat is

  subtype sel_t is std_logic_vector(1 downto 0);
  constant S0 : sel_t := '0' & '0';
  constant S1 : sel_t := '0' & '1';
  constant S2 : sel_t := '1' & '1';
  constant S3 : sel_t := '1' & '0';
  
  signal sel : sel_t;
  
begin

  process (sel)
  begin
    case sel is
      when S0 => null;
      when S1 => null;
      when S2 => null;
      when S3 => null;
      when others => null; 
    end case;
  end process;
  
end architecture;

