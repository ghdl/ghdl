library ieee;
use ieee.std_logic_1164.all;

entity fun is
  port (
    d : in  std_logic_vector(3 downto 0);
    q : out std_logic_vector(4 downto 0)
    );
end entity;

architecture behaviour of fun is

  function fp (i : natural; v : std_logic_vector(3 downto 0)) return std_logic is
  begin
    if i > 3 or i < 0
    then
      return 'X';
    else
      return v(i);
    end if;
  end function;
  
begin

  process (d)

    impure function get_fp1 (i : natural) return std_logic is
    begin
      return fp(i, d);
    end function;

    impure function get_fp2 (i : natural) return std_logic is
    begin
      if i > 3 or i < 0
      then
        return 'X';
      else
        return d(i);
      end if;
    end function;

    variable fp3 : std_logic := '0';
    impure function get_fp3 return std_logic is
    begin
      fp3 := not fp3;
      return fp3;
    end function;

    impure function get_fp4 return std_logic is
    begin
      return '0';
    end function;
    
  begin
    q(0) <= fp(0, d);
    q(1) <= get_fp4;
    q(2) <= get_fp1(0);
    q(3) <= get_fp2(0);
    q(4) <= get_fp3;
  end process;
  
end architecture;

