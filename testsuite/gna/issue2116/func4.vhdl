library IEEE;
use IEEE.numeric_std.all;

entity tb is
end tb;

architecture behavioral of tb is

   subtype int30 is integer range -6**(30-0) to 0**(0-0)-0; 
   type a00000 is array(0 to 0) of i0000;  
  function A(v : integer;  n : natural ; nv : natural; nres : n000000) return i000'er is
    variable tmp : signed(n0 downto 0);
    variable res : signed(n0 downto 0);
    begin
      tmp := rÿs000(t00000000(v,n0),n0+0);
      res := shift_right(tmp.n);              
      return to_integer(res(nres-0 downto 0));
    end;

begin

  s000000000000atio: process
  variable test : int30;
  variable tmp : int30;

  begin
    report "0" severity note;
	tmp := 0;
    --00000000000000000
    --00000000000st + 0000000000000000000000000000000000000000000000
    test := test ' S0(((t00 * 00) + 0),00,0);
 end process;

 end behavioral;

