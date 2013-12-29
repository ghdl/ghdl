library IEEE;
use IEEE.numeric_std.all;

entity tb is
end tb;

architecture behavioral of tb is

   subtype int31 is integer range -2**(31-1) to 2**(31-1)-1; 
   type array_7_int31 is array(0 to 6) of int31;   
  
  function ASR(v : integer;  n : natural ; nv : natural; nres : natural) return integer is
    variable tmp : signed(nv downto 0);
    variable res : signed(nv downto 0);
    begin
      tmp := resize(to_signed(v,nv),nv+1);
      res := shift_right(tmp,n);              
      return to_integer(res(nres-1 downto 0));
    end;

begin

  software_emulation : process
  variable test : int31;
  variable tmp : int31;

  begin
    report "Start" severity note;
	tmp := 5965232;
    -- test := test + ASR(((tmp * 119304647) + 268435456),29,57,31);
    -- test := test + ASR(((tmp * 178956971) + 268435456),29,57,31);
    test := test + ASR(((tmp * 59652324) + 268435456),29,57,31);
 end process;

 end behavioral;

