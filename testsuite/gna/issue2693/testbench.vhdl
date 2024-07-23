package pack1 is 
  type t1 is array(natural range <>) of bit;
  subtype s1 is t1(0 to 7);
end package pack1;

use work.pack1.all;

entity genFunc_sub is
  generic(
          type type1;
          pure function func parameter ( signal in1,in2 : in type1 ) return s1 is <>
         );
  port(
       signal i1   : in type1;
       signal i2   : in type1;
       signal cond : in bit;
       signal out1 : out s1
      );
end entity ;

architecture arch1 of genFunc_sub is
begin
  process (all)
  begin
    if cond then
      out1 <= func(i1,i2);
    else
      out1 <= func(i2,i1);
    end if;
  end process;
end architecture arch1;

use work.pack1.all;    

entity intfSubProgDecl16 is
  port (inp1 : in bit_vector(0 to 7);
        inp2 : in bit_vector(0 to 7);
        cond : in bit;
        outp : out s1
       );
end entity;   
   
architecture arch of intfSubProgDecl16 is
pure function func  parameter ( signal in1,in2 : in bit_vector(0 to 7)) return s1 is
   begin
      if in1 = in2 then
          return "00000000";
      else
           return "11111111";
      end if; 
end function;
begin
   b1 : block is
        begin
            inst1 : entity work.genFunc_sub(arch1)
              generic map ( type1 => bit_vector(0 to 7))
              port map ( inp1,inp2,cond,outp);
        end block;
end architecture arch; 


library STD, IEEE;
use STD.textio.all;
use IEEE.std_logic_1164.all;
use work.pack1.all;

entity TESTBENCH is
end;

architecture TESTBENCH of TESTBENCH is
component intfSubProgDecl16
	port(
		INp1 : in BIT_VECTOR( 0 to 7 );
		INp2 : in BIT_VECTOR( 0 to 7 );
		COND : in BIT;
		OUTP : out s1
	);
end component;

signal tb_IN1 : BIT_VECTOR( 0 to 7 );
signal tb_IN2 : BIT_VECTOR( 0 to 7 );
signal tb_COND : BIT;
signal tb_OUTP : s1;

signal tb_driver : std_logic_vector(0 to 16);

begin

  tb_inst : intfSubProgDecl16 
    port map(tb_IN1,
             tb_IN2,
             tb_COND,
             tb_OUTP
            );

   process
   begin
     wait for 1 ns;
     tb_IN1  <= "00110011";
     tb_IN2  <= "00110011";
     wait for 1 ns;
     tb_IN2  <= "00110010";
     wait for 1 ns;
     tb_IN2  <= "00110011";
     wait for 1 ns;
     tb_IN1  <= "10110011";
     wait for 1 ns;
     wait;
   end process;

end;
