library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reprod is
  generic (
    abits : integer := 16;
    dbits : integer := 88;
    width : integer range 1 to 128 := 32); 
  port (
    clk     : in  std_ulogic;
    rstn    : in  std_ulogic;
    address : in  std_logic_vector (abits-1 downto 0);
    datain  : in  std_logic_vector (dbits-1 downto 0);
    enable  : in  std_logic_vector (dbits/8-1 downto 0);
    write   : in  std_logic_vector (dbits/8-1 downto 0);
    read    : in  std_logic_vector (dbits/8-1 downto 0);
    do : out std_logic_vector(width - 1 downto 0)
    );
end;

architecture rtl of reprod is
  type reg_bank is record
    conf  : std_logic_vector( width-1 downto 0);
    count :std_logic_vector( width-1 downto 0);
  end record;
  
  constant reg_bank_NONE : reg_bank :=
    (conf => (others => '0'), count => (others => '0'));
  
  constant T_CNT : integer := 0; 
  constant T_CNF : integer := 1; 

  function fillin (address : std_logic_vector (abits-1 downto 0);
                   rb : reg_bank;
                   datain : std_logic_vector (dbits-1 downto 0);
                   write : std_logic_vector (dbits/8-1 downto 0);
                   byte : natural) return reg_bank
  is 
    variable ret: reg_bank;  
    variable id: natural ;
    constant bw: integer := width/8; 
  begin 
    if write(byte) = '1' then
      ret := rb;
      id := byte mod bw * 8;
      case to_integer(unsigned(address(1 downto 0))) is
        when T_CNT =>
          ret.count(id+7 downto id) := datain(id+7 downto id); 
        when T_CNF =>
          ret.conf(id+7 downto id) := datain(id+7 downto id);
        when others => 
      end case;
    end if;
    return ret;
    end;

  signal rb : reg_bank; 
  signal la :  std_logic_vector (abits-1 downto 0); 

begin

  m: process (clk,rstn) 
    variable rbv    : reg_bank; 
  begin
    
    if rising_edge(clk) then
      if rstn = '0' then
        rbv := reg_bank_NONE;
      end if;
      for i in 0 to (dbits/8-1) loop 
        if write(i) = '1' and enable(i) = '1'then 
          rbv := fillin(address,rbv,datain, write, i); -- Comment me
        elsif write(i) = '0' and enable(i) = '1'  and read(i) = '0' then 
          la <= address; 
        elsif  read(i) = '1' and enable(i) = '1'then 
          rbv := fillin(la,rbv,(others => '0'),read,i); -- Comment me
        end if;
      end loop;
    end if;
    rb <= rbv;

    if la = (abits - 1 downto 0 => '0') then
      do <= rb.conf;
    else
      do <= rb.count;
    end if;
  end process;

end architecture;
