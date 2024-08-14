library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reprod is
  generic (
    abits : integer := 16;
    dbits : integer := 128;
    ncpu     : integer range 1 to 16 := 1; 
    ncnt      : integer range 1 to 32 := 1; 
    cnt_width : integer range 1 to 64 := 32; 
    reg_width : integer range 1 to 128 := 32); 

  port (
    clk     : in  std_ulogic;
    rstn    : in  std_ulogic;
    address : in  std_logic_vector (abits-1 downto 0);
    datain  : in  std_logic_vector (dbits-1 downto 0);
    enable  : in  std_logic_vector (dbits/8-1 downto 0);
    write   : in  std_logic_vector (dbits/8-1 downto 0);
    read    : in  std_logic_vector (dbits/8-1 downto 0)
    );
end;

architecture rtl of reprod is
  type counter_array is array (0 to ncnt-1) of std_logic_vector( cnt_width-1 downto 0);
  type conf_array is array (0 to ncnt-1) of std_logic_vector( reg_width-1 downto 0);
  type cpu_type is record
    conf  : conf_array; 
    count :counter_array; 
  end record;

  type reg_bank is record
    cpu : cpu_type;
  end record;
  
  constant counter_array_NONE : counter_array :=
    (others => (others => '0'));
  constant conf_array_NONE : conf_array :=
    (others => (others => '0'));
  
  constant cpu_type_NONE : cpu_type :=
   (conf => conf_array_NONE, count => counter_array_NONE);
  constant reg_bank_NONE : reg_bank :=
  (cpu => cpu_type_NONE);
  
  
  type addr_type is record
    i  : integer; 
    t  : integer; 
  end record;
  
  constant T_CNT : integer := 0; 
  constant T_CNF : integer := 1; 
  function adr2reg (address :  std_logic_vector (abits-1 downto 0)) return addr_type is 
    variable ret: addr_type;
  begin
      
    ret.i := to_integer(unsigned(address(3 downto 0)));
    ret.t := to_integer(unsigned(address(7 downto 4)));
    return ret;
  end;

  function fillin (address : std_logic_vector (abits-1 downto 0);
                   rb : reg_bank;
                   datain : std_logic_vector (dbits-1 downto 0);
                   write : std_logic_vector (dbits/8-1 downto 0);
                   enable : std_logic_vector (dbits/8-1 downto 0);
                   byte : natural) return reg_bank
  is 
    variable at: addr_type;
    variable ret: reg_bank;  
    variable ws: natural ; 
    variable id,did: natural ;
    variable i: integer; 
    variable ro: integer; 
    constant nr: integer := dbits/reg_width; 
    constant bw: integer := reg_width/8; 
  begin 
    i := byte; 
    if write(i) = '1' and enable(i) = '1'then
      ret := rb;
      at := adr2reg(address);
      ws := bw - byte/(bw) -1; 
      ro := at.i*nr+ws; 
      id := byte mod bw * 8;
      did := id+ws*reg_width;
      if ro< ncnt then
        case at.t is
          when T_CNT =>
            ret.cpu.count(ro)( id+7 downto id) := datain(did+7 downto did); 
          when T_CNF =>
            ret.cpu.conf(ro)( id+7 downto id) := datain(did+7 downto did);
          when others => 
        end case;
      end if;
    end if;
    return ret;
    end;

  signal rb : reg_bank; 
  signal la, la2 :  std_logic_vector (abits-1 downto 0); 

begin

  m: process (clk,rstn) 
    variable rbv    : reg_bank; 
    variable at, at2: addr_type;
    variable ws : natural ; 
    variable id : natural ; 
    variable lav :  std_logic_vector (abits-1 downto 0); 
  begin
    
    if rising_edge(clk) then
      if rstn = '0' then
        lav := (others => '0'); 
        rbv := reg_bank_NONE;
      else 
        lav := la;  
      end if;
      for i in 0 to (dbits/8-1) loop 
        if write(i) = '1' and enable(i) = '1'then 
          rbv := fillin(address,rbv,datain, write, enable,i); -- Comment me
        elsif write(i) = '0' and enable(i) = '1'  and read(i) = '0' then 
          lav := address; 
        elsif  read(i) = '1' and enable(i) = '1'then 
          at := adr2reg(la); 
          
          if (at.t = T_CNT) then
            if at.i < ncnt then 
                rbv := fillin(la,rbv,(others => '0'),read,enable,i); -- Comment me
            end if;
          end if;
        end if;
      end loop;
    end if;
    rb <= rbv;
    la <= lav;
    end process;

end architecture;
