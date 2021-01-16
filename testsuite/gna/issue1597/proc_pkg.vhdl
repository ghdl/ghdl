------------------------------------------------
--!  Test intent : declaration of procedure in package
--!  Test scope  : function with active checking
--!  Keywords    : [function, attribute, active]
--!  References  : [VH2000 1.1:]
--!                [Rlink REQ00:2.1.1.2.1]
--!                [Rlink REQ00:14.1.38]
--
-------------------------------------------------
--  declare a procedures and functions to demonstrate
--  attributes.
--    with partially driven bus we should see ativity as expected.
--   
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.subs_pkg.all;

package proc_pkg is

  type ctl_if_t is record
    d1 : std_logic_vector(31 downto 0);
    d2 : std_logic_vector(31 downto 0);
    a1 : std_logic_vector(15 downto 0);
    wr  : std_logic;
    di  : std_logic_vector(63 downto 0);
    dv  : std_logic;
  end record;

  type rsp_if_t is record
    do1 : std_logic_vector(31 downto 0);
    do2 : std_logic_vector(31 downto 0);
    sel : std_logic_vector(3 downto 0);
    addr : std_logic_vector(11 downto 0);
    do  : std_logic_vector(63 downto 0);
  end record;

  type marr_t is array(15 downto 0) of std_logic_vector(63 downto 0);
  type arr_marr_t is array(3 downto 0) of marr_t;

  procedure mem_access(signal ctl : in ctl_if_t;
                       signal rsp : out rsp_if_t;
                       signal mem : inout arr_marr_t;
                       signal ack : out std_logic);

  procedure mem_obj(signal mem : in marr_t;
                    signal addr : in std_logic_vector(11 downto 0);
                    signal wr : in std_logic;
                    signal w_in : in std_logic_vector(63 downto 0);
                    signal mem_out : out marr_t;
                    signal dout : out std_logic_vector(63 downto 0)
                    );

  procedure proc_mix(signal val1 : in std_logic_vector;
                     signal val2 : in std_logic_vector);

  function kslv2int(vec : std_logic_vector) return integer;
  
  function active_bit(signal vec : std_logic_vector;
                      idx : integer) return boolean;

  function active_bits(signal vec : std_logic_vector) return std_logic_vector;

  procedure act_mon(signal ctl : in ctl_if_t;
                    signal rsp : in rsp_if_t;
                    signal act : out boolean
                    );

end package proc_pkg;

package body proc_pkg is

  --  activity  functions.
  function active_bit(signal vec : std_logic_vector;
                      idx : integer) return boolean is
  begin
    return vec(idx)'active;
  end function;
  

  function active_bits(signal vec : std_logic_vector) return std_logic_vector is
    variable rtnv : std_logic_vector(vec'range);
  begin
    for i in vec'range loop
      if active_bit(vec, i) then
        rtnv(i) := '1';
      else
        rtnv(i) := '0';
      end if;
    end loop;
    return rtnv;
  end function;


  -- convert std_logic_vector to integer.
  function kslv2int(vec : std_logic_vector) return integer is
    variable rtn : integer := 0;
    variable len : integer := (vec'length) - 1;
  begin
    --report "Vector in: " & slv2str(vec);
    for i in vec'range loop
      if vec(i) = '1' then
        rtn := rtn + (2**i);
      end if;
    end loop;
    --report "Integer out: " & integer'image(rtn);
    return rtn;
  end function;

  procedure act_mon(signal ctl : in ctl_if_t;
                    signal rsp : in rsp_if_t;
                    signal act : out boolean
                    ) is
  variable delta : integer := 0;
  variable ltime : time;
  begin
    while true loop
      act <= false;
      if ctl'active then
        report "ctl is active ...";
      end if;
      
      if rsp'active then
        report "rsp is active ...";
      end if;
      
      if ctl'active or rsp'active then
        if ltime = now then
          delta := delta + 1;
        else
          delta := 0;
        end if;
        report "delta now at: " & integer'image(delta);
        act <= true;
      end if;
      ltime := now;
      --wait for 0 ps;
      wait on ctl, rsp;
    end loop;
  end procedure;
  


  -- access the memory passed.
  procedure mem_obj(signal mem : in marr_t;
                    signal addr : in std_logic_vector(11 downto 0);
                    signal wr : in std_logic;
                    signal w_in : in std_logic_vector(63 downto 0);
                    signal mem_out : out marr_t;
                    signal dout : out std_logic_vector(63 downto 0)
                    ) is
    variable idx : integer := 0;
  begin
    idx := kslv2int(addr);
    --report integer'image(idx);
    dout(63 downto 16) <= mem(kslv2int(addr(3 downto 0)))(63 downto 16);
    mem_out <= mem;
    if wr = '1' then
      --report "Writing mem ...";
      mem_out(kslv2int(addr(3 downto 0))) <= w_in;
    end if;
    wait for 0 ps;
  end procedure;

  -- do a request for memory access.
  procedure mem_access(signal ctl : in ctl_if_t;
                       signal rsp : out rsp_if_t;
                       signal mem : inout arr_marr_t;
                       signal ack : out std_logic) is
    --variable rsp_v : std_logic;
    variable idx : integer;
  begin
    ack <= '0';
    case ctl.a1(7 downto 4) is
      when "0000" =>
        mem_obj(mem(0), ctl.a1(11 downto 0), ctl.wr, ctl.di, mem(0), rsp.do);
      when "0001" =>
        mem_obj(mem(1), ctl.a1(11 downto 0), ctl.wr, ctl.di, mem(1), rsp.do);
      when "0010" =>
        mem_obj(mem(2), ctl.a1(11 downto 0), ctl.wr, ctl.di, mem(2), rsp.do);
      when "0011" =>
        mem_obj(mem(3), ctl.a1(11 downto 0), ctl.wr, ctl.di, mem(3), rsp.do);
      when others =>
        mem_obj(mem(0), ctl.a1(11 downto 0), ctl.wr, ctl.di, mem(0), rsp.do);
    end case;
  
    ack <= '1';
    --wait until  = '1';
  end procedure;
    
    --  the test procedure implemenation
    procedure proc_mix(signal val1 : in std_logic_vector;
                       signal val2 : in std_logic_vector) is
      variable tmp : bit_vector(val1'high downto val1'low);
      variable tmp2 : bit_vector(val2'high downto val2'low);
    begin
      report "-----  start of call  ----";
      wait for 0.5 ns;   --<<  get us off the rising edge event let data settle
      tmp := to_bitvector(val1);
      report "val1 got: " & bv2str(tmp);
      tmp2 := to_bitvector(val2);
      report "val2 got: " & bv2str(tmp2);
      wait for 2 ns;   --<<  get us off the rising edge after next data change.
      report "-----   then  ----";
      tmp := to_bitvector(val1);
      report "val1 got: " & bv2str(tmp);
      tmp2 := to_bitvector(val2);
      report "val2 got: " & bv2str(tmp2);
    end procedure;
    
end proc_pkg;
