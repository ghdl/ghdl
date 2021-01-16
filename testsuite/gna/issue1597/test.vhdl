------------------------------------------------
--!  Test intent : Procedure calls with signals parameters.
--!  Test scope  : Checking activity with  'active
--!  Keywords    : [procedure, parameters, signal]
--!  References  : [VH2000 1.1:]
--!                [Rlink REQ00:2.1.1.2.1]
--!                [Rlink REQ00:2.1.1.2.4]
--
-------------------------------------------------
--  'active 
-- 
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use work.subs_pkg.all;
use work.proc_pkg.all;
use work.all;

entity pf7 is
end entity;

architecture tb of pf7 is

  signal tclk : std_logic := '0';
  signal trst : std_logic := '0';
  signal tper : integer := 1;
  
  signal u1_in:  std_logic_vector(7 downto 0);
  signal u1_out: std_logic_vector(7 downto 0);
  signal u2_in:  std_logic_vector(7 downto 0);
  signal u2_out: std_logic_vector(7 downto 0);
  
  signal gen_out : std_logic_vector(15 downto 0);
  
  signal mems : arr_marr_t;
  
  signal mem_if_in : ctl_if_t;
  signal mem_if_out : rsp_if_t;
  signal mon_if_in : ctl_if_t;
  signal mon_if_out : rsp_if_t;
  signal ackm : std_logic;
  
  signal sig_act : boolean;

begin

  act_mon(mem_if_in, mem_if_out, sig_act);

  process(mem_if_in)
  begin
    for i in mem_if_in.d1'range loop
      if mem_if_in.d1(i)'active then
        report "Bit : " & integer'image(i) & " is active";
      end if;
    end loop;
  end process;

  -- clock
  clock: process
  begin
    tclk <= '0';
    wait for 1 ns;
    tclk <= '1';
    wait for 1 ns;
  end process;
  
  sim_ctl: process
    variable v_cnt : integer := 0;
  begin
    while v_cnt < 5 loop
      v_cnt := v_cnt + 1;
      --report "Tick ...";
      wait until tclk'event and tclk = '1';
    end loop;
    report "Passed, END SIM ..." severity failure;
  end process;

  -- generate some data
  dgen: process(tclk)
    variable vec : bit_vector(15 downto 0) := (others => '1');
  begin
    if tclk'event and tclk = '1' then
      vec := klsfr(vec);
      --report bv2str(vec);
      gen_out <= to_stdlogicvector(vec);
      u1_in  <= to_stdlogicvector(vec(7 downto 0));
      u2_in  <= to_stdlogicvector(vec(15 downto 8));
    end if;
  end process;

  --  memory access process
  mem_acc: process
    variable addr : std_logic_vector(15 downto 0) := (others => '0');
    variable idx : integer;
    variable  init : integer := 0;
    variable v_tmp_dat : std_logic_vector(63 downto 0);
  begin
    if init = 0 then
      for i in 3 downto 0 loop
        for j in marr_t'range loop
          mems(i)(j) <= (others => '0');
          wait for 0 ps;
        end loop;
      end loop;
      report "Done.";
      wait for 0 ps;
      for i in 3 downto 0 loop
        for j in marr_t'range loop
          assert kslv2int(mems(i)(j)) = 0 
            report "ERROR: Ram not initialized"
            severity failure;
        end loop;
      end loop;
      mem_if_in.d1 <= (others => '0');
      init := 1;
      
    elsif tclk'event and tclk = '1' then
      mem_access(mem_if_in, mem_if_out, mems, ackm);
      mem_if_in.wr <= '0';
      mem_access(mem_if_in, mem_if_out, mems, ackm);
      --report "Active Drive state: " & slv2str(active_bits(mem_if_out.do));
      --report "Inactive outputs Drive state: " & slv2str(active_bits(mem_if_out.do1));
      wait for 0 ps;
      --report "Inactive Drive state: " & slv2str(active_bits(mem_if_out.do));
      --report "Data: " & slv2str(mem_if_out.do);
      if now > 2 ns then
        assert v_tmp_dat(63 downto 16) = mem_if_out.do(63 downto 16)
          report "Error:  data missmatch on read ..."
          severity failure;
      end if;
    elsif tclk'event and tclk = '0' then
      mem_if_in.a1 <= addr;
      mem_if_in.wr <= '1';
      addr := addr + 16#0001#;
      --report "Address: " & slv2str(addr);
      mem_if_in.di <= gen_out & gen_out(7 downto 0) & gen_out(15 downto 8) & gen_out & gen_out;
      v_tmp_dat := gen_out & gen_out(7 downto 0) & gen_out(15 downto 8) & gen_out & gen_out;
      
      -- generate events on ctl.d1
      wait for 2 ps;
      if now > 2 ns then
        for i in mem_if_in.d1'range loop
          mem_if_in.d1(i) <= not mem_if_in.d1(i);
          wait for 0 ps;
        end loop;
        report slv2str(mem_if_in.d1);
      end if;
    end if;
    wait on tclk;
  end process;

end tb;
