library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- sewuence generator
-- note: cos,sin output on negative edge before pha, sync.
--       pha on positive edge, sync on positive edge
entity ram_lut is
port( nreset,clk,wstb,rstb: in std_logic;
cmd: in std_logic_vector(2 downto 0); -- control 
din: in std_logic_vector(7 downto 0);
dout: out std_logic_vector(7 downto 0);
pos_ctr: out std_logic_vector(9 downto 0);
sin,cos,indo,pha,sync:out std_logic);
end ram_lut;

architecture rtl of ram_lut is

component ram2k8 is
port( clk,WEn,CSn: in std_logic; -- clk rising edge, WEn & CSn are active low
ADDR: in std_logic_vector(10 downto 0); 
dw: in std_logic_vector(7 downto 0); -- write data
dr: out std_logic_vector(7 downto 0) -- read data
);
end component;

-- signals for memory
signal mCLK,mWEn,mCEn: std_logic;
signal mA11: std_logic_vector(10 downto 0); -- address for the 2k mem
signal mA: std_logic_vector(8 downto 0); -- normal address
signal mD: std_logic_vector(7 downto 0);
signal mQ: std_logic_vector(7 downto 0);
signal mQr: std_logic_vector(7 downto 0); -- register to hold data

-- signals for sequence generator
signal ctr_lim: unsigned(8 downto 0); -- final byte address
signal dib_ctr: unsigned(1 downto 0);
signal sini,cosi,indoi,phai,synci: std_logic;
signal pos_ctri:unsigned(9 downto 0);
signal lut_ctl: std_logic_vector(7 downto 0); -- see below:

alias lut_run: std_logic is lut_ctl(7);
alias lut_3ph: std_logic is lut_ctl(5);
alias indo_def: std_logic is lut_ctl(4);
alias dib_lim: std_logic_vector(1 downto 0) is lut_ctl(2 downto 1);

signal rd,csgen,csregen:std_logic; -- mem rd, controls for generator
signal mxo:std_logic_vector(2 downto 0);-- mux of data

begin

pos_ctr<=std_logic_vector(pos_ctri);
sync<=synci;
sin<=sini;
cos<=cosi;
pha<=phai;
indo<=indoi;

-- dout
dout<= mQ when cmd="000" else
       std_logic_vector(ctr_lim(7 downto 0)) when cmd="010" else
       lut_ctl when cmd="011" else
       "01111110"; -- 7e

mD<=din; --always

-- modified controls
-- cmd: 000 read or write mem, autoinc
--      001 zero address - wstb
--      010 ctr_lim(7 downto 0) - wstb, rstb
--      011 lut_ctl - wstb,rstb
--      101 
--
-- lut_ctl: run,0,3pha,set_indo,0,dib_lim1,dib_lim0.ctr_lim(8)

 mCEn<='0' when ((cmd="000") and ((wstb='1') or (rstb='1'))) else
       '0' when rd='1' else
       '1'; -- 1 cycle long
 mWEn<='0' when ((cmd="000") and ((wstb='1')))
           else '1';
 mCLK<=not(clk) and not(mCEn); -- only clock when necessary

process(mQ,dib_ctr) begin -- set mxo(2 downto 0) to indoi,mxo(1),mxo(0)
  if lut_3ph='0' then 
    mxo(2)<=indo_def; -- per lut_ctl
    case dib_ctr is
      when "00"=> 
        mxo(0)<=mQ(0);
        mxo(1)<=mQ(1);
      when "01"=> 
        mxo(0)<=mQ(2);
        mxo(1)<=mQ(3);
      when "10"=> 
        mxo(0)<=mQ(4);
        mxo(1)<=mQ(5);
      when others=> 
        mxo(0)<=mQ(6);
        mxo(1)<=mQ(7);
     end case;
   else -- lut_3ph='1'
     case dib_ctr is
       when "00"=>
         mxo(0)<=mQ(0);
         mxo(1)<=mQ(1);
         mxo(2)<=mq(2);
       when others=>
         mxo(0)<=mQ(4);
         mxo(1)<=mQ(5);
         mxo(2)<=mq(6);
     end case;
   end if;
end process;

process (nreset,clk) -- generator
begin
  if nreset='0' then
    synci<='0';
    cosi<='0';
    sini<='0';
    phai<='0';
    pos_ctri<=(others=>'0');
    csgen<='0'; -- 1 when running
    csregen<='0'; -- 1 when output cos,sin to register
    dib_ctr<=(others=>'0');
    mQr<=(others=>'0');
    rd<='0'; -- read data byte
  elsif clk'event and clk='1' then
    synci<='0'; -- will give a pulse at adr 0
    rd<='0'; -- pulse
    if mCEn='0' then  -- always autoinc at end of access.
      mQr<=mQ; 
      mA<=std_logic_vector(unsigned(mA) + 1);
    end if;
    if lut_run='0' then -- hold in reset
      if ((cmd="001") and (wstb='1')) then -- handle table write from spi
        mA<=(others=>'0'); -- separate or part of cycle
      elsif (mCEn='0') then 
        mA<=std_logic_vector(unsigned(mA) + 1); 
      end if;
      dib_ctr<=(others=>'0');
      phai<='0';
      pos_ctri<=(others=>'0');
    else -- lut_run='1' 
      if csregen='1' then -- timing: pha follows cos,sin output
        phai<=not(phai); 
      end if; 
      if rd='1' then 
        mQr<=mQ; 
        mA<=std_logic_vector(unsigned(mA) + 1);
      end if;
      if csgen='0' then
        mA<=(others=>'0'); -- separate or part of cycle
        synci<='1';
        rd<='1';
      else -- csgen='1';
        dib_ctr<=dib_ctr+1;
        if ((pos_ctri=ctr_lim) and (dib_ctr=unsigned(dib_lim))) then
          dib_ctr<="00";
          pos_ctri<=(others=>'0');
          mA<=(others=>'0');
          rd<='1';
          synci<='1';
        elsif dib_ctr="11" then
          dib_ctr<="00";
          rd<='1';
        end if;
      end if;
    end if;
  elsif clk'event and clk='0' then -- to get cos,sin on negedge
    if lut_run='0' then  
      csregen<='0';
    elsif csregen='0' then
      if rd='1' then csregen<='1'; end if; -- set it once
    else  -- normal running
      cosi<=mxo(0);
      sini<=mxo(1);
      indoi<=mxo(2);
    end if;
  end if;
end process;

process (nreset,clk) -- handle setting things per cmd
begin
  if nreset='0' then
    ctr_lim<=(others=>'0');
    lut_ctl<=(others=>'0');
  elsif clk'event and clk='1' then
    if wstb='1' then
      case cmd is
        when "010"=> -- load ctr_lim
          ctr_lim<=unsigned(lut_ctl(0) & din); -- so load lut ctl first
        when "011"=> -- load cmd
          lut_ctl<=din;
          dib_lim<=unsigned(din(2 downto 1));
          ctr_lim(8)<=din(0);
        when others=>
      end case;
    end if;
  end if;
end process;

-- this is based on 512 bytes so we do not need 2 upper bits
mA11<=("00" & mA); 

-- based on ram2k8.vhp in vhdl_ip.
-- need to move this RAM out so we just have ports.
-- target -Dxfab 512x8 for asic
--        -Dghdl model for simulation
--        -Dxilinx model for fpga

r1:ram2k8 port map(
mCLK,mWEn,mCEn,
mA11, mD, mQ);


end rtl;
