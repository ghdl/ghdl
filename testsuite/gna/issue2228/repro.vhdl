library ieee;
use ieee.std_logic_1164.all;

entity cpu_disas is
generic (
  disasg : in integer range 0 to 3 := 1
  );
port (
  pc            : in  std_logic_vector -- ;                 -- PC
  );
end entity;

architecture behav of cpu_disas is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity test is
    generic (
       physaddr     : integer range 32 to 56 := 36; -- Physical Addressing GUESS
        single_issue : integer range 0  to 1 := 1; -- CHANGED set here
        disas        : integer := 1 -- follows entity
    );

end entity;

architecture foo of test is
    function maximum(x : integer; y: integer) return integer is
    begin
      if x > y then
        return x;
      else
        return y;
      end if;
    end;

    function minimum(x : integer; y: integer) return integer is
    begin
      if x < y then
        return x;
      else
        return y;
      end if;
    end;
    -- use gaisler.noelv.XLEN;
    constant XLEN:  integer := 32;  -- tempted to try 42
    constant va:        std_logic_vector := x"abc";
    -- constant va         : std_logic_vector := gaisler.mmucacheconfig.va(riscv_mmu);
    constant pa:        std_logic_vector := x"dead";
    -- constant pa         : std_logic_vector := gaisler.mmucacheconfig.pa(riscv_mmu);

    function addr_bits return integer is
    begin
      return minimum(XLEN, 1 + maximum(va'length, minimum(physaddr, pa'length)));
    end;
    subtype addr_type is std_logic_vector(addr_bits - 1 downto 0);
    subtype pctype         is addr_type;
    constant lanes       : std_logic_vector(0 to 1 - single_issue) := (others => '0');  -- Used as range.
    type pc_lanes_type     is array (lanes'range) of pctype;
    subtype wordx  is std_logic_vector(XLEN - 1 downto 0);
    -- Return high bit from data.
    function get_hi(data : std_logic_vector) return std_logic is
    begin
      return data(data'high);
    end;
    function pc2xlen(pc : pctype) return wordx is
      -- Non-constant
      variable data : wordx;
    begin
      data           := (others => get_hi(pc));
      data(pc'range) := pc;

      return data;
    end;
    
    signal pc             : pc_lanes_type; -- initialize?
begin
    iw_gen: for i in lanes'range generate
      iw : entity work.cpu_disas  -- grlib.cpu_disas
        generic map (
          disasg => disas
          )
        port map(
          pc          => pc2xlen(pc(i)) -- ,
          );
    end generate;
  -- end generate;
end architecture;
