library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reprod is
  generic(problem : integer range 0 to 3 := 1);

  port(rdb64   : in  std_logic_vector(63 downto 0);
       mask    : in  std_logic_vector(1 to 3);
       vaddr   : in  std_logic_vector(41 downto 12);
       result  : out std_logic_vector(63 downto 0)
      );
end entity reprod;

architecture rtl of reprod is
  subtype word64 is std_logic_vector(63 downto 0);

  function u2i(data : std_logic_vector) return integer is
  begin
    assert data'length <= 31 report "Data too large for integer" severity failure;
    return to_integer(unsigned(data));
  end;

  type va_bits is array (integer range <>) of integer;
  constant SZ_SPARC : va_bits(1 to 3) := (8, 6, 6);    -- 8 + 6 + 6     + 12 = 32 bits
  constant SZ_SV32  : va_bits(1 to 2) := (10, 10);     -- 10 + 10       + 12 = 32
  constant SZ_SV39  : va_bits(1 to 3) := (9, 9, 9);    -- 9 + 9 + 9     + 12 = 39
  constant SZ_SV48  : va_bits(1 to 4) := (9, 9, 9, 9); -- 9 + 9 + 9 + 9 + 12 = 48

  subtype va_type is integer range 0 to 3;
  constant sparc : integer := 0;
  constant sv32  : integer := 1;
  constant sv39  : integer := 2;
  constant sv48  : integer := 3;

  function va_size(what : va_type) return integer is
  begin
    case what is
    when sv32   => return SZ_SV32'length;
    when sv39   => return SZ_SV39'length;
    when sv48   => return SZ_SV48'length;
    when others => return SZ_SPARC'length;
    end case;
  end;

  function pt_code(mask_in : std_logic_vector) return std_logic_vector is
    -- Ensure we have the expected bit range.
    variable mask : std_logic_vector(1 to mask_in'length) := mask_in;
  begin
    if    mask(1) = '0' then
      return "00";
    elsif mask(2) = '0' then
      return "01";
    elsif mask'length > 2 and mask(3) = '0' then
      return "10";
    else
      return "11";
    end if;
  end;

  -- Returns address of page table entry.
  -- Returned address is 64 bit long and must be cut down to size by caller.
  function pt_addr(data  : std_logic_vector; mask : std_logic_vector;
                   vaddr : std_logic_vector; code : std_logic_vector) return word64 is
    -- Non-constant
    -- Using hypervisor guest address size here.
    variable addr   : word64 := (others => '0');
    variable pos    : integer;
  begin
    -- Every page table is the size of one page (thus downto 12).
    -- 12 due to smallest page size, 10 are the information bits.
    if addr'length >= data'length + (12 - 10) then
      addr(data'high + (12 - 10) downto 12) := data(data'high downto 10);
    else
      addr(addr'high downto 12) := data(addr'high - (12 - 10) downto 10);
    end if;
    pos := 12;
    for i in mask'length downto 1 loop
      if i > u2i(code) then
        pos := pos + va_size(i);
      end if;
    end loop;

    assert pos <= 48 report "Unsupported sv mode" severity failure;

    -- Reference seems to work as intended
    if problem = 0 then
      if    pos = 12 + 9 then -- 21
        addr(11 downto 11 - 9 + 1) := vaddr(21 - 1 downto 21 - 9);
      elsif pos = 12 + 9 * 2 then -- 30
        addr(11 downto 11 - 9 + 1) := vaddr(30 - 1 downto 30 - 9);
      elsif pos = 12 + 9 * 3 then -- 39
        addr(11 downto 11 - 9 + 1) := vaddr(39 - 1 downto 39 - 9);
      end if;
    end if;

    -- assign n117 = n87 == 32'b00000000000000000000000000100111; // 39 : 6-bits
    -- assign n122 = n87[4:0];             // Error: trunc should not truncate
    -- assign n124 = n122 + 5'b01011;      // ERROR: Should not add
    -- assign n126 = vaddr[n124 + 0 -: 9]; // ERROR: Incorrect due to n124
    if problem = 1 then
      if    pos = 12 + 9 then -- 21
        addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9); -- ghdl uses 5 bits for the pos
      elsif pos = 12 + 9 + 9 then -- 30
        addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9); -- ghdl uses 5 bits for the pos
      elsif pos = 12 + 9 + 9 + 9 then -- 39
        addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9); -- ghdl uses 5 bits for the pos
      end if;
    end if;

    -- Should not compile since pos can go out of bounds
    if problem = 2 then
      if    pos = 12 then
        -- NOTE: Just for reference of working behavior
        -- addr(11 downto 11 - 9 + 1) := vaddr(12 - 1 downto 12 - 9); -- Correctly throws error
        addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9);  -- Should throw error
      elsif pos = 12 + 9 then -- 21
        addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9);
      elsif pos = 12 + 9 + 9 then -- 30
        addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9);
      end if;
    end if;

    -- Pos should be within bounds of vaddr but the select signal in the bit select statement uses too few bits.
    if problem = 3 then
      addr(11 downto 11 - 9 + 1) := vaddr(pos - 1 downto pos - 9); -- ghdl uses 5 bits for the pos
    end if;
    -- assign n98 = n87[4:0];              // ERROR: Why truncate? Pos can legally be 39 which doesn't fit in 5 bits
    -- assign n100 = n98 + 5'b01011;       // ERROR: Why add 11?
    -- assign n102 = vaddr[n100 + 0 -: 9]; //(dyn_extract)

    assert addr(2 downto 0) = "000" report " Should be zero" severity failure;
    return addr;
  end;

begin

  process (rdb64, mask, vaddr)
  begin
    result <= pt_addr(rdb64, mask, vaddr, pt_code(mask));
  end process;

end architecture;

