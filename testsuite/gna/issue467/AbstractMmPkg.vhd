-------------------------------------------------------------------------------
-- Title      : Abstract Memory-Mapped Interface
-- Project    : 
-------------------------------------------------------------------------------
-- File       : AbstractMmPkg.vhd
-- Author     : Rob Gaddi  <rgaddi@highlandtechnology.com>
-- Company    : Highland Technology, Inc.
-- Created    : 20-Nov-2017
-- Last update: 2017-11-25
-- Platform   : Simulation
-- Standard   : VHDL-2008
-------------------------------------------------------------------------------
-- Description: Support package for abstract memory-mapped interface BFMs.
-------------------------------------------------------------------------------
-- Revision History:
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library osvvm;
use osvvm.AlertLogPkg.all;
use osvvm.TbUtilPkg.all;
use osvvm.ResolutionPkg.all;

package AbstractMmPkg is
	
	-----------------------------------------------------------------------
	--	Constants and Types
	-----------------------------------------------------------------------
	
	type AlertLogIDArrayType is array(integer range <>) of AlertLogIDType;
	function alert_resolver(ta: AlertLogIDArrayType) return AlertLogIDType;
	subtype ResolvedAlert is alert_resolver AlertLogIDType;
	
	-- Transaction types
	type TransactionType_unresolved is (
		NONE,
		SINGLE,
		
		LINEAR_BURST,
		CONSTANT_BURST,
		CYCLE_BURST,
		
		BURST_DATA,
		
		PARAM
	);
	type TransactionArrayType is array(integer range <>) of TransactionType_unresolved;
	function resolved(ta: TransactionArrayType) return TransactionType_unresolved;
	subtype TransactionType is resolved TransactionType_unresolved;
	
	type AbstractMmRecType is record
		writedata : std_logic_vector;
		readdata : std_logic_vector;
		address : unsigned;
		byteen : std_logic_vector;
		write : std_logic;
		burstlen : integer_max;
		trans : TransactionType;
		
		addressiswords : std_logic;
		alert : ResolvedAlert;
		
		rdy : std_logic;
		ack : std_logic;
	end record AbstractMmRecType;
	
	constant AMR_READ: std_logic := '0';
	constant AMR_WRITE: std_logic := '1';
	
	constant AMR_ADDRESS_BYTES : std_logic := '0';
	constant AMR_ADDRESS_WORDS : std_logic := '1';
	
	constant ALRT : AlertLogIDType := GetAlertLogID("AbstractMmPkg");
	
	-----------------------------------------------------------------------
	--	Driver Functions
	-----------------------------------------------------------------------
	
	--	AmrRead (single read)
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in unsigned;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in std_logic_vector;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in natural;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in unsigned;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in natural;
		signal rec: inout AbstractMmRecType
	);
	
	--	AmrWrite (single write)
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in unsigned;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrWrite(
		data: in std_logic_vector;
		addr: in std_logic_vector;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in natural;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in unsigned;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in natural;
		signal rec: inout AbstractMmRecType
	);

	--	AmrAssert (single assert)
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in unsigned;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrAssert(
		data: in std_logic_vector;
		addr: in std_logic_vector;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in natural;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in unsigned;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	);
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in natural;
		signal rec: inout AbstractMmRecType
	);
	
	-----------------------------------------------------------------------
	--	Model Support Functions
	-----------------------------------------------------------------------
	
	procedure InterpretByteEnable(
		rec    : in AbstractMmRecType;
		width  : out natural;
		align  : out natural
	);
	
	function GetByteAddress(rec: in AbstractMmRecType; unalign: boolean := false) return unsigned;
	function GetWordAddress(rec: in AbstractMmRecType) return unsigned;
	
	-----------------------------------------------------------------------
	--	Utility Functions
	-----------------------------------------------------------------------
	
	--	Initialization
	procedure InitializeAmr(signal rec: out AbstractMmRecType);
--	function INIT_AMR(datalen, addrlen : positive) return AbstractMmRecType;
--	function INIT_AMR(datalen, addrlen, belen : positive) return AbstractMmRecType;
	
	--	Selecting word/byte addressing
	procedure SetAddressWords(signal rec: inout AbstractMmRecType);
	procedure SetAddressBytes(signal rec: inout AbstractMmRecType);
	
	--	Overriding the default alert
	procedure OverrideAlert(signal rec: inout AbstractMmRecType; alert: AlertLogIDType);
	
end package AbstractMmPkg;

package body AbstractMmPkg is
	
	procedure InitializeAmr(signal rec: out AbstractMmRecType) is
		variable local : AbstractMmRecType(
			writedata(rec.writedata'range),
			readdata(rec.readdata'range),
			address(rec.address'range),
			byteen(rec.byteen'range)
		);
		constant WD : std_logic_vector(rec.writedata'range) := (others => 'Z');
		constant RD : std_logic_vector(rec.readdata'range) := (others => 'Z');
		constant AD : unsigned(rec.address'range) := (others => 'Z');
		constant BE : std_logic_vector(rec.byteen'range) := (others => 'Z');
	begin
		local := (
			writedata => WD,
			readdata => RD,
			address => AD,
			byteen => BE,
			write => 'Z',
			burstlen => integer'left,
			trans => NONE,
			addressiswords => 'Z',
			alert => ALRT,
			rdy => 'Z',
			ack => 'Z'
		);
		rec <= local;
	end procedure InitializeAmr;

	--function INIT_AMR(
	--	datalen, addrlen : positive
	--) return AbstractMmRecType is
	--	constant belen : positive := datalen / 8;
	--begin
	--	return INIT_AMR(datalen, addrlen, belen);
	--end function INIT_AMR;
	
	--function INIT_AMR(
	--	datalen, addrlen, belen: positive
	--) return AbstractMmRecType is
	--begin
	--	return (
	--		writedata => (datalen downto 1 => 'Z'),
	--		readdata => (datalen downto 1 => 'Z'),
	--		address => (addrlen downto 1 => 'Z'),
	--		byteen => (belen downto 1 => 'Z'),
	--		write => 'Z',
	--		burstlen => integer'left,
	--		trans => NONE,
			
	--		addressiswords => 'Z',
	--		alert => ALRT,
			
	--		rdy => 'Z',
	--		ack => 'Z'
	--	);
	--end function INIT_AMR;
	
	procedure SetAddressWords(signal rec: inout AbstractMmRecType) is
	begin
		rec.addressiswords <= AMR_ADDRESS_WORDS;
	end procedure SetAddressWords;
	
	procedure SetAddressBytes(signal rec: inout AbstractMmRecType) is
	begin
		rec.addressiswords <= AMR_ADDRESS_BYTES;
	end procedure SetAddressBytes;
	
	-----------------------------------------------------------------------
	--	AmrRead
	-----------------------------------------------------------------------
	
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in unsigned;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
		constant WD : std_logic_vector(rec.writedata'range) := (others => 'X');
	begin
		rec.writedata <= WD;
		rec.address <= RESIZE(addr, rec.address'length);
		rec.byteen <= byteen;
		rec.write <= AMR_READ;
		rec.burstlen <= 1;
		rec.trans <= SINGLE;
		RequestTransaction(rec.rdy, rec.ack);
		data := rec.readdata;
	end procedure AmrRead;
	
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in std_logic_vector;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
	begin
		AmrRead(data, UNSIGNED(addr), byteen, rec);
	end procedure AmrRead;
	
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in natural;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
	begin
		AmrRead(data, TO_UNSIGNED(addr, rec.address'length), byteen, rec);
	end procedure AmrRead;
	
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in unsigned;
		signal rec: inout AbstractMmRecType
	) is
		variable byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrRead(data, addr, byteen, rec);
	end procedure AmrRead;
	
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
		variable byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrRead(data, UNSIGNED(addr), byteen, rec);
	end procedure AmrRead;
	
	procedure AmrRead(
		data: out std_logic_vector;
		addr: in natural;
		signal rec: inout AbstractMmRecType
	) is
		variable byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrRead(data, TO_UNSIGNED(addr, rec.address'length), byteen, rec);
	end procedure AmrRead;
	
	-----------------------------------------------------------------------
	--	AmrWrite (single write)
	-----------------------------------------------------------------------
	
	procedure AmrWrite(
		data: in std_logic_vector;
		addr: in unsigned;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
	begin
		rec.writedata <= data;
		rec.address <= RESIZE(addr, rec.address'length);
		rec.byteen <= byteen;
		rec.write <= AMR_WRITE;
		rec.burstlen <= 1;
		rec.trans <= SINGLE;
		RequestTransaction(rec.rdy, rec.ack);
	end procedure AmrWrite;
	
	procedure AmrWrite(
		data: in std_logic_vector;
		addr: in std_logic_vector;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is begin
		AmrWrite(data, UNSIGNED(addr), byteen, rec);
	end procedure AmrWrite;
	
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in natural;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is begin
		AmrWrite(data, TO_UNSIGNED(addr, rec.address'length), byteen, rec);
	end procedure AmrWrite;
	
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in unsigned;
		signal rec: inout AbstractMmRecType
	) is 
		constant byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrWrite(data, addr, byteen, rec);
	end procedure AmrWrite;
	
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
		constant byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrWrite(data, UNSIGNED(addr), byteen, rec);
	end procedure AmrWrite;
	
	procedure AmrWrite(
		data: in  std_logic_vector;
		addr: in natural;
		signal rec: inout AbstractMmRecType
	) is 
		constant byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrWrite(data, TO_UNSIGNED(addr, rec.address'length), byteen, rec);
	end procedure AmrWrite;

		-----------------------------------------------------------------------
	--	AmrAssert (single assert)
	-----------------------------------------------------------------------
	
	procedure AmrAssert(
		data: in std_logic_vector;
		addr: in unsigned;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
		variable readdata : std_logic_vector(data'range);
	begin
		AmrRead(readdata, addr, byteen, rec);
		--AffirmIfEqual(rec.alert, readdata, data, "Assert @ 0x" & TO_HSTRING(addr));
	end procedure AmrAssert;
	
	procedure AmrAssert(
		data: in std_logic_vector;
		addr: in std_logic_vector;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is begin
		AmrAssert(data, UNSIGNED(addr), byteen, rec);
	end procedure AmrAssert;
	
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in natural;
		byteen: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is begin
		AmrAssert(data, TO_UNSIGNED(addr, rec.address'length), byteen, rec);
	end procedure AmrAssert;
	
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in unsigned;
		signal rec: inout AbstractMmRecType
	) is 
		constant byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrAssert(data, addr, byteen, rec);
	end procedure AmrAssert;
	
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in std_logic_vector;
		signal rec: inout AbstractMmRecType
	) is
		constant byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrAssert(data, UNSIGNED(addr), byteen, rec);
	end procedure AmrAssert;
	
	procedure AmrAssert(
		data: in  std_logic_vector;
		addr: in natural;
		signal rec: inout AbstractMmRecType
	) is 
		constant byteen : std_logic_vector(rec.byteen'range) := (others => '1');
	begin
		AmrAssert(data, TO_UNSIGNED(addr, rec.address'length), byteen, rec);
	end procedure AmrAssert;

	-----------------------------------------------------------------------
	--	Utility Functions
	-----------------------------------------------------------------------

	-- Turn a number into the number of bits needed to represent it.
	function clog2(x : positive) return natural is
		variable y : natural := 1;
	begin
		for log in 0 to 255 loop
			if y >= x then
				return log;
			end if;
			y := y * 2;
		end loop;
		return natural'right;
	end function clog2;
	
	-- Allow only 1 entry to be other than NONE.
	function resolved(ta: TransactionArrayType) return TransactionType_unresolved is
		variable r : TransactionType_unresolved := NONE;
		variable t : TransactionType_unresolved;
	begin
		for idx in ta'range loop
			t := ta(idx);
			if t /= NONE then
				assert r = NONE
					report "Multiple non-NONE transaction types."
					severity failure;
				r := t;
			end if;
		end loop;
		return r;
	end function resolved;

	-- Allow up to 1 entry to be other than our local ALRT, in which
	-- case it wins.
	function alert_resolver(ta: AlertLogIDArrayType) return AlertLogIDType is
		variable r : AlertLogIDType := ALRT;
		variable t : AlertLogIDType;
	begin
		for idx in ta'range loop
			t := ta(idx);
			if (t /= ALRT) and (t >= ALERTLOG_BASE_ID) then
				assert r = ALRT
					report "Multiple alerts provided."
					severity failure;
				r := t;
			end if;
		end loop;
		return r;
	end function alert_resolver;
	
	procedure InterpretByteEnable(
		rec    : in AbstractMmRecType;
		width  : out natural;
		align  : out natural
	) is
		alias byteen : std_logic_vector(rec.byteen'range) is rec.byteen;
		alias LA : AlertLogIDType is rec.alert;
		variable first, last: integer;
		variable found : boolean := false;
	begin
		if (and byteen) = '1' then
			-- Try to provide fast resolution for the most common case.
			width := byteen'length;
			align := 0;
		else
			-- Alright, do it the hard way.  Scan for contiguous enables.
			for i in byteen'low to byteen'high loop
				if byteen(i) = '1' then
					found := true;
					first := i;
					exit;
				end if;
			end loop;
			
			if not found then
				-- No byte enables are set
				Alert(LA, "No byte enables set.", WARNING);
				width := 0;
				align := 0;
			else
				last := first;
				for i in first+1 to byteen'high loop
					if byteen(i) = '1' then
						last := i;
					else
						exit;
					end if;
				end loop;
				
				if last /= byteen'high then
					for i in last+1 to byteen'high loop
						if byteen(i) = '1' then
							Alert(LA, "Non-contiguous byte enables " & TO_STRING(byteen), WARNING);
							exit;
						end if;
					end loop;
				end if;
				
				width := last-first+1;
				align := first;
			end if;
		end if;
	end procedure InterpretByteEnable;
	
	function GetByteAddress(rec: in AbstractMmRecType; unalign: boolean := false) return unsigned is
		variable padding : unsigned(clog2(rec.byteen'length)-1 downto 0);
		variable alignment : integer := integer'left;
	begin
		case rec.addressiswords is
			when AMR_ADDRESS_BYTES =>
				return rec.address;
				
			when AMR_ADDRESS_WORDS =>
				if unalign then
					for i in rec.byteen'low to rec.byteen'high loop
						if rec.byteen(i) = '1' then
							alignment := i;
							exit;
						end if;
					end loop;
					if alignment /= integer'left then
						report "All bytes disabled." severity warning;
						alignment := 0;
					end if;
					padding := TO_UNSIGNED(alignment, padding'length);
				else
					padding := (others => '0');
				end if;
				return rec.address & PADDING;
				
			when others =>
				report "Byte/word addressing not defined." severity failure;
				return (rec.address'range => 'X');
		end case;
	end function GetByteAddress;
	
	function GetWordAddress(rec: in AbstractMmRecType) return unsigned is
		variable padding : unsigned(clog2(rec.byteen'length)-1 downto 0);
		variable alignment, width : integer;
	begin
		case rec.addressiswords is
			when AMR_ADDRESS_BYTES =>
				return rec.address(rec.address'high downto padding'length);
				
			when AMR_ADDRESS_WORDS =>
				return rec.address;
				
			when others =>
				report "Byte/word addressing not defined." severity failure;
				return (rec.address'range => 'X');
		end case;
	end function GetWordAddress;
	
	procedure OverrideAlert(signal rec: inout AbstractMmRecType; alert: AlertLogIDType) is
	begin
		rec.alert <= alert;
	end procedure OverrideAlert;
	
end package body AbstractMmPkg;
