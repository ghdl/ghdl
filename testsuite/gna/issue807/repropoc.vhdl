entity repropoc is
end ;

library	IEEE;
use			IEEE.std_logic_1164.all;

package config_private is
  -- TODO: 
  -- ===========================================================================
  subtype T_BOARD_STRING is STRING(1 to 16);
  subtype T_BOARD_CONFIG_STRING is STRING(1 to 64);
  subtype T_DEVICE_STRING is STRING(1 to 32);

  -- Data structures to describe UART / RS232
  type T_BOARD_UART_DESC is record
    IsDTE : BOOLEAN; -- Data terminal Equipment (e.g. PC, Printer)
    FlowControl : T_BOARD_CONFIG_STRING; -- (NONE, SW, HW_CTS_RTS, HW_RTR_RTS)
    BaudRate : T_BOARD_CONFIG_STRING;		-- e.g. "115.2 kBd"
    BaudRate_Max : T_BOARD_CONFIG_STRING;
  end record;
	
  -- Data structures to describe Ethernet
  type T_BOARD_ETHERNET_DESC is record
    IPStyle : T_BOARD_CONFIG_STRING;
    RS_DataInterface : T_BOARD_CONFIG_STRING;
    PHY_Device : T_BOARD_CONFIG_STRING;
    PHY_DeviceAddress : STD_LOGIC_VECTOR(7 downto 0);
    PHY_DataInterface : T_BOARD_CONFIG_STRING;
    PHY_ManagementInterface : T_BOARD_CONFIG_STRING;
  end record;

  subtype T_BOARD_ETHERNET_DESC_INDEX is NATURAL range 0 to 7;
  type T_BOARD_ETHERNET_DESC_VECTOR is array(NATURAL range <>) of T_BOARD_ETHERNET_DESC;

  -- Data structures to describe a board layout
  type T_BOARD_INFO is record
    BoardName : T_BOARD_CONFIG_STRING;
    FPGADevice : T_BOARD_CONFIG_STRING;
    UART : T_BOARD_UART_DESC;
    Ethernet : T_BOARD_ETHERNET_DESC_VECTOR(T_BOARD_ETHERNET_DESC_INDEX);
    EthernetCount : T_BOARD_ETHERNET_DESC_INDEX;
  end record;

  type T_BOARD_INFO_VECTOR is array (natural range <>) of T_BOARD_INFO;

  constant C_POC_NUL : CHARACTER;
  constant C_BOARD_STRING_EMPTY : T_BOARD_STRING;
  constant C_BOARD_CONFIG_STRING_EMPTY	: T_BOARD_CONFIG_STRING;
  constant C_DEVICE_STRING_EMPTY : T_DEVICE_STRING;
  CONSTANT C_BOARD_INFO_LIST : T_BOARD_INFO_VECTOR;
end package;


package body config_private is
  constant C_POC_NUL : CHARACTER := '~';
  constant C_BOARD_STRING_EMPTY : T_BOARD_STRING := (others => C_POC_NUL);
  constant C_BOARD_CONFIG_STRING_EMPTY	: T_BOARD_CONFIG_STRING		:= (others => C_POC_NUL);
  constant C_DEVICE_STRING_EMPTY : T_DEVICE_STRING := (others => C_POC_NUL);

  function conf(str : string) return T_BOARD_CONFIG_STRING is
    constant ConstNUL : STRING(1 to 1) := (others => C_POC_NUL);
    variable Result : STRING(1 to T_BOARD_CONFIG_STRING'length);
  begin
    Result := (others => C_POC_NUL);
    if (str'length > 0) then
      Result(1 to str'length) := str;
    end if;
    return Result;
  end function;
	
  constant C_BOARD_ETHERNET_DESC_EMPTY	: T_BOARD_ETHERNET_DESC		:= (
    IPStyle => C_BOARD_CONFIG_STRING_EMPTY,
    RS_DataInterface => C_BOARD_CONFIG_STRING_EMPTY,
    PHY_Device => C_BOARD_CONFIG_STRING_EMPTY,
    PHY_DeviceAddress => x"00",
    PHY_DataInterface => C_BOARD_CONFIG_STRING_EMPTY,
    PHY_ManagementInterface => C_BOARD_CONFIG_STRING_EMPTY
    );      
	
  -- predefined UART descriptions
  function brd_CreateUART(IsDTE : BOOLEAN; FlowControl : STRING; BaudRate : STRING; BaudRate_Max : STRING := "") return T_BOARD_UART_DESC is
    variable Result			: T_BOARD_UART_DESC;
  begin
    Result.IsDTE				:= IsDTE;
    Result.FlowControl	:= conf(FlowControl);
    Result.BaudRate			:= conf(BaudRate);
    Result.BaudRate_Max	:= conf(BaudRate);
    return Result;
  end function;
	
  constant C_BOARD_UART_EMPTY : T_BOARD_UART_DESC :=
    brd_CreateUART(TRUE, "NONE", "0 Bd");

  function brd_CreateEthernet(IPStyle : STRING; RS_DataInt : STRING;
                              PHY_Device : STRING;
                              PHY_DevAddress : STD_LOGIC_VECTOR(7 downto 0);
                              PHY_DataInt : STRING;
                              PHY_MgntInt : STRING)
    return T_BOARD_ETHERNET_DESC is
    variable Result		: T_BOARD_ETHERNET_DESC;
  begin
    Result.IPStyle := conf(IPStyle);
    Result.RS_DataInterface := conf(RS_DataInt);
    Result.PHY_Device := conf(PHY_Device);
    Result.PHY_DeviceAddress := PHY_DevAddress;
    Result.PHY_DataInterface := conf(PHY_DataInt);
    Result.PHY_ManagementInterface	:= conf(PHY_MgntInt);
    return Result;
  end function;

  constant C_BOARD_ETH_EMPTY : T_BOARD_ETHERNET_DESC :=
    brd_CreateEthernet("", "", "", x"00", "", "");
	
  constant C_BOARD_ETH_NONE : T_BOARD_ETHERNET_DESC_VECTOR(T_BOARD_ETHERNET_DESC_INDEX)	:= (others => C_BOARD_ETH_EMPTY);


  -- Board Descriptions
  -- =========================================================================
  CONSTANT C_BOARD_INFO_LIST : T_BOARD_INFO_VECTOR := (
    -- Custom Board (MUST BE LAST ONE)
    -- ======================================================================
    1 => (
      BoardName => conf("Custom"),
      FPGADevice => conf("Device is unknown for a custom board"),
      UART => C_BOARD_UART_EMPTY,
      Ethernet => C_BOARD_ETH_NONE,
      EthernetCount =>	0
      )
    );
end package body;


use work.config_private.all;
architecture behav of repropoc is
begin
end behav;
