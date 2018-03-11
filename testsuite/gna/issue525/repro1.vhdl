entity repro is
  port
    (
      clk       : in  bit;
      rst       : in  bit;

      data_out  : out bit_vector(7 downto 0)
    );
end ;


architecture arch1 of repro is

  --
  -- NOTE: placing port attributes here in the architecture body is illegal in VHDL,
  --       but unfortunately Vivado requires them here to parse the X_INTERFACE attributes
  --
  attribute X_INTERFACE_INFO      : string;
  attribute X_INTERFACE_PARAMETER : string;

  attribute X_INTERFACE_INFO      of rst : signal is "xilinx.com:signal:reset:1.0 rst RST";
  attribute X_INTERFACE_PARAMETER of rst : signal is "POLARITY ACTIVE_HIGH";

begin


end arch1;

