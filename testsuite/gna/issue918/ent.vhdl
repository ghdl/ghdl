package power is
  type voltage is range integer'low to integer'high units
    uV;
    mV = 1000 uV;
     V = 1000 mV;
    kV = 1000  V;
  end units;
end package;

use work.power.all;

entity LTC is
  generic (
    V_MIN : voltage := 0 V;
    V_MAX : voltage := voltage'high
  );
  port (
    Vin  : in  voltage range 0 V to 15 V;
    Vout : out voltage range V_MIN to V_MAX
  );
end entity;

architecture ic of LTC is
begin
  Vout <= Vin * 0.95;
end architecture;

use work.power.all;

entity board is
  port (
    Vin  : in  voltage;
    Vout : out voltage
  );
end entity;

architecture ic of board is
begin
  U1: entity work.LTC
    port map (
      Vin  => 2.5 V,
      Vout => open
    );
end architecture;
