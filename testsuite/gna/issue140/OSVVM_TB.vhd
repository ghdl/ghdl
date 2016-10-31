entity OSVVM_TB is
end OSVVM_TB;

architecture behavioral of OSVVM_TB is

type CovPType is protected
procedure AddBins(n : integer);
end protected;

type CovPType is protected body

procedure AddBins(n : integer) is
begin
   null;
end AddBins;

end protected body;

    shared variable Timeout_aborts_transfer : CovPType;

begin

    --Master
MASTER_STIMULUS: process
begin
    Timeout_aborts_transfer.AddBins(0);      -- comment this line out and the following piece of garbage is correctly diagnosed
    Timeout_aborts_transfer : CovPType;      -- syntax error here! 
end process;

end behavioral;

