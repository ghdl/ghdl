library osvvm;
use osvvm.AlertLogPkg.all;

entity repro is
end repro;

architecture behav of repro is
begin
  process
  begin
    SetAlertLogJustify(True);
    ReportAlerts;
    wait;
  end process;
end behav;
