library ieee;
use ieee.std_logic_1164.all;

use work.Axi4LiteInterfacePkg.all;

entity output_test is
    port(
        clk_in   : in   std_logic;
        areset_n : in   std_logic;
        m_axi4   : view Axi4LiteMasterView;
        s_axi4   : view Axi4LiteSlaveView
    );
end entity output_test;

architecture test of output_test is
begin

    regs : entity work.top
        port map(
            aclk             => clk_in,
            areset_n         => areset_n,
            awvalid          => s_axi4.WriteAddress.Valid,
            awready          => s_axi4.WriteAddress.Ready,
            awaddr           => s_axi4.WriteAddress.Addr(11 downto 2),
            awprot           => s_axi4.WriteAddress.Prot,
            wvalid           => s_axi4.WriteData.Valid,
            wready           => s_axi4.WriteData.Ready,
            wdata            => s_axi4.WriteData.Data,
            wstrb            => s_axi4.WriteData.Strb,
            bvalid           => s_axi4.WriteResponse.Valid,
            bready           => s_axi4.WriteResponse.Ready,
            bresp            => s_axi4.WriteResponse.Resp,
            arvalid          => s_axi4.ReadAddress.Valid,
            arready          => s_axi4.ReadAddress.Ready,
            araddr           => s_axi4.ReadAddress.Addr(11 downto 2),
            arprot           => s_axi4.ReadAddress.Prot,
            rvalid           => s_axi4.ReadData.Valid,
            rready           => s_axi4.ReadData.Ready,
            rdata            => s_axi4.ReadData.Data,
            rresp            => s_axi4.ReadData.Resp,
            Bottom_awvalid_o => m_axi4.WriteAddress.Valid,
            Bottom_awready_i => m_axi4.WriteAddress.Ready,
            Bottom_awaddr_o  => m_axi4.WriteAddress.Addr(11 downto 2),
            Bottom_awprot_o  => m_axi4.WriteAddress.Prot,
            Bottom_wvalid_o  => m_axi4.WriteData.Valid,
            Bottom_wready_i  => m_axi4.WriteData.Ready,
            Bottom_wdata_o   => m_axi4.WriteData.Data,
            Bottom_wstrb_o   => m_axi4.WriteData.Strb,
            Bottom_bvalid_i  => m_axi4.WriteResponse.Valid,
            Bottom_bready_o  => m_axi4.WriteResponse.Ready,
            Bottom_bresp_i   => m_axi4.WriteResponse.Resp,
            Bottom_arvalid_o => m_axi4.ReadAddress.Valid,
            Bottom_arready_i => m_axi4.ReadAddress.Ready,
            Bottom_araddr_o  => m_axi4.ReadAddress.Addr(11 downto 2),
            Bottom_arprot_o  => m_axi4.ReadAddress.Prot,
            Bottom_rvalid_i  => m_axi4.ReadData.Valid,
            Bottom_rready_o  => m_axi4.ReadData.Ready,
            Bottom_rdata_i   => m_axi4.ReadData.Data,
            Bottom_rresp_i   => m_axi4.ReadData.Resp
        );
end architecture test;
