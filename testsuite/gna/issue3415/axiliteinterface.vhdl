library ieee;
use ieee.std_logic_1164.all;

package Axi4LiteInterfacePkg is
    subtype Axi4RespType is std_logic_vector(1 downto 0);
    subtype Axi4ProtType is std_logic_vector(2 downto 0);

    type Axi4LiteWriteAddressRecType is record
        Valid : std_logic;
        Ready : std_logic;
        Addr  : std_logic_vector;
        Prot  : Axi4ProtType;
    end record;

    view  Axi4LiteWriteAddressMasterView of Axi4LiteWriteAddressRecType is
        Valid : out;
        Ready : in;
        Addr  : out;
        Prot  : out;
    end view;
    alias Axi4LiteWriteAddressSlaveView is Axi4LiteWriteAddressMasterView'converse;

    type Axi4LiteWriteDataRecType is record
        Valid : std_logic;
        Ready : std_logic;
        Data  : std_logic_vector;
        Strb  : std_logic_vector;
    end record;

    view  Axi4LiteWriteDataMasterView of Axi4LiteWriteDataRecType is
        Valid : out;
        Ready : in;
        Data  : out;
        Strb  : out;
    end view;
    alias Axi4LiteWriteDataSlaveView is Axi4LiteWriteDataMasterView'converse;

    type Axi4LiteWriteResponseRecType is record
        Valid : std_logic;
        Ready : std_logic;
        Resp  : Axi4RespType;
    end record;

    view  Axi4LiteWriteResponseMasterView of Axi4LiteWriteResponseRecType is
        Valid : in;
        Ready : out;
        Resp  : in;
    end view;
    alias Axi4LiteWriteResponseSlaveView is Axi4LiteWriteResponseMasterView'converse;

    type Axi4LiteReadAddressRecType is record
        Valid : std_logic;
        Ready : std_logic;
        Addr  : std_logic_vector;
        Prot  : Axi4ProtType;
    end record;

    view  Axi4LiteReadAddressMasterView of Axi4LiteReadAddressRecType is
        Valid : out;
        Ready : in;
        Addr  : out;
        Prot  : out;
    end view;
    alias Axi4LiteReadAddressSlaveView is Axi4LiteReadAddressMasterView'converse;

    type Axi4LiteReadDataRecType is record
        Valid : std_logic;
        Ready : std_logic;
        Data  : std_logic_vector;
        Resp  : Axi4RespType;
    end record;

    view  Axi4LiteReadDataMasterView of Axi4LiteReadDataRecType is
        Valid : in;
        Ready : out;
        Data  : in;
        Resp  : in;
    end view;
    alias Axi4LiteReadDataSlaveView is Axi4LiteReadDataMasterView'converse;

    type Axi4LiteRecType is record
        WriteAddress  : Axi4LiteWriteAddressRecType;
        WriteData     : Axi4LiteWriteDataRecType;
        WriteResponse : Axi4LiteWriteResponseRecType;
        ReadAddress   : Axi4LiteReadAddressRecType;
        ReadData      : Axi4LiteReadDataRecType;
    end record;

    view  Axi4LiteMasterView of Axi4LiteRecType is
        WriteAddress  : view Axi4LiteWriteAddressMasterView;
        WriteData     : view Axi4LiteWriteDataMasterView;
        WriteResponse : view Axi4LiteWriteResponseMasterView;
        ReadAddress   : view Axi4LiteReadAddressMasterView;
        ReadData      : view Axi4LiteReadDataMasterView;
    end view;
    alias Axi4LiteSlaveView is Axi4LiteMasterView'converse;

end package Axi4LiteInterfacePkg;
