library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity zion is
    Port (
        leds : out STD_LOGIC_VECTOR(7 downto 0);
        clk  : in  STD_LOGIC);
end zion;

architecture Behavioral of zion is
    -- Digital clock manager
    COMPONENT dcm1
    PORT(
        CLKIN_IN : IN std_logic;
        CLKFX_OUT : OUT std_logic;
        CLKIN_IBUFG_OUT : OUT std_logic;
        CLK0_OUT : OUT std_logic
        );
    END COMPONENT;

    -- BRAM component
    COMPONENT memory
    generic (width_bits, depth_words, addr_size : integer);
    port (
        clka    : in std_logic;
        ena     : in std_logic;
        wea     : in std_logic_vector(0 downto 0);
        addra   : in std_logic_vector(addr_size - 1 downto 0);
        dina    : in std_logic_vector(width_bits - 1 downto 0);
        douta   : out std_logic_vector(width_bits - 1 downto 0);

        clkb    : in std_logic;
        enb     : in std_logic;
        web     : in std_logic_vector(0 downto 0);
        addrb   : in std_logic_vector(addr_size - 1 downto 0);
        dinb    : in std_logic_vector(width_bits - 1 downto 0);
        doutb   : out std_logic_vector(width_bits - 1 downto 0));
    END COMPONENT;
    -- IRAM input/output signals
    signal iram_addra   : MemWordAddr;
    signal iram_douta   : Logic_Word;
    signal iram_enb     : std_logic;
    signal iram_web     : lvbit;
    signal iram_addrb   : MemWordAddr;
    signal iram_dinb    : Logic_Word;
    signal iram_doutb   : Logic_Word;
    -- DRAM input/output signals
    signal dram_ena     : std_logic;
    signal dram_addra   : DataByteAddr;
    signal dram_douta   : Logic_Byte;
    signal dram_wea     : lvbit;
    signal dram_dina    : Logic_Byte;
    signal dram_enb     : std_logic;
    signal dram_addrb   : DataByteAddr;
    signal dram_dinb    : Logic_Byte;
    signal dram_web     : lvbit;
    signal dram_doutb   : Logic_Byte;


    -- Registers component
    COMPONENT my_regs
    PORT(
        rd1 : IN std_logic_vector(3 downto 0);
        rd2 : IN std_logic_vector(3 downto 0);
        wr_idx : IN std_logic_vector(3 downto 0);
        wr_data : IN std_logic_vector(15 downto 0);
        we : IN std_logic;
        clk : IN std_logic;
        reg1 : OUT std_logic_vector(15 downto 0);
        reg2 : OUT std_logic_vector(15 downto 0)
        );
    END COMPONENT;
    signal reg_idx1     : Reg_Index;
    signal reg_idx2     : Reg_Index;
    signal reg_dout1    : Logic_Word;
    signal reg_dout2    : Logic_Word;
    signal wr_reg_en    : std_logic;
    signal wr_reg_idx   : Reg_Index;
    signal wr_reg_data  : Logic_Word;


    -- pipeline stages

    COMPONENT pl_stage1
    PORT(
        clk             : in std_logic;
        iram_addr       : out MemWordAddr;
        iram_dout       : in Logic_Word;
        reg_idx1        : out Reg_Index;
        reg_idx2        : out Reg_Index;
        reg_dout1       : in Logic_Word;
        reg_dout2       : in Logic_Word;
        st1out          : out Stage_1_2_Interface;
        branch_flag     : in std_logic;
        branch_dest     : in MemWordAddr;
        stall_flag      : in std_logic);
    END COMPONENT;

    COMPONENT pl_stage2
    PORT(
        clk             : in std_logic;
        st2in           : in Stage_1_2_Interface;
        reg1_fwd        : in FwdValue;
        reg2_fwd        : in FwdValue;
        branch_flag     : out std_logic;
        branch_dest     : out MemWordAddr;
        st2out          : in Stage_2_3_Interface);
    END COMPONENT;

    COMPONENT pl_stage3
    PORT(
        clk             : in std_logic;
        st3in           : in Stage_2_3_Interface;
        iram_en         : out std_logic;
        iram_we         : out lvbit;
        iram_addr       : out MemWordAddr;
        iram_din        : out Logic_Word;
        iram_dout       : in Logic_Word;
        dram_ena        : out std_logic;
        dram_wea        : out lvbit;
        dram_addra      : out DataByteAddr;
        dram_dina       : out Logic_Byte;
        dram_douta      : in Logic_Byte;
        dram_enb        : out std_logic;
        dram_web        : out lvbit;
        dram_addrb      : out DataByteAddr;
        dram_dinb       : out Logic_Byte;
        dram_doutb      : in Logic_Byte;
        wr_reg_en       : out std_logic;
        wr_reg_idx      : out Reg_Index;
        wr_reg_data     : out Logic_Word;
        io_reg_we       : out lvbit;
        io_reg_inp      : out Logic_Byte);
    END COMPONENT;

    COMPONENT hazardctl
    PORT(
        clk                 : in std_logic;
        st1_reg1_idx        : in Reg_Index;
        st1_reg2_idx        : in Reg_Index;
        st2_wr_type         : in Write_Type;
        st2_wr_reg_idx      : in Reg_Index;
        st3_alu_res         : in Logic_Word;
        st3_pc_plus_2       : in MemWordAddr;
        st2_reg1_fwd        : out FwdValue;
        st2_reg2_fwd        : out FwdValue;
        st1_stall_flag      : out std_logic);
    END COMPONENT;


    -- dcm output
    signal dcm_clk              : std_logic;

    -- explicit initialization to avoid an optimization warning
    signal st1out, st2in : Stage_1_2_Interface := Stage_1_2_Interface_zero;
    signal st2out, st3in : Stage_2_3_Interface;

    signal st1_stall_flag               : std_logic;
    signal st2_reg1_fwd, st2_reg2_fwd   : FwdValue;
    signal branch_flag                  : std_logic;
    signal branch_dest                  : MemWordAddr;


    -- I/O memory

    -- byte-sized I/O register. cur gets set to inp if we is "1".
    type IO_Register_Byte is
        record
            cur : Logic_Byte;
            inp : Logic_Byte;
            -- using a vector to make initialization easier
            we  : lvbit;
        end record;

    signal io_leds_reg : IO_Register_Byte
        := (others => (others => '0'));

begin

    inst_dcm1: dcm1 PORT MAP(
        CLKIN_IN        => clk,
        CLKFX_OUT       => dcm_clk,
        CLKIN_IBUFG_OUT => open,
        CLK0_OUT        => open
    );

    iram : memory
    GENERIC MAP( width_bits=>16, addr_size=>13, depth_words=>8192 )
    PORT MAP (
        clka    => dcm_clk,
        ena     => '1',
        addra   => iram_addra,
        douta   => iram_douta,
        wea     => "0",
        dina    => X"0000",

        clkb    => dcm_clk,
        enb     => iram_enb,
        addrb   => iram_addrb,
        doutb   => iram_doutb,
        web     => iram_web,
        dinb    => iram_dinb);

    dram : memory
    GENERIC MAP( width_bits=>8, addr_size=>11, depth_words=>2048 )
    PORT MAP (
        clka    => dcm_clk,
        ena     => dram_ena,
        addra   => dram_addra,
        douta   => dram_douta,
        wea     => dram_wea,
        dina    => dram_dina,

        clkb    => dcm_clk,
        enb     => dram_enb,
        addrb   => dram_addrb,
        doutb   => dram_doutb,
        web     => dram_web,
        dinb    => dram_dinb);

    inst_my_regs : my_regs PORT MAP (
        clk     => dcm_clk,
        rd1     => reg_idx1,
        rd2     => reg_idx2,
        reg1    => reg_dout1,
        reg2    => reg_dout2,
        we      => wr_reg_en,
        wr_idx  => wr_reg_idx,
        wr_data => wr_reg_data);


    inst_pl_stage1: pl_stage1 PORT MAP(
        clk         => dcm_clk,
        iram_addr   => iram_addra,
        iram_dout   => iram_douta,
        reg_idx1    => reg_idx1,
        reg_idx2    => reg_idx2,
        reg_dout1   => reg_dout1,
        reg_dout2   => reg_dout2,
        st1out      => st1out,
        branch_flag => branch_flag,
        branch_dest => branch_dest,
        stall_flag  => st1_stall_flag);

    inst_pl_stage2: pl_stage2 PORT MAP(
        clk         => dcm_clk,
        st2in       => st2in,
        reg1_fwd    => st2_reg1_fwd,
        reg2_fwd    => st2_reg2_fwd,
        branch_flag => branch_flag,
        branch_dest => branch_dest,
        st2out      => st2out);

    inst_pl_stage3: pl_stage3 PORT MAP(
        clk         => dcm_clk,
        st3in       => st3in,
        iram_en     => iram_enb,
        iram_we     => iram_web,
        iram_addr   => iram_addrb,
        iram_din    => iram_dinb,
        iram_dout   => iram_doutb,
        dram_ena    => dram_ena,
        dram_wea    => dram_wea,
        dram_addra  => dram_addra,
        dram_dina   => dram_dina,
        dram_douta  => dram_douta,
        dram_enb    => dram_enb,
        dram_web    => dram_web,
        dram_addrb  => dram_addrb,
        dram_dinb   => dram_dinb,
        dram_doutb  => dram_doutb,
        wr_reg_en   => wr_reg_en,
        wr_reg_idx  => wr_reg_idx,
        wr_reg_data => wr_reg_data,
        io_reg_we   => io_leds_reg.we,
        io_reg_inp  => io_leds_reg.inp);

    inst_hazardctl : hazardctl PORT MAP(
        clk             => dcm_clk,
        st1_reg1_idx    => reg_idx1,
        st1_reg2_idx    => reg_idx2,
        st2_wr_type     => st2in.wr_type,
        st2_wr_reg_idx  => st2in.wr_reg_idx,
        st3_alu_res     => st3in.alu_res,
        st3_pc_plus_2   => st3in.pc_plus_2,
        st2_reg1_fwd    => st2_reg1_fwd,
        st2_reg2_fwd    => st2_reg2_fwd,
        st1_stall_flag  => st1_stall_flag);


    sync_proc : process(dcm_clk)
    begin
        if rising_edge(dcm_clk) then
            st2in <= st1out;
            st3in <= st2out;
        end if;
    end process;


    -------------------------------------------------
    -- I/O
    -------------------------------------------------

    io_leds_proc : process(dcm_clk, io_leds_reg.inp, io_leds_reg.we)
    begin
        if rising_edge(dcm_clk) then
            if io_leds_reg.we = "1" then
                io_leds_reg.cur <= io_leds_reg.inp;
            end if;
        end if;
    end process;

    leds <= io_leds_reg.cur;

end Behavioral;
