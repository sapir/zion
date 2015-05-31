library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use defs.ALL;


entity cpu_core is
    Port (
        clk         : in std_logic;

        -- IRAM input/output
        iram_addra  : out MemWordAddr;
        iram_douta  : in  Instr_Type;
        iram_enb    : out std_logic;
        iram_web    : out lvbit;
        iram_addrb  : out MemWordAddr;
        iram_dinb   : out Instr_Type;
        iram_doutb  : in  Instr_Type;

        -- DRAM input/output
        dram_ena    : out std_logic;
        dram_addra  : out DataByteAddr;
        dram_douta  : in  Logic_Byte;
        dram_wea    : out lvbit;
        dram_dina   : out Logic_Byte;
        dram_enb    : out std_logic;
        dram_addrb  : out DataByteAddr;
        dram_dinb   : out Logic_Byte;
        dram_web    : out lvbit;
        dram_doutb  : in  Logic_Byte;

        -- IO
        io_leds_we  : out lvbit;
        io_leds_inp : out Logic_Byte);
end cpu_core;


architecture Behavioral of cpu_core is

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

    -- stage 0 output flip-flops also include the IRAM outputs,
    -- as IRAM is synchronous
    COMPONENT pl_stage0
    PORT(
        clk             : in std_logic;
        iram_addr       : out MemWordAddr;
        st0out          : out Stage_0_1_Interface;
        branch_flag     : in std_logic;
        branch_dest     : in MemWordAddr;
        st1_stall_flag  : in std_logic);
    END COMPONENT;

    COMPONENT pl_stage1
    PORT(
        clk             : in std_logic;
        iram_dout       : in Instr_Type;
        st1in           : in Stage_0_1_Interface;
        reg_idx1        : out Reg_Index;
        reg_idx2        : out Reg_Index;
        reg_dout1       : in Logic_Word;
        reg_dout2       : in Logic_Word;
        st1out          : out Stage_1_2_Interface;
        branch_flag     : in std_logic;
        st1_stall_flag  : in std_logic);
    END COMPONENT;

    COMPONENT pl_stage2
    PORT(
        clk             : in std_logic;
        st2in           : in Stage_1_2_Interface;
        reg1_fwd        : in FwdValue;
        reg2_fwd        : in FwdValue;
        branch_flag     : out std_logic;
        branch_dest     : out MemWordAddr;
        st2out          : out Stage_2_3_Interface;
        iram_en         : out std_logic;
        iram_we         : out lvbit;
        iram_addr       : out MemWordAddr;
        iram_din        : out Instr_Type;
        dram_ena        : out std_logic;
        dram_wea        : out lvbit;
        dram_addra      : out DataByteAddr;
        dram_dina       : out Logic_Byte;
        dram_enb        : out std_logic;
        dram_web        : out lvbit;
        dram_addrb      : out DataByteAddr;
        dram_dinb       : out Logic_Byte;
        io_reg_we       : out lvbit;
        io_reg_inp      : out Logic_Byte);
    END COMPONENT;

    COMPONENT pl_stage3
    PORT(
        clk             : in std_logic;
        st3in           : in Stage_2_3_Interface;
        iram_dout       : in Instr_Type;
        dram_douta      : in Logic_Byte;
        dram_doutb      : in Logic_Byte;
        wr_reg_en       : out std_logic;
        wr_reg_idx      : out Reg_Index;
        wr_reg_data     : out Logic_Word);
    END COMPONENT;

    COMPONENT hazardctl
    PORT(
        clk                 : in std_logic;
        st1_reg1_idx        : in Reg_Index;
        st1_reg2_idx        : in Reg_Index;
        st2_wr_reg_en       : in std_logic;
        st2_wr_reg_src      : in RegWriteSrc_Type;
        st2_wr_reg_idx      : in Reg_Index;
        st2_invalid_flag    : in std_logic;
        st3_alu_res         : in Logic_Word;
        st2_reg1_fwd        : out FwdValue;
        st2_reg2_fwd        : out FwdValue;
        st1_stall_flag      : out std_logic);
    END COMPONENT;


    -- no separate st1in as st0out has internal flip-flops
    signal st0out : Stage_0_1_Interface;
    -- explicit initialization to avoid an optimization warning
    signal st1out, st2in : Stage_1_2_Interface := Stage_1_2_Interface_zero;
    signal st2out, st3in : Stage_2_3_Interface;

    signal st1_stall_flag               : std_logic;
    signal st2_reg1_fwd, st2_reg2_fwd   : FwdValue;
    signal branch_flag                  : std_logic;
    signal branch_dest                  : MemWordAddr;

begin

    inst_my_regs : my_regs PORT MAP (
        clk     => clk,
        rd1     => reg_idx1,
        rd2     => reg_idx2,
        reg1    => reg_dout1,
        reg2    => reg_dout2,
        we      => wr_reg_en,
        wr_idx  => wr_reg_idx,
        wr_data => wr_reg_data);


    inst_pl_stage0: pl_stage0 PORT MAP(
        clk             => clk,
        iram_addr       => iram_addra,
        st0out          => st0out,
        branch_flag     => branch_flag,
        branch_dest     => branch_dest,
        st1_stall_flag  => st1_stall_flag);

    inst_pl_stage1: pl_stage1 PORT MAP(
        clk             => clk,
        iram_dout       => iram_douta,
        st1in           => st0out,
        reg_idx1        => reg_idx1,
        reg_idx2        => reg_idx2,
        reg_dout1       => reg_dout1,
        reg_dout2       => reg_dout2,
        st1out          => st1out,
        branch_flag     => branch_flag,
        st1_stall_flag  => st1_stall_flag);

    inst_pl_stage2: pl_stage2 PORT MAP(
        clk         => clk,
        st2in       => st2in,
        reg1_fwd    => st2_reg1_fwd,
        reg2_fwd    => st2_reg2_fwd,
        branch_flag => branch_flag,
        branch_dest => branch_dest,
        st2out      => st2out,
        iram_en     => iram_enb,
        iram_we     => iram_web,
        iram_addr   => iram_addrb,
        iram_din    => iram_dinb,
        dram_ena    => dram_ena,
        dram_wea    => dram_wea,
        dram_addra  => dram_addra,
        dram_dina   => dram_dina,
        dram_enb    => dram_enb,
        dram_web    => dram_web,
        dram_addrb  => dram_addrb,
        dram_dinb   => dram_dinb,
        io_reg_we   => io_leds_we,
        io_reg_inp  => io_leds_inp);

    inst_pl_stage3: pl_stage3 PORT MAP(
        clk         => clk,
        st3in       => st3in,
        iram_dout   => iram_doutb,
        dram_douta  => dram_douta,
        dram_doutb  => dram_doutb,
        wr_reg_en   => wr_reg_en,
        wr_reg_idx  => wr_reg_idx,
        wr_reg_data => wr_reg_data);

    inst_hazardctl : hazardctl PORT MAP(
        clk                 => clk,
        st1_reg1_idx        => reg_idx1,
        st1_reg2_idx        => reg_idx2,
        st2_wr_reg_en       => st2in.wr_reg_en,
        st2_wr_reg_src      => st2in.wr_reg_src,
        st2_wr_reg_idx      => st2in.wr_reg_idx,
        st2_invalid_flag    => st2in.invalid_flag,
        st3_alu_res         => st3in.alu_res,
        st2_reg1_fwd        => st2_reg1_fwd,
        st2_reg2_fwd        => st2_reg2_fwd,
        st1_stall_flag      => st1_stall_flag);


    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            -- no need to copy st0out, see Stage_0_1_Interface
            st2in <= st1out;
            st3in <= st2out;
        end if;
    end process;

end Behavioral;

