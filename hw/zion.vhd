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

    -- Instruction RAM component (actually ROM)
    -- TODO: allow word reads & writes from Instruction RAM using port B
    COMPONENT instr_blkmem
      PORT (
        clka : IN STD_LOGIC;
        addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        douta : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
      );
    END COMPONENT;

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

    -- ALU
    COMPONENT alu
    PORT (
        op  : in Alu_Op_Type;
        a   : in Logic_Word;
        b   : in Logic_Word;
        res : out Logic_Word);
    END COMPONENT;

    -- Data RAM component
    COMPONENT data_blockmem
      PORT (
        clka : IN STD_LOGIC;
        ena : IN STD_LOGIC;
        wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
        addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
        dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        clkb : IN STD_LOGIC;
        enb : IN STD_LOGIC;
        web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
        addrb : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
        dinb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
      );
    END COMPONENT;


    -- dcm output
    signal dcm_clk              : std_logic;

    signal pc       : MemWordAddr := (others => '0');
    -- if '1', pc in stage 0 won't be updated to stage 1's next_pc, so
    -- instruction in stage 1 will be repeated. (stage 1 should of course also
    -- be invalidated using st1out.invalid_flag.) in case of branch from stage
    -- 2, stall of stage 1 will be ignored; it should have been invalidated
    -- anyway.
    signal stall_st1 : std_logic := '0';

    type Stage_0_1_Interface is
        record
            instr       : Logic_Word;
            -- next_pc actually also loops back to be used in stage 0
            next_pc     : MemWordAddr;

            -- TODO: isn't this the same thing as next_pc when it matters?
            -- inputs to stage 4
            pc_plus_2   : MemWordAddr;      -- used for branch link
        end record;
    signal st0out, st1in : Stage_0_1_Interface;


    signal cur_opcode : Opcode_Type;
    -- some instruction fields that are useful to multiple instructions
    signal rd, rs, rt : Reg_Index;
    signal imm8       : Logic_Byte;

    -- source of register values to use in stage 2
    -- (from perspective of instruction running in stage 2, though this value
    -- is set in stage 1)
    type RegValue_Src is (
        -- use register value read in stage 1
        rvs_st1,
        -- use ALU res passed to stage 3 from previous instruction's stage 2
        rvs_st3_alu,
        -- use $pc + 2 value passed to stage 3
        rvs_st3_pc_plus_2,
        -- use data currently being written to this register in stage 4
        rvs_st4_wr_data);

    type ImmOrReg_Type is
        record
            imm     : Logic_Word;
            reg_idx : Reg_Index;
            reg_val : Logic_Word;
            -- source of register value to use, after data hazards are taken
            -- into account
            regval_src : RegValue_Src;
            -- if '1', use register. if '0', use immediate value.
            use_reg : std_logic;
        end record;

    type Branch_Type is (b_none, b_always_imm, b_always_reg, b_eqz, b_nez);
    type Write_Type is (wr_none, wr_alu_to_reg, wr_memb_to_reg, wr_memw_to_reg,
        wr_reg_to_memb, wr_reg_to_memw, wr_pc_plus_2_to_ra);

    type Stage_1_2_Interface is
        record
            -- inputs to stage 2

            -- if '1', don't do anything (no branches or writes)
            invalid_flag: std_logic;

            alu_op      : Alu_Op_Type;

            value1      : ImmOrReg_Type;    -- associated register is in rs
            value2      : ImmOrReg_Type;    -- associated register is in rt

            -- type of [conditional] branch to be performed. register to be
            -- tested will be in value1. (ALU inputs are set to ignore
            -- these values). Branch destination will be calculated by ALU
            branch_type : Branch_Type;
            -- pc after branch (except for b_always_reg, in which case reg2 is
            -- used). not calculated using ALU to avoid data hazard detection
            -- on ALU inputs.
            branch_dest : MemWordAddr;

            -- inputs to stages 3 & 4
            wr_type     : Write_Type;
            wr_reg_idx  : Reg_Index;

            -- copied from stage 0
            pc_plus_2   : MemWordAddr;
        end record;
    -- explicit initialization to avoid an optimization warning
    signal st1out, st2in : Stage_1_2_Interface
        := (invalid_flag => '0',
            alu_op      => aluop_add,
            value1      => (imm => (others => '0'),
                            reg_idx => (others => '0'),
                            reg_val => (others => '0'),
                            regval_src => rvs_st1,
                            use_reg => '0'),
            value2      => (imm => (others => '0'),
                            reg_idx => (others => '0'),
                            reg_val => (others => '0'),
                            regval_src => rvs_st1,
                            use_reg => '0'),
            branch_type => b_none,
            wr_type     => wr_none,
            others      => (others => '0'));


    -- value of $rs and $rt for stage 2. may be different from
    -- st2in.value1/2.reg_val due to data hazards and forwarding.
    signal st2_reg1_val : Logic_Word;
    signal st2_reg2_val : Logic_Word;
    -- inputs for 'main_alu' instance
    signal alu_inp1 : Logic_Word;
    signal alu_inp2 : Logic_Word;
    -- whether branch should be performed.
    -- is calculated in stage 2 but not used in stage 3 (so not in
    -- Stage_2_3_Interface).
    -- instead, it's used directly by stage 0 (to perform the branch) and
    -- stage 1 (to invalidate instruction following branch).
    signal st2_branch_flag : std_logic;

    type Stage_2_3_Interface is
        record
            alu_res     : Logic_Word;

            -- copied from stage 0
            pc_plus_2   : MemWordAddr;

            -- copied from stage 1
            wr_type     : Write_Type;   -- rather than forward invalid_flag to
                                            -- stage 3, this just gets set to
                                            -- wr_none
            wr_reg_idx  : Reg_Index;
            reg2_idx    : Reg_Index;    -- st1out.value2.reg_idx
            reg2_val    : Logic_Word;   -- not necessarily copied from stage 1
                                            -- due to data hazard handling
        end record;
    signal st2out, st3in : Stage_2_3_Interface;


    -- value of $rt for stage 3. same idea as st2_reg2_val above.
    signal st3_reg2_val : Logic_Word;
    -- dram inputs (copied from component definition)
    signal dram_ena     : STD_LOGIC;
    signal dram_wea     : STD_LOGIC_VECTOR(0 DOWNTO 0);
    signal dram_addra   : STD_LOGIC_VECTOR(10 DOWNTO 0);
    signal dram_dina    : STD_LOGIC_VECTOR(7 DOWNTO 0);
    signal dram_enb     : STD_LOGIC;
    signal dram_web     : STD_LOGIC_VECTOR(0 DOWNTO 0);
    signal dram_addrb   : STD_LOGIC_VECTOR(10 DOWNTO 0);
    signal dram_dinb    : STD_LOGIC_VECTOR(7 DOWNTO 0);
    -- dram outputs (copied from component definition)
    signal dram_douta   : STD_LOGIC_VECTOR(7 DOWNTO 0);
    signal dram_doutb   : STD_LOGIC_VECTOR(7 DOWNTO 0);

    type Stage_3_4_Interface is
        record
            wr_reg_data : Logic_Word;   -- data to be written to register
            wr_reg_en   : std_logic;    -- whether to write to register file
            wr_reg_idx  : Reg_Index;
        end record;
    signal st3out, st4in : Stage_3_4_Interface;


    -- I/O memory

    -- byte-sized I/O register. cur gets set to inp if we is "1".
    type IO_Register_Byte is
        record
            cur : Logic_Byte;
            inp : Logic_Byte;
            -- using a vector to make initialization easier
            we  : std_logic_vector(0 downto 0);
        end record;

    signal io_leds_reg : IO_Register_Byte
        := (others => (others => '0'));

    ------------------------------------------
    -- Memory-mapped I/O addresses (MSB is 1)
    ------------------------------------------
    -- LEDs bitmask. byte-only and write-only.
    constant iomem_addr_leds : Logic_Word := "1000000000000000";

begin

    inst_dcm1: dcm1 PORT MAP(
        CLKIN_IN        => clk,
        CLKFX_OUT       => dcm_clk,
        CLKIN_IBUFG_OUT => open,
        CLK0_OUT        => open
    );

    -------------------------------
    -- Stage 0: Instruction Fetch
    -------------------------------

    st0_sync_proc : process(dcm_clk, st1in.next_pc)
    begin
        if rising_edge(dcm_clk) then
            pc <= st1in.next_pc;
        end if;
    end process;

    st0out.pc_plus_2 <= std_logic_vector(unsigned(pc) + 1);

    st0_comb_proc : process(pc, st0out.pc_plus_2, stall_st1,
        st2_branch_flag, st2in.branch_type, st2in.branch_dest, st2_reg2_val)
    begin
        if st2_branch_flag = '1' then
            if st2in.branch_type = b_always_reg then
                st0out.next_pc <= st2_reg2_val(9 downto 0);
            else
                -- all other branch types calculate the destination in stage 1
                st0out.next_pc <= st2in.branch_dest;
            end if;

        elsif stall_st1 = '1' then
            st0out.next_pc <= pc;

        else
            -- copy value we're passing to stage 4
            st0out.next_pc <= st0out.pc_plus_2;
        end if;
    end process;

    -- instruction memory
    iram : instr_blkmem PORT MAP (
        clka => dcm_clk,
        addra => pc,
        douta => st0out.instr);


    -------------------------------------------------
    -- Stage 1: Instruction Decode + read registers
    -------------------------------------------------

    st1_sync_proc : process(dcm_clk)
    begin
        if rising_edge(dcm_clk) then
            st1in <= st0out;
        end if;
    end process;

    rd <= st1in.instr(11 downto 8);
    rs <= st1in.instr(7 downto 4);
    rt <= st1in.instr(3 downto 0);
    imm8 <= st1in.instr(11 downto 4);

    reg_file: my_regs PORT MAP(
        rd1     => rs,
        rd2     => rt,
        reg1    => st1out.value1.reg_val,
        reg2    => st1out.value2.reg_val,
        -- write is done in stage 4
        wr_idx  => st4in.wr_reg_idx,
        wr_data => st4in.wr_reg_data,
        we      => st4in.wr_reg_en,
        clk     => dcm_clk);

    st1_opcode_proc : process(st1in)
    begin
        cur_opcode <= opc_break; -- catch-all
        if st1in.instr(15) = '0' then
            -- 4-bit opcode. first bit is 0
            case st1in.instr(14 downto 12) is
                when "000" => cur_opcode <= opc_add;
                when "001" => cur_opcode <= opc_sub;
                when "010" => cur_opcode <= opc_slt;
                when "011" => cur_opcode <= opc_sltu;
                when "100" => cur_opcode <= opc_li8;
                when "101" => cur_opcode <= opc_lui;
                when "110" => cur_opcode <= opc_addi;
                when "111" => cur_opcode <= opc_ori;
                when others => cur_opcode <= opc_break;
            end case;

        elsif st1in.instr(14) = '0' then
            -- 5-bit opcode. first 2 bits are 10
            case st1in.instr(13 downto 11) is
                when "000" => cur_opcode <= opc_lb;
                when "001" => cur_opcode <= opc_lw;
                when "010" => cur_opcode <= opc_sb;
                when "011" => cur_opcode <= opc_sw;
                when "100" => cur_opcode <= opc_b;
                when "101" => cur_opcode <= opc_bal;
                when "110" => cur_opcode <= opc_beqz;
                when "111" => cur_opcode <= opc_bnez;
                when others => cur_opcode <= opc_break;
            end case;

        else
            -- 8-bit opcode. first 2 bits are 11
            case st1in.instr(13 downto 8) is
                when "000000" => cur_opcode <= opc_sll;
                when "000001" => cur_opcode <= opc_srl;
                when "000010" => cur_opcode <= opc_slli;
                when "000011" => cur_opcode <= opc_srli;
                when "000100" => cur_opcode <= opc_and;
                when "000101" => cur_opcode <= opc_or;
                when "000110" => cur_opcode <= opc_nor;
                when "000111" => cur_opcode <= opc_xor;
                when "001000" => cur_opcode <= opc_jr;
                when "001001" => cur_opcode <= opc_jalr;
                when "001010" => cur_opcode <= opc_exts;

                when "001100" => cur_opcode <= opc_break;
                when others => cur_opcode <= opc_break;
            end case;
        end if; -- opcode width
    end process;

    -- decide on stage 1 outputs
    st1_output_proc : process(st1in, cur_opcode, st2_branch_flag,
        rd, rs, rt, imm8)
    begin
        -- first set default values
        st1out.alu_op           <= aluop_add;
        st1out.value1.use_reg   <= '0';
        st1out.value1.reg_idx   <= rs;
        st1out.value1.imm       <= (others => '0');
        st1out.value2.use_reg   <= '0';
        st1out.value2.reg_idx   <= rt;
        st1out.value2.imm       <= (others => '0');
        st1out.branch_type      <= b_none;
        st1out.branch_dest      <= (others => '0');
        st1out.wr_type          <= wr_none;
        st1out.wr_reg_idx       <= (others => '0');
        st1out.pc_plus_2        <= st1in.pc_plus_2;

        case cur_opcode is
            -- IFmt_Math3, IFmt_Math2
            when  opc_add | opc_sub | opc_slt | opc_sltu
                | opc_sll | opc_srl | opc_and | opc_or
                | opc_nor | opc_xor | opc_exts =>

                case cur_opcode is
                    when opc_add            => st1out.alu_op <= aluop_add;
                    when opc_sub            => st1out.alu_op <= aluop_sub;
                    when opc_and            => st1out.alu_op <= aluop_and;
                    when opc_or             => st1out.alu_op <= aluop_or;
                    when opc_nor            => st1out.alu_op <= aluop_nor;
                    when opc_xor            => st1out.alu_op <= aluop_xor;
                    when opc_sll            => st1out.alu_op <= aluop_sll;
                    when opc_srl            => st1out.alu_op <= aluop_srl;
                    when opc_exts           => st1out.alu_op <= aluop_exts;
                    when opc_slt            => st1out.alu_op <= aluop_slt;
                    when opc_sltu           => st1out.alu_op <= aluop_sltu;

                    when others => st1out.alu_op <= aluop_add;
                end case;

                st1out.value1.use_reg <= '1';
                st1out.value2.use_reg <= '1';

                st1out.wr_type <= wr_alu_to_reg;
            
                case cur_opcode is
                    -- IFmt_Math3
                    when opc_add | opc_sub | opc_slt | opc_sltu =>
                        st1out.wr_reg_idx <= rd;

                    -- IFmt_Math2
                    when others =>
                        st1out.wr_reg_idx <= rs;
                end case;

            -- IFmt_Mem
            when opc_lb | opc_sb | opc_lw | opc_sw =>

                -- use ALU to calculate memory offset
                st1out.alu_op <= aluop_add;
                st1out.value1.use_reg <= '1';
                st1out.value2.imm <= Logic_Word(
                    resize(signed(st1in.instr(10 downto 8)), 16));

                -- decide what to write where
                case cur_opcode is
                    -- for loads, write mem output to $rt
                    when opc_lb =>
                        st1out.wr_type <= wr_memb_to_reg;
                        st1out.wr_reg_idx <= rt;

                    when opc_lw =>
                        st1out.wr_type <= wr_memw_to_reg;
                        st1out.wr_reg_idx <= rt;

                    -- for stores, write $rt to mem.
                    -- note that we're reading $rt but not passing it to the
                    -- alu - we're setting use_reg to '0'. alu calculation uses
                    -- value2.imm instead
                    when opc_sb =>
                        st1out.wr_type <= wr_reg_to_memb;
                        st1out.value2.use_reg <= '0';

                    when opc_sw =>
                        st1out.wr_type <= wr_reg_to_memw;
                        st1out.value2.use_reg <= '0';

                    when others =>
                        -- impossible
                end case;

            -- IFmt_Imm8
            when opc_li8 =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= (others => '0');
                st1out.value2.imm <= "00000000" & imm8;

            when opc_lui =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= (others => '0');
                st1out.value2.imm <= imm8 & "00000000";

            when opc_addi =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= Logic_Word(resize(signed(imm8), 16));
                st1out.value2.use_reg <= '1';

            when opc_ori =>
                st1out.wr_reg_idx <= rt;
                st1out.wr_type <= wr_alu_to_reg;

                st1out.alu_op <= aluop_or;
                st1out.value1.imm <= "00000000" & imm8;
                st1out.value2.use_reg <= '1';

            -- IFmt_JmpRel
            when opc_b | opc_bal =>
                st1out.branch_type <= b_always_imm;

                st1out.branch_dest <= MemWordAddr(
                    unsigned(st1in.next_pc)
                    -- TODO: we're ignoring msb of branch offset
                    + unsigned(st1in.instr(9 downto 0)));

                -- link: save $pc+2 in $ra
                if cur_opcode = opc_bal then
                    st1out.wr_reg_idx <= ra_reg_idx;
                    st1out.wr_type <= wr_pc_plus_2_to_ra;
                end if;

            -- IFmt_Branch
            when opc_beqz | opc_bnez =>
                if cur_opcode = opc_beqz then
                    st1out.branch_type <= b_eqz;
                else
                    st1out.branch_type <= b_nez;
                end if;

                -- branch instruction format is weird.
                st1out.branch_dest <= MemWordAddr(
                    unsigned(st1in.next_pc)
                    + unsigned(resize(signed(st1in.instr(6 downto 0)), 10)));

                -- note we'll be using the value of $rt (value2.reg_val) to
                -- evaluate the branch condition, but we don't care what the
                -- ALU sees because we won't be using it.

            -- IFmt_JmpReg
            when opc_jr | opc_jalr =>
                st1out.branch_type <= b_always_reg;

                -- note we'll be using the value of $rt as the branch
                -- destination, but we don't care what the ALU sees because
                -- we won't be using it.

                -- link: save $pc+2 in $ra
                if cur_opcode = opc_jalr then
                    st1out.wr_reg_idx <= ra_reg_idx;
                    st1out.wr_type <= wr_pc_plus_2_to_ra;
                end if;

            -- TODO: opc_break?

            when others =>
                -- use defaults
        end case;
    end process;

    st1_hazards_proc : process(st1out.value1, st1out.value2,
        st2in.wr_type, st2in.wr_reg_idx, st2_branch_flag,
        st3in.wr_type, st3in.wr_reg_idx)
    begin
        -- default: *don't* invalidate or stall stuff
        st1out.invalid_flag <= '0';
        stall_st1           <= '0';

        -- figure out what data hazards stage 2 will encounter.
        -- do this early (in stage 1) so that stage 2 is shorter.

        -- default: use values from stage 1 unless there's a data hazard
        st1out.value1.regval_src <= rvs_st1;
        st1out.value2.regval_src <= rvs_st1;

        -- forward from stage 4 (which is currently stage 3)
        case st3in.wr_type is
            when wr_alu_to_reg | wr_pc_plus_2_to_ra
                | wr_memb_to_reg | wr_memw_to_reg =>

                if st3in.wr_reg_idx = st1out.value1.reg_idx then
                    st1out.value1.regval_src <= rvs_st4_wr_data;
                end if;

                if st3in.wr_reg_idx = st1out.value2.reg_idx then
                    st1out.value2.regval_src <= rvs_st4_wr_data;
                end if;

            when others =>
                -- no data hazard
        end case;

        -- forward from stage 3 (which is currently stage 2).
        -- preferred over stage 4 because it comes later.
        case st2in.wr_type is
            when wr_alu_to_reg =>
                if st2in.wr_reg_idx = st1out.value1.reg_idx then
                    st1out.value1.regval_src <= rvs_st3_alu;
                end if;

                if st2in.wr_reg_idx = st1out.value2.reg_idx then
                    st1out.value2.regval_src <= rvs_st3_alu;
                end if;

            when wr_memb_to_reg | wr_memw_to_reg =>
                st1out.invalid_flag <= '1';
                -- try this instruction again next cycle
                stall_st1 <= '1';

            when wr_pc_plus_2_to_ra =>
                if ra_reg_idx = st1out.value1.reg_idx then
                    st1out.value1.regval_src <= rvs_st3_pc_plus_2;
                end if;

                if ra_reg_idx = st1out.value2.reg_idx then
                    st1out.value2.regval_src <= rvs_st3_pc_plus_2;
                end if;

            when others =>
                -- no data hazard
        end case;


        -- handle control hazard: invalidate instruction following
        -- successful branch
        -- TODO: consider putting the branch flag in st2out and then
        -- invalidating instruction after it reaches stage 2 using
        -- st3in.branch_flag. may make design faster.
        if st2_branch_flag = '1' then
            st1out.invalid_flag <= '1';
        end if;
    end process;


    -------------------------------------------------
    -- Stage 2: Execute
    -------------------------------------------------

    st2_sync_proc : process(dcm_clk)
    begin
        if rising_edge(dcm_clk) then
            st2in <= st1out;
        end if;
    end process;

    st2_data_hazard_proc : process(st2in.value1, st2in.value2,
        st3in.alu_res, st3in.pc_plus_2,
        st4in.wr_reg_data)
    begin
        case st2in.value1.regval_src is
            when rvs_st1 =>     st2_reg1_val <= st2in.value1.reg_val;
            when rvs_st3_alu => st2_reg1_val <= st3in.alu_res;

            when rvs_st3_pc_plus_2 =>
                                st2_reg1_val <= "000000" & st3in.pc_plus_2;

            when rvs_st4_wr_data =>
                                st2_reg1_val <= st4in.wr_reg_data;

            -- impossible
            when others =>      st2_reg1_val <= (others => '0');
        end case;

        case st2in.value2.regval_src is
            when rvs_st1 =>     st2_reg2_val <= st2in.value2.reg_val;
            when rvs_st3_alu => st2_reg2_val <= st3in.alu_res;

            when rvs_st3_pc_plus_2 =>
                                st2_reg2_val <= "000000" & st3in.pc_plus_2;

            when rvs_st4_wr_data =>
                                st2_reg2_val <= st4in.wr_reg_data;

            -- impossible
            when others =>      st2_reg2_val <= (others => '0');
        end case;
    end process;
 
    st2_branch_proc : process(st2in, st2_reg2_val)
    begin
        if st2in.invalid_flag = '0' then
            case st2in.branch_type is
                when b_always_imm | b_always_reg =>
                    st2_branch_flag <= '1';

                when b_eqz =>
                    if st2_reg2_val = "0000000000000000" then
                        st2_branch_flag <= '1';
                    else
                        st2_branch_flag <= '0';
                    end if;

                when b_nez =>
                    if st2_reg2_val /= "0000000000000000" then
                        st2_branch_flag <= '1';
                    else
                        st2_branch_flag <= '0';
                    end if;

                when others =>
                    st2_branch_flag <= '0';
            end case;
        else -- if invalid
            st2_branch_flag <= '0';
        end if;
    end process;

    -- sub-records (st2in.value*) specified explicitly to avoid warning
    -- (note that register values aren't used directly; outputs of
    -- st2_data_hazard_proc are used instead.)
    st2_alu_proc : process(st2in, st2in.value1, st2in.value2,
        st2_reg1_val, st2_reg2_val)
    begin
        if st2in.value1.use_reg = '1' then
            alu_inp1 <= st2_reg1_val;
        else
            alu_inp1 <= st2in.value1.imm;
        end if;

        if st2in.value2.use_reg = '1' then
            alu_inp2 <= st2_reg2_val;
        else
            alu_inp2 <= st2in.value2.imm;
        end if;
    end process;

    main_alu : alu PORT MAP (
        op  => st2in.alu_op,
        a   => alu_inp1,    -- decided in st2_alu_proc
        b   => alu_inp2,    -- decided in st2_alu_proc
        res => st2out.alu_res);

    -- forward values from stage 1
    st2out.wr_type      <= wr_none when st2in.invalid_flag = '1'
                                else st2in.wr_type;
    st2out.wr_reg_idx   <= st2in.wr_reg_idx;
    st2out.pc_plus_2    <= st2in.pc_plus_2;
    st2out.reg2_idx     <= st2in.value2.reg_idx;
    st2out.reg2_val     <= st2_reg2_val;    -- ok, not necessarily from stage 1.


    -------------------------------------------------
    -- Stage 3: Memory
    -------------------------------------------------

    st3_sync_proc : process(dcm_clk)
    begin
        if rising_edge(dcm_clk) then
            st3in <= st2out;
        end if;
    end process;

    st3_data_hazard_proc : process(st3in.reg2_idx, st3in.reg2_val,
        st4in.wr_reg_en, st4in.wr_reg_idx, st4in.wr_reg_data)
    begin
        -- default: use values from stage 2
        st3_reg2_val <= st3in.reg2_val;

        -- forward from stage 4
        if st4in.wr_reg_en = '1' and st4in.wr_reg_idx = st3in.reg2_idx then
            st3_reg2_val <= st4in.wr_reg_data;
        end if;
    end process;

    st3_dram_inps_proc : process(st3in, st3_reg2_val)
        -- 16-bit address of first byte we're reading/writing
        variable addr0 : Logic_Word;
        -- address following addr0
        variable addr1 : Logic_Word;

        -- bytes to be written at addr0, addr1
        variable byte0, byte1 : Logic_Byte;
    begin
        addr0 := st3in.alu_res;
        addr1 := Logic_Word(u16(addr0) + 1);

        case st3in.wr_type is
            when wr_reg_to_memb =>
                -- write register's low byte
                byte0 := st3_reg2_val(7 downto 0);
                byte1 := (others => '0');

            when wr_reg_to_memw =>
                byte0 := st3_reg2_val(15 downto 8);
                byte1 := st3_reg2_val(7 downto 0);

            when others =>
                -- not really writing anything
                byte0 := (others => '0');
                byte1 := (others => '0');
        end case;

        -- default values:
        dram_ena        <= '0';
        dram_wea        <= "0";
        dram_addra      <= addr0(10 downto 0);
        dram_dina       <= (others => '0');
        dram_enb        <= '0';
        dram_web        <= "0";
        dram_addrb      <= addr1(10 downto 0);
        dram_dinb       <= (others => '0');
        io_leds_reg.inp <= (others => '0');
        io_leds_reg.we  <= "1";

        if addr0(15) = '1' then
            -- Memory-mapped IO

            if addr0 = iomem_addr_leds and st3in.wr_type = wr_reg_to_memb then
                io_leds_reg.inp <= byte0;
                io_leds_reg.we <= "1";
            end if;
        else
            -- Regular memory

            case st3in.wr_type is
                when wr_reg_to_memb =>
                    dram_ena    <= '1';
                    dram_wea    <= "1";
                    dram_dina   <= byte0;

                when wr_reg_to_memw =>
                    dram_ena    <= '1';
                    dram_wea    <= "1";
                    dram_dina   <= byte0;
                    dram_enb    <= '1';
                    dram_web    <= "1";
                    dram_dinb   <= byte1;

                when wr_memb_to_reg =>
                    dram_ena    <= '1';

                when wr_memw_to_reg =>
                    dram_ena    <= '1';
                    dram_enb    <= '1';

                when others =>
                    -- use defaults
            end case;
        end if;
    end process;

    dram : data_blockmem PORT MAP (
        clka    => dcm_clk,
        ena     => dram_ena,
        wea     => dram_wea,
        addra   => dram_addra,
        dina    => dram_dina,
        douta   => dram_douta,
        clkb    => dcm_clk,
        enb     => dram_enb,
        web     => dram_web,
        addrb   => dram_addrb,
        dinb    => dram_dinb,
        doutb   => dram_doutb
      );

    st3_output_proc : process(st3in, dram_douta, dram_doutb)
    begin
        case st3in.wr_type is
            when wr_alu_to_reg =>
                st3out.wr_reg_data  <= st3in.alu_res;
                st3out.wr_reg_en    <= '1';

            when wr_memb_to_reg =>
                st3out.wr_reg_data  <= "00000000" & dram_douta;
                st3out.wr_reg_en    <= '1';

            when wr_memw_to_reg =>
                st3out.wr_reg_data  <= dram_douta & dram_doutb;
                st3out.wr_reg_en    <= '1';

            when wr_pc_plus_2_to_ra =>
                st3out.wr_reg_data  <= "000000" & st3in.pc_plus_2;
                st3out.wr_reg_en    <= '1';

            when others =>
                st3out.wr_reg_data  <= (others => '0');
                st3out.wr_reg_en    <= '0';
        end case;

        st3out.wr_reg_idx   <= st3in.wr_reg_idx;
    end process;


    -------------------------------------------------
    -- Stage 4: Write-Back
    -------------------------------------------------

    st4_sync_proc : process(dcm_clk)
    begin
        if rising_edge(dcm_clk) then
            st4in <= st3out;
        end if;
    end process;

    -- implementation is in stage 0; see use of st4in inputs there


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
