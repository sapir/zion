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


    signal pc       : MemWordAddr := (others => '0');

    type Stage_0_1_Interface is
        record
            instr       : Logic_Word;
            -- next_pc actually also loops back to be used in stage 0
            next_pc     : MemWordAddr;
        end record;
    signal st0out, st1in : Stage_0_1_Interface;


    signal cur_opcode : Opcode_Type;
    -- some instruction fields that are useful to multiple instructions
    signal rd, rs, rt : Reg_Index;
    signal imm8       : Logic_Byte;

    type ImmOrReg_Type is
        record
            imm     : Logic_Word;
            reg_idx : Reg_Index;
            reg_val : Logic_Word;
            -- if '1', use register. if '0', use immediate value.
            use_reg : std_logic;
        end record;

    type Branch_Type is (b_none, b_always, b_eqz, b_nez);
    type Write_Type is (wr_none, wr_alu_to_reg, wr_memb_to_reg, wr_memw_to_reg,
        wr_reg_to_memb, wr_reg_to_memw);

    type Stage_1_2_Interface is
        record
            -- inputs to stage 2
            alu_op      : Alu_Op_Type;

            value1      : ImmOrReg_Type;    -- associated register is in rs
            value2      : ImmOrReg_Type;    -- associated register is in rt

            -- type of [conditional] branch to be performed. register to be
            -- tested will be in value1. (ALU inputs are set to ignore
            -- these values). Branch destination will be in alu_res
            branch_type : Branch_Type;

            -- inputs to stages 3 & 4
            wr_type     : Write_Type;
            wr_reg_idx  : Reg_Index;
        end record;
    signal st1out, st2in : Stage_1_2_Interface;


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

            -- copied from stage1
            wr_type     : Write_Type;
            wr_reg_idx  : Reg_Index;
            reg2_idx    : Reg_Index;    -- st1out.value2.reg_idx
            reg2_val    : Logic_Word;   -- not necessarily copied from stage1
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


begin
    -------------------------------
    -- Stage 0: Instruction Fetch
    -------------------------------

    st0_sync_proc : process(clk, st1in.next_pc)
    begin
        if rising_edge(clk) then
            pc <= st1in.next_pc;
        end if;
    end process;

    st0_comb_proc : process(pc, st2_branch_flag, st2out.alu_res)
    begin
        if st2_branch_flag = '1' then
            -- truncate result
            st0out.next_pc <= st2out.alu_res(9 downto 0);
        else
            st0out.next_pc <= std_logic_vector(unsigned(pc) + 1);
        end if;
    end process;

    -- instruction memory
    iram : instr_blkmem PORT MAP (
        clka => clk,
        addra => pc,
        douta => st0out.instr);


    -------------------------------------------------
    -- Stage 1: Instruction Decode + read registers
    -------------------------------------------------

    st1_sync_proc : process(clk)
    begin
        if rising_edge(clk) then
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
        clk     => clk);

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
                when "100" => cur_opcode <= opc_lb;
                when "101" => cur_opcode <= opc_lw;
                when "110" => cur_opcode <= opc_sb;
                when "111" => cur_opcode <= opc_sw;
                when others => cur_opcode <= opc_break;
            end case;

        elsif st1in.instr(14) = '0' then
            -- 5-bit opcode. first 2 bits are 10
            case st1in.instr(13 downto 11) is
                when "000" => cur_opcode <= opc_li8;
                when "001" => cur_opcode <= opc_lui;
                when "010" => cur_opcode <= opc_addi;
                when "011" => cur_opcode <= opc_ori;
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

    -- decide on stage1 outputs
    st1_output_proc : process(st1in, cur_opcode,
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
        st1out.wr_type          <= wr_none;
        st1out.wr_reg_idx       <= (others => '0');

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

                    -- subtract then check flags
                    -- TODO: actually implement it
                    when opc_slt | opc_sltu => st1out.alu_op <= aluop_sub;

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
                st1out.branch_type <= b_always;

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= "000000" & st1in.next_pc;
                st1out.value2.imm <= Logic_Word(
                    resize(signed(st1in.instr(10 downto 0)), 16));

                -- TODO: link for bal!

            -- IFmt_Branch
            when opc_beqz | opc_bnez =>
                if cur_opcode = opc_beqz then
                    st1out.branch_type <= b_eqz;
                else
                    st1out.branch_type <= b_nez;
                end if;

                -- branch instruction format is weird.
                -- also - we'll be using the value of $rt (value2.reg_val) to
                -- evaluate the branch condition, but the ALU will be ignoring
                -- this value because we're setting use_reg to '0'. we'll
                -- provide it with an immediate value instead.
                st1out.value2.use_reg <= '0';

                st1out.alu_op <= aluop_add;
                st1out.value1.imm <= "000000" & st1in.next_pc;
                st1out.value2.imm <= Logic_Word(
                    resize(signed(st1in.instr(6 downto 0)), 16));

            -- IFmt_JmpReg
            when opc_jr | opc_jalr =>
                st1out.branch_type <= b_always;

                st1out.alu_op <= aluop_add;
                st1out.value1.use_reg <= '1';
                st1out.value2.imm <= (others => '0');

                -- TODO: link for jalr!

            -- TODO: opc_break?

            when others =>
                -- use defaults
        end case;

        -- handle control hazard: invalidate instruction following
        -- successful branch
        -- TODO: consider putting the branch flag in st2out and then
        -- invalidating instruction after it reaches stage 2 using
        -- st3in.branch_flag. may make design faster.
        if st2_branch_flag = '1' then
            st1out.branch_type      <= b_none;
            st1out.wr_type          <= wr_none;
        end if;
    end process;


    -------------------------------------------------
    -- Stage 2: Execute
    -------------------------------------------------

    st2_sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            st2in <= st1out;
        end if;
    end process;

    st2_data_hazard_proc : process(st2in.value1, st2in.value2,
        st3in.wr_type, st3in.wr_reg_idx,
        st4in.wr_reg_en, st4in.wr_reg_idx)
    begin
        -- default: use values from stage 1
        st2_reg1_val <= st2in.value1.reg_val;
        st2_reg2_val <= st2in.value2.reg_val;

        -- forward from stage 4
        if st4in.wr_reg_en = '1' then
            if st4in.wr_reg_idx = st2in.value1.reg_idx then
                st2_reg1_val <= st4in.wr_reg_data;
            elsif st4in.wr_reg_idx = st2in.value2.reg_idx then
                st2_reg2_val <= st4in.wr_reg_data;
            end if;
        end if;

        -- forward from stage 3. (preferred over stage 4)
        case st3in.wr_type is
            when wr_alu_to_reg =>
                if st3in.wr_reg_idx = st2in.value1.reg_idx then
                    st2_reg1_val <= st3in.alu_res;
                elsif st3in.wr_reg_idx = st2in.value2.reg_idx then
                    st2_reg2_val <= st3in.alu_res;
                end if;

            when wr_memb_to_reg | wr_memw_to_reg =>
                -- TODO: stall! (or delay slot)

            when others =>
                -- no data hazard
        end case;
    end process;
 
    st2_branch_proc : process(st2in, st2_reg2_val)
    begin
        case st2in.branch_type is
            when b_always =>
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
        op =>  st2in.alu_op,
        a  =>  alu_inp1,    -- decided in st2_alu_proc
        b  =>  alu_inp2,    -- decided in st2_alu_proc
        res => st2out.alu_res);

    -- forward values from stage1
    st2out.wr_type      <= st2in.wr_type;
    st2out.wr_reg_idx   <= st2in.wr_reg_idx;
    st2out.reg2_idx     <= st2in.value2.reg_idx;
    st2out.reg2_val     <= st2_reg2_val;    -- ok, not necessarily from stage 1.


    -------------------------------------------------
    -- Stage 3: Memory
    -------------------------------------------------

    st3_sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            st3in <= st2out;
        end if;
    end process;

    st3_data_hazard_proc : process(st3in.reg2_idx, st3in.reg2_val,
        st4in.wr_reg_en, st4in.wr_reg_idx)
    begin
        -- default: use values from stage 2
        st3_reg2_val <= st3in.reg2_val;

        -- forward from stage 4
        if st4in.wr_reg_en = '1' and st4in.wr_reg_idx = st3in.reg2_idx then
            st3_reg2_val <= st4in.wr_reg_data;
        end if;
    end process;

    st3_dram_inps_proc : process(st3in, st3_reg2_val)
        -- address to use (truncated value of alu_res)
        variable addr0  : MemByteAddr;
        -- address following addr0
        variable addr1  : MemByteAddr;
    begin
        addr0 := st3in.alu_res(10 downto 0);
        addr1 := MemByteAddr(unsigned(addr0) + 1);

        -- default values:
        dram_ena    <= '0';
        dram_wea    <= "0";
        dram_addra  <= addr0;
        dram_dina   <= (others => '0');
        dram_enb    <= '0';
        dram_web    <= "0";
        dram_addrb  <= addr1;
        dram_dinb   <= (others => '0');

        case st3in.wr_type is
            when wr_reg_to_memb =>
                dram_ena    <= '1';
                dram_wea    <= "1";
                -- write register's low byte
                dram_dina   <= st3_reg2_val(7 downto 0);

            when wr_reg_to_memw =>
                dram_ena    <= '1';
                dram_wea    <= "1";
                dram_dina   <= st3_reg2_val(15 downto 8);
                dram_enb    <= '1';
                dram_web    <= "1";
                dram_dinb   <= st3_reg2_val(7 downto 0);

            when wr_memb_to_reg =>
                dram_ena    <= '1';

            when wr_memw_to_reg =>
                dram_ena    <= '1';
                dram_enb    <= '1';

            when others =>
                -- use defaults
        end case;
    end process;

    dram : data_blockmem PORT MAP (
        clka    => clk,
        ena     => dram_ena,
        wea     => dram_wea,
        addra   => dram_addra,
        dina    => dram_dina,
        douta   => dram_douta,
        clkb    => clk,
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

            when others =>
                st3out.wr_reg_data  <= (others => '0');
                st3out.wr_reg_en    <= '0';
        end case;

        st3out.wr_reg_idx   <= st3in.wr_reg_idx;
    end process;


    -------------------------------------------------
    -- Stage 4: Write-Back
    -------------------------------------------------

    st4_sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            st4in <= st3out;
        end if;
    end process;

    -- implementation is in stage 0; see use of st4in inputs there


    -------------------------------------------------
    -- I/O
    -------------------------------------------------

    -- force registers to an output so entire design isn't optimized away.
    -- TODO: memory-mapped IO instead
    leds <= std_logic_vector(st1out.value1.reg_val(7 downto 0));
end Behavioral;