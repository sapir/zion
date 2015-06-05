#!/usr/bin/python
from __future__ import print_function
import operator
from collections import namedtuple


InstrField = namedtuple('InstrField', 'msb lsb width')
INSTR_FIELDS = {
    'opcode': InstrField(17, 12,  6),
    'rd':     InstrField(11,  8,  4),
    'rs':     InstrField( 7,  4,  4),
    'rt':     InstrField( 3,  0,  4),
    'imm8':   InstrField(11,  4,  8),
    'imm4':   InstrField( 7,  4,  4),   # same bits as rs but different usage
    'addr12': InstrField(11,  0, 12),
    'addr8':  InstrField(11,  4,  8),   # same as imm8 but different usage
    'regofs': InstrField(11,  4,  8),   # ofs is in rd, reg is in rs
    }


InstrFormat = namedtuple('InstrFormat', 'name operandFields')

IFmt_Math3   = InstrFormat('Math3',  'rd rs rt')
# for shift instructions, rs and rt are swapped
IFmt_Math3_shift = InstrFormat('Math3 (shift)', 'rd rt rs')
IFmt_Math2   = InstrFormat('Math2',  'rd rt')       # Math3 with ignored rs
IFmt_Imm8    = InstrFormat('Imm8',   'rt imm8')
IFmt_Imm4    = InstrFormat('Imm4',   'rt imm4')     # Imm8 with ignored bits
# memory access - 2 regs + 4-bit signed offset (-8 to 7)
# (for lw/sw, offset must be aligned anyway, so we can multiply by 2)
IFmt_Mem     = InstrFormat('Mem',    'rt regofs')
IFmt_JmpRel  = InstrFormat('JmpRel', 'addr12')
IFmt_JmpReg  = InstrFormat('JmpReg', 'rt')
IFmt_Branch  = InstrFormat('Branch', 'rt addr8')
IFmt_Special = InstrFormat('Special','')


# opcodes by group:
#
### math/logic
# add, sub
# slt, sltu
# sll, srl
# and, or, nor, xor
# addi, (andi, slti, sltiu)
# slli, srli
#
### immediate loads
# li8       # = addi w/ $zero
# (li16)    # = lui+addi/ori
# lui       # = li8 + slli 8
# (move)    # = add w/ $zero
#
### jumps and branches
# (j, jal)
# jr, jalr
# b, bal
# beqz, bnez
#
### memory access
# lb, lw
# sb, sw


OpcodeInfo = namedtuple('OpcodeInfo', 'id ifmt')

_O = OpcodeInfo

OPCODES = {
    # Opcode group 0: 3-register math & logic
    'add':  _O(0x00, IFmt_Math3),   # write to 0 for nop
    'sub':  _O(0x01, IFmt_Math3),   # used for neg
    'slt':  _O(0x02, IFmt_Math3),
    'sltu': _O(0x03, IFmt_Math3),
    'and':  _O(0x04, IFmt_Math3),
    'or':   _O(0x05, IFmt_Math3),
    'nor':  _O(0x06, IFmt_Math3),   # nor w/ self to get not
    'xor':  _O(0x07, IFmt_Math3),
    'sll':  _O(0x08, IFmt_Math3_shift),
    'srl':  _O(0x09, IFmt_Math3_shift),
    'exts': _O(0x0a, IFmt_Math2),   # also like Math3. rs is ignored.

    # Opcode group 1: reg+imm math & logic
    # (for opcodes with a 3-register version, sub-id should match
    # sub-id in group 0)
    'addi': _O(0x10, IFmt_Imm8),    # useful for loops and function epilogues
    'subi': _O(0x11, IFmt_Imm8),    # useful for function prologues
    'slti': _O(0x12, IFmt_Imm8),
    'sltiu':_O(0x13, IFmt_Imm8),
    'andi': _O(0x14, IFmt_Imm8),
    'ori':  _O(0x15, IFmt_Imm8),    # useful for li16
    'nori': _O(0x16, IFmt_Imm8),
    'xori': _O(0x17, IFmt_Imm8),
    'slli': _O(0x18, IFmt_Imm4),
    'srli': _O(0x19, IFmt_Imm4),
    'li8':  _O(0x1b, IFmt_Imm8),    # = 3-reg addi w/ $zero
    'lui':  _O(0x1c, IFmt_Imm8),    # = li8 + slli w/ 16

    # Opcode group 2: jumps and branches (6)
    # (low bit of opcode is used as link flag. non-linking jumps
    # should have even opcodes)
    'j':    _O(0x20, IFmt_JmpRel),
    'jal':  _O(0x21, IFmt_JmpRel),
    'jr':   _O(0x22, IFmt_JmpReg),
    'jalr': _O(0x23, IFmt_JmpReg),
    'beqz': _O(0x24, IFmt_Branch),
    'bnez': _O(0x26, IFmt_Branch),

    # Opcode group 3: everything else
    'lb':   _O(0x30, IFmt_Mem),
    'lw':   _O(0x31, IFmt_Mem),
    'sb':   _O(0x32, IFmt_Mem),
    'sw':   _O(0x33, IFmt_Mem),
    'break':_O(0x35, IFmt_Special),
    }

OPCODES_BY_ID = dict(
    (o.id, (name, o))
    for (name, o)
    in OPCODES.iteritems())


NICE_REGISTER_NAMES = dict(
    (name, num)
    for (num, name) in enumerate(
        '$zero $sp $s0 $s1'
        ' $a0 $a1 $v0 $v1'
        ' $a2 $a3 $v2 $v3'
        ' $s2 $s3 $fp $ra'
        .split()))

NUM_REGISTERS = len(NICE_REGISTER_NAMES)

REGISTER_NAMES = dict(NICE_REGISTER_NAMES)
REGISTER_NAMES.update(
    ('$r{0}'.format(i), i)
    for i in xrange(16))


def extractBits(n, msb, width):
    lsb = msb - width + 1
    n &= (1 << (msb + 1)) - 1
    n >>= lsb
    return n

def setBits(n, msb, width, val):
    lsb = msb - width + 1

    mask = (1 << width) - 1
    assert val == (val & mask)

    n &= ~(mask << lsb) # remove old bits
    n |= (val << lsb)   # set new bits
    return n

def getInstrField(n, name):
    fld = INSTR_FIELDS[name]
    return extractBits(n, fld.msb, fld.width)

def setInstrField(n, name, val):
    fld = INSTR_FIELDS[name]
    return setBits(n, fld.msb, fld.width, val)

def decodeML(word):
    opcodeID = getInstrField(word, 'opcode')
    name, opcodeInfo = OPCODES_BY_ID[opcodeID]
    operands = [getInstrField(word, f)
        for f in opcodeInfo.ifmt.operandFields.split()]
    return (name, operands)

def encodeML(opcode, operands):
    opcodeInfo = OPCODES[opcode]

    word = setInstrField(0, 'opcode', opcodeInfo.id)

    fieldNames = opcodeInfo.ifmt.operandFields.split()

    for (name, val) in zip(fieldNames, operands):
        word = setInstrField(word, name, val)

    return word
