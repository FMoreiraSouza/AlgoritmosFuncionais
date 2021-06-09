let r

//Operações básicas
const mult = a => b => a * b

r = mult(2)(5)
r

const PRI = a => _ => a
r = PRI(1)(2)
r

const M = _ => b => _ => b
r = M(1)(2)(8)
r

const ULT = _ => b => b
r = ULT(1)(2)
r

const TRO = f => a => b => f(b)(a)
r = TRO(PRI)(8)(2)
r

r = TRO(ULT)(8)(2)
r

// Operações booleanas

const T = PRI 
const F = ULT
T.inspect = () => 'Verdadeiro PRI'
F.inspect = () => 'Falso ULT'

const NOT = a => a(F)(T)
r = NOT(F)
r

const AND = a => b => a(b)(F)
r = AND(T)(F)
r
r

const NAND = f => g => a => b => f(g(a)(b))
r = NAND(NOT)(AND)(F)(F)
r

const OR = a => b => a(T)(b)
r = OR(F)(T)
r

const NOR = f => g => a => b => f(g(a)(b))
r = NOR(NOT)(OR)(F)(F)
r

const XOR = f => g => h => a => b => f(g(h(a)(b)))
r = XOR(OR)(AND)(NOT)(T)(T)
r
