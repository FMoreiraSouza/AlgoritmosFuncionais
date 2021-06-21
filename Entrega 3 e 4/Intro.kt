import java.util.*
import kotlin.math.max
import kotlin.math.min
import kotlin.math.sign


//Capítulo 3

// countNeg
var neg = { b: List<Int> -> b.filter { it < 0 }}
fun countneg (calc: List<Int>):Int {
    return calc.size
}

// Final
val final = {a: Int, b: List<Int> -> b.reversed().take(a).reversed()}

//Iguais
val iguais = {a:Int, b:Int, c:Int -> when {
    (a == b && a == c) -> 3
    (a == b && a != c) -> 2
    (a != b && b == c) -> 2
    (a != b && a == c) -> 2
    else -> 0
}
}

//Miolo
val miolo = {a: List<Int> -> a.subList(1, a.last()-1)}

//Gangorra
val gangorra = {a:Int, b:Int, c:Int, d:Int ->
    if(a * b == c * d) 0
    else if (a * b >= c * d) -1
    else 1
}
//Menor2
val min2 = {a:Int, b:Int -> min(a,b)}

//Menor3
val min3 = {a:Int, b:Int, c:Int -> minOf(a,b,c) }

//Soma2
val soma2 = {a:Int, b:Int -> a + b}

//Capítulo 4

//SomaÍmpares
val somaImpares = {a: List<Int> -> a.filter { it % 2 != 0 }.sum()}

//Max3
val max3 = {a:Int, b:Int, c:Int -> maxOf(a, b, c) }

//Fatorial
fun fatorial(a:Int):Int {
    return if(a == 0) 1
    else if(a == 1) 1
    else a * fatorial(a-1)
}
fun factorial(a: Int):Int {
    var ans = 1
    var i: Int = a
    while(i>1){
        ans*=i
        i--
    }
    return ans
}


//Elemento
val elemento = {a:Int, b:List<Int> ->
    when{
        a > 0 ->b.get(a)
        else -> b.reversed().get(a * (-1) - 1)
}
}
//Pertence
val pertence = {a:Int, b:Array<Int> -> b.any { it == a }}

//Total
val total = {a:List<Int> -> a.map { it -> 1 }.sum() }

//Maior
val maior = { a:Array<Int> ->
    for (i in 0 until a.size) {
        for (j in i + 1 until a.size) {
            if (a[i] > a[j]) {
                a[i]

            }
        }
    }
}

//Corpo
val corpo = {a:Array<Int> -> a.dropLast(1)}

//Divide
val divide = {a:List<Int>, b:Int -> a.chunked(b)}

//União
val uniao = {a: List<Int>, b: List<Int> -> a.union(b)}

//Intersec
val intersec = {a:List<Int>, b:List<Int> -> a.intersect(b)}

//Splitints
val splitints = {a: List<Int> -> a.groupBy { it % 2 == 0 }.values}

//Sublist
val sublist = {a: Int, b:Int, c:List<Int> ->  when {
    (a < 0) -> c.subList(a*(-1),b)
    (b < 0) -> c.subList(a, b*(-1))
    else -> c.subList(a,b)
}
}
//Paridade
val paridade = {a:List<Boolean> ->
    when{
    a.count { it == true } % 2 == 0 -> true
        else -> false
}}

//Swap
fun swaper(a:Array<Int>, b:Int, c:Int):Array<Int>{
    var temp = a.get(c)
    a.set(c, a.get(b))
    a.set(b,temp)
    return a
}

//Euler
fun euler(a: Int):Int{
    var ans = 0
    var i = 1
    while (i < a) {
        if(i % 3 == 0 || i % 5 == 0) {
            ans += i
        }
        i++
    }
    return ans
}

fun main (){

//    println(countneg(neg(listOf(1, 2, 3, -1, -2, 1))))
//    println(final(3, listOf(2,5,4,5,9,8)))
//    println(iguais(1,1,3))
//    println(miolo(listOf(1,2,3,4,5)))
//    println(gangorra(30, 100, 70, 50))
//    println(min2(1,2))
//    println(min3(3,5,12))
//    println(soma2(3, 5))
//    println(somaImpares(listOf(1,1,4,2)))
//    println(max3(1, 2, 8))
//    println(factorial(5))
//    println(elemento(2, listOf(2,7,3,8)))
//    println(pertence(2, arrayOf(3,7,4,2)))
//    println(total(listOf(2,5,5)))
//    println(maior(arrayOf(1,2,5)))
//    println(corpo(arrayOf(1,2,5)))
//    println(divide(arrayListOf(1,2,3,4), 3))
//    println(uniao(listOf(4,5) , listOf(4,2,5)))
//    println(intersec(listOf(3,5,5,7), listOf(9,7,2,1,3,8)))
//    println(splitints(listOf(3,5,7,8,10)))
//    println(sublist(0, 3, listOf(0,2,2,3,4,5,7,8,9,10)))
//    println(paridade(listOf(true,true)))
//    println(swaper(arrayOf(1,3,4), 1, 2))
//    println(euler(50))

}

