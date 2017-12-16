package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("times"){
    val charCounter: List[(Char, Int)]= times(List('a', 'b', 'a'))
    val map: Map[Char, Int]= charCounter.toMap
    assert(map('a') == 2 && map('b') == 1)
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton"){
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    /*List( Leaf(e,1), Leaf(t,2), Fork(Leaf(e,1), Leaf(t,2), List(e, t),3), Leaf(x,4))
    did not equal
     List(                        Fork(Leaf(e,1), Leaf(t,2), List(e, t),3), Leaf(x,4))
     */
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until"){
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) ==
      Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
  }

  test("create code tree"){
    assert(createCodeTree(string2Chars("xetxxtx")) ==
      Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
  }

  test("decode"){
    val bits: List[Bit] = List(1, 0, 1, 0, 0)
    val chars: List[Char] = List('x', 't', 'e')
    assert(decode(createCodeTree(string2Chars("xetxxtx")), bits) === chars)
  }

  test("decode secret"){
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode secret"){
    assert(encode(frenchCode)(decodedSecret) == secret)
  }

  test("codeBits"){
    val codeTable: CodeTable = List(('a', List(0,1,0)), ('b', List(0,0,1)), ('c', List(1,1,1)))
    assert(codeBits(codeTable)('c') == List(1,1,1))
    assert(codeBits(codeTable)('b') == List(0,0,1))
    assert(codeBits(codeTable)('a') == List(0,1,0))


  }
  test("encode xte"){
    val bits: List[Bit] = List(1, 0, 1, 0, 0)
    val chars: List[Char] = List('x', 't', 'e')
    assert(encode(createCodeTree(string2Chars("xetxxtx")))(chars) === bits)
  }

  test("quick encode"){
    new TestTrees {
      val huff = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
      val tree = createCodeTree(huff)
      assert(decode(tree, quickEncode(tree)(huff)) === huff)
    }
  }


}
