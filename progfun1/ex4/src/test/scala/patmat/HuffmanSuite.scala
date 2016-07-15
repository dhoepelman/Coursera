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


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode some longer text should be identity (using encode)") {
    val string = List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.')
    val tree = createCodeTree(string)
    assert(decode(tree, encode(tree)(string)) === string)
  }

  test("decode and encode some longer text should be identity (using quickEncode)") {
    val string = List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.')
    val tree = createCodeTree(string)
    assert(decode(tree, quickEncode(tree)(string)) === string)
  }

  test("frenccode fail") {
    val code = "encoreuntextetressecret".toList
    val good = List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1)
    decode(frenchCode, good) === code
    encode(frenchCode)(code) === good
  }

  test("optimal1") {
    val string = "aaaaabbc".toList
    val reverse = string.reverse
    val codetree = createCodeTree(string)
    val codetreereverse = createCodeTree(reverse)
    codetree === codetreereverse
    val encoded = encode(codetree)(string)
    encoded === List(0,0,0,0,0,10,10,11)
  }

}
