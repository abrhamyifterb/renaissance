package org.renaissance.text

import org.scalatest.flatspec.AnyFlatSpec
import org.renaissance.BenchmarkContext

class RegexMatchingTest extends AnyFlatSpec {
  "RegexMatching" should "match the correct number of occurrences" in {
    val benchmark = new RegexMatching
    val context = new TestBenchmarkContext(isDefaultConfiguration = true)

    benchmark.setUpBeforeAll(context)

    val result = benchmark.run(context)

    val expectedMatchCount = 1000
    assert(result.validator.isValid(expectedMatchCount))

    benchmark.tearDownAfterAll(context)
  }
}

class TestBenchmarkContext(isDefaultConfiguration: Boolean) extends BenchmarkContext {
  override def isDefaultConfiguration: Boolean = this.isDefaultConfiguration
}
