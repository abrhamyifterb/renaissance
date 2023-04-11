package org.renaissance.text

import org.renaissance.Benchmark
import org.renaissance.Benchmark._
import org.renaissance.BenchmarkContext
import org.renaissance.BenchmarkResult
import org.renaissance.BenchmarkResult.Validators
import org.renaissance.License
import scala.collection.JavaConverters._
import scala.io.Source
import java.io.FileNotFoundException


@Name("regex-matching")
@Group("text")
@Summary("Performs regular expression matching on a text file using Scala.")
@Licenses(Array(License.APACHE2))
@Repetitions(10)
@Parameter(name = "input_path", defaultValue = "shakespeare.txt")
@Configuration(name = "test", settings = Array())
@Configuration(name = "jmh")
final class RegexMatching extends Benchmark {

  private var textFile: String = _
  private var regexFile: String = _

  private var text: String = _
  private var regexPatterns: List[String] = _

  private var expectedMatches = 1618171

override def setUpBeforeAll(c: BenchmarkContext): Unit = {
  textFile = c.parameter("input_path").value
  regexFile = "regex_match_shakespeare.txt"

  val classLoader = getClass.getClassLoader
  val textInputStream = classLoader.getResourceAsStream(textFile)
  if (textInputStream == null) {
    throw new FileNotFoundException(s"Resource file not found: $textFile")
  }
  text = Source.fromInputStream(textInputStream).mkString

  val regexInputStream = classLoader.getResourceAsStream(regexFile)
  if (regexInputStream == null) {
    throw new FileNotFoundException(s"Resource file not found: $regexFile")
  }
  regexPatterns = Source.fromInputStream(regexInputStream).getLines().toList
}

  override def run(c: BenchmarkContext): BenchmarkResult = {
    val totalMatches = regexPatterns.map { pattern =>
      val regex = pattern.r
      regex.findAllIn(text).length
    }.sum

    Validators.simple("matchCount", expectedMatches, totalMatches)
  }

  override def tearDownAfterAll(c: BenchmarkContext): Unit = {
    text = null
    regexPatterns = null
  }
}
