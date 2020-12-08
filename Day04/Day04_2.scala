import scala.io.Source

object Day04 extends App {
  val input_text = Source.fromFile("inputs/input.txt").mkString

  val byr_ptn = "byr:(?:19[2-8][0-9]|199[0-9]|200[0-2])"
  val iyr_ptn = "iyr:(?:201[0-9]|2020)"
  val eyr_ptn = "eyr:(?:202[0-9]|2030)"
  val hgt_ptn = "hgt:(?:(?:1[5-8][0-9]|19[0-3])cm|(?:59|6[0-9]|7[0-6])in)"
  val hcl_ptn = "hcl:#[0-9a-f]{6}"
  val ecl_ptn = "ecl:(?:amb|blu|brn|gry|grn|hzl|oth)"
  val pid_ptn = "pid:[0-9]{9}"

  // Note, for this regex to work we've assumed there are no passports with
  // duplicated fields.
  val field_pattern = raw"(?:$byr_ptn|$iyr_ptn|$eyr_ptn|$hgt_ptn|$hcl_ptn|$ecl_ptn|$pid_ptn)"
  val pattern = raw"(?:${field_pattern}\s(?:cid:\S+\s)?){7}".r
  val matches = pattern.findAllIn(input_text)

  println(matches.length)
}