package day05

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int
import scala.compiletime.ops.long

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day05"
)

enum Kind:
  case Seed, Soil, Fertilizer, Water, Light, Temparature, Humidity, Location

enum Pair:
  case Mapped(start: Long, len: Long)
  case Unmapped(start: Long, len: Long)

  def value() =
    this match {
      case Mapped(start, len)   => (start, len)
      case Unmapped(start, len) => (start, len)
    }

case class MapInfo(dest: Long, src: Long, len: Long):

  def findDestination(value: Long) =
    if value < src + len && value >= src
    then Some(dest + (value - src))
    else None

  def findDestination(pair: Pair): Option[List[Pair]] =

    val (v, vl) = pair.value()

    if v + vl <= src || v >= src + len
    then None
    else

      val ls = (v < src, v + vl > src + len) match
        case (true, true) =>
          List(
            Pair.Unmapped(v, src - v),
            Pair.Mapped(dest, len),
            Pair.Unmapped(src + len, v + vl - (src + len))
          )
        case (true, false) =>
          List(Pair.Unmapped(v, src - v), Pair.Mapped(dest, v + vl - src))
        case (false, true) =>
          List(
            Pair.Mapped(dest + (v - src), src + len - v),
            Pair.Unmapped(src + len, (v + vl) - (src + len))
          )
        case (false, false) =>
          List(Pair.Mapped(dest + (v - src), vl))

      Some(ls)

case class Almanac(
    seeds: Array[Long],
    mappers: Map[(Kind, Kind), List[MapInfo]]
):
  def findLocationBySeed(sk: Kind, s: Long): Long =
    nextKind(sk) match {
      case Some(dk) =>
        val entries = mappers((sk, dk))
        val x = entries
          .map(mi => mi.findDestination(s))
          .dropWhile(_.isEmpty)
          .toList match {
          case Some(d) :: _ => findLocationBySeed(dk, d)
          case _            => findLocationBySeed(dk, s)
        }
        x
      case None => s
    }

  def findLocationBySeedRange(sk: Kind, pairs: List[Pair]): Long =
    nextKind(sk) match {
      case Some(dk) =>
        val entries = mappers((sk, dk))

        val lss: List[(Option[List[day05.Pair]], day05.Pair)] = entries
          .flatMap(mi => pairs.map(pair => (mi.findDestination(pair), pair)))

        if lss.forall(_._1.isEmpty) then findLocationBySeedRange(dk, pairs)
        else

          val emptyDefaults = lss
            .filter(_._1.isEmpty)
            .map((_, p) => Pair.Unmapped(p.value()._1, p.value()._2))
            .toSet
            .toList

          val (mapped, unmapped) =
            lss.filter(!_._1.isEmpty).map(_._1.get).flatten.partition {
              case (Pair.Mapped(start, len)) => true
              case Pair.Unmapped(start, len) => false
            }

          val unmapped1 = Set.newBuilder[Pair]

          for uls <- pairs do {
            val (us, ul) = uls.value()

            if mapped.isEmpty then unmapped1 += Pair.Unmapped(us, ul)
            else

              for mls <- mapped do {
                val (ms, ml) = mls.value()

                if !(us + ul <= ms || us >= ms + ml) then {

                  (us <= ms, ms + ml <= us + ul) match {

                    case (true, true) =>
                      if ms - us > 0 then
                        unmapped1 += Pair.Unmapped(us, ms - us)

                      if us + ul - (ms + ml) > 0 then
                        unmapped1 += Pair.Unmapped(ms + ml, us + ul - (ms + ml))

                    case (true, false) =>
                      if ms - us > 0 then
                        unmapped1 += Pair.Unmapped(us, ms - us)

                    case (false, true) =>
                      if us + ul - (ms + ml) > 0 then
                        unmapped1 += Pair.Unmapped(ms + ml, us + ul - (ms + ml))

                    case (false, false) => unmapped1 += Pair.Unmapped(us, ul)

                  }

                }

              }

          }

          val ms = mapped.toSet.toList
          val us = unmapped1.result().toList

          val xs = ms ++ us
          findLocationBySeedRange(dk, xs)

      case None =>
        pairs.map(p => p.value()._1).min
    }

def createAlmanac(input: String) =
  input.split("\n\n") match
    case Array(
          seeds_raw,
          seed2soil,
          soil2fertilizer,
          fertilizer2water,
          water2light,
          light2temp,
          tem2hum,
          hum2loc
        ) =>
      def getEntries(s: String) = {
        s.split("\n")
          .toList
          .drop(1)
          .map { case s"$dest $src $len" =>
            MapInfo(dest.toLong, src.toLong, len.toLong)
          }
          .toList
      }

      var mappers: Map[(Kind, Kind), List[MapInfo]] = Map()
      val seeds = seeds_raw match {
        case s"seeds: $rest" =>
          rest.split(" ").map(_.toLong)
      }

      mappers += ((Kind.Seed, Kind.Soil) -> getEntries(seed2soil))
      mappers += ((Kind.Soil, Kind.Fertilizer) -> getEntries(soil2fertilizer))
      mappers += ((Kind.Fertilizer, Kind.Water) -> getEntries(fertilizer2water))
      mappers += ((Kind.Water, Kind.Light) -> getEntries(water2light))
      mappers += ((Kind.Light, Kind.Temparature) -> getEntries(light2temp))
      mappers += ((Kind.Temparature, Kind.Humidity) -> getEntries(tem2hum))
      mappers += ((Kind.Humidity, Kind.Location) -> getEntries(hum2loc))

      Almanac(seeds, mappers)

def nextKind(k: Kind) =
  k match
    case Kind.Seed        => Some(Kind.Soil)
    case Kind.Soil        => Some(Kind.Fertilizer)
    case Kind.Fertilizer  => Some(Kind.Water)
    case Kind.Water       => Some(Kind.Light)
    case Kind.Light       => Some(Kind.Temparature)
    case Kind.Temparature => Some(Kind.Humidity)
    case Kind.Humidity    => Some(Kind.Location)
    case Kind.Location    => None

def part1(input: String): String =
  val almanac = createAlmanac(input)
  almanac.seeds
    .map(s => almanac.findLocationBySeed(Kind.Seed, s))
    .min
    .toString()

def part2(input: String): String =

  val almanac = createAlmanac(input)
  // val pairs =
  //   almanac.seeds.grouped(2).map(a => Pair.Unmapped(a(0), a(1))).toList

  val pairs =
    almanac.seeds
      .grouped(2)
      .map(a => Pair.Unmapped(a(0), a(0) + a(1) - 1))
      .toList
  
  val x = almanac.findLocationBySeedRange(Kind.Seed, pairs)
  
  x.toString()

@main def part12: Unit =

  val input = loadInput()
