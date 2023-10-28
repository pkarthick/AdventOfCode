package main

import (
	"fmt"
	"os"
	"strings"
)

const Start = 'S'
const End = 'E'

type Point struct {
	rowIndex int
	colIndex int
}

func (point Point) clone() Point {
	return Point{
		rowIndex: point.rowIndex,
		colIndex: point.colIndex,
	}
}

type ElevationIndex struct {
	elevation byte
	index     int
	count     int
}

func (elevationIndex ElevationIndex) clone() ElevationIndex {
	return ElevationIndex{
		elevation: elevationIndex.elevation,
		index:     elevationIndex.index,
		count:     elevationIndex.count,
	}
}

type NavigationPath struct {
	points            []Point
	field             *NavigationField
	maxElevationIndex ElevationIndex
}

func (path NavigationPath) contains(point Point) bool {

	for l := len(path.points) - 1; l >= 0; l -= 1 {
		p := path.points[l]
		if p.colIndex == point.colIndex && p.rowIndex == point.rowIndex {
			return true
		}
	}
	return false
}

func (path NavigationPath) availablePaths() []NavigationPath {

	last := path.points[len(path.points)-1]

	points := []Point{
		{rowIndex: last.rowIndex, colIndex: last.colIndex + 1},
		{rowIndex: last.rowIndex + 1, colIndex: last.colIndex},
		{rowIndex: last.rowIndex - 1, colIndex: last.colIndex},
		{rowIndex: last.rowIndex, colIndex: last.colIndex - 1},
	}

	lastElevation := last.getElevation(path.field)

	paths := []NavigationPath{}

	for _, point := range points {

		if point.rowIndex < 0 || point.colIndex < 0 || point.rowIndex == path.field.rowCount || point.colIndex == path.field.columnCount || path.contains(point) {
			continue
		}

		elevation := point.getElevation(path.field)

		if elevation != 'z' && elevation == End {
			continue
		}

		newPoints := []Point{}

		for _, p := range path.points {
			newPoints = append(newPoints, p.clone())
		}

		newPoints = append(newPoints, point)

		if elevation == lastElevation {
			paths = append(paths, NavigationPath{points: newPoints, field: path.field, maxElevationIndex: ElevationIndex{index: path.maxElevationIndex.index, elevation: elevation, count: len(path.points) + 1}})
		} else if elevation-1 == lastElevation {
			paths = append(paths, NavigationPath{points: newPoints, field: path.field, maxElevationIndex: ElevationIndex{index: len(path.points), elevation: elevation, count: len(path.points) + 1}})
		}

	}

	return paths

}

func (point Point) getElevation(field *NavigationField) byte {

	elevation := field.elevationRows[point.rowIndex][point.colIndex]

	if elevation == Start {
		return 97
	}

	return elevation
}

func (path NavigationPath) getElevation() byte {

	point := path.points[len(path.points)-1]

	elevation := path.field.elevationRows[point.rowIndex][point.colIndex]

	if elevation == Start {
		return 97
	}

	return elevation
}

type NavigationField struct {
	elevationRows []string
	pending       []NavigationPath
	processed     map[Point]ElevationIndex
	columnCount   int
	rowCount      int
	origin        Point
	destination   Point
	shortest      *NavigationPath
}

func findPoint(elevationRows []string, elevation rune) Point {

	var point Point

	for r, row := range elevationRows {
		for c, el := range row {
			if el == elevation {
				point = Point{
					rowIndex: r,
					colIndex: c,
				}

			}
		}
	}

	return point

}

func (field *NavigationField) navigatePendingPaths() int {

	for len(field.pending) > 0 {

		path := field.pending[len(field.pending)-1]

		if path.getElevation() == 'z' {

			if field.shortest == nil {
				field.shortest = &path
			} else if len(path.points) < len(field.shortest.points) {
				field.shortest = &path
			}
		}

		field.pending = field.pending[0 : len(field.pending)-1]

		for _, newpath := range path.availablePaths() {

			last := newpath.points[len(newpath.points)-1]
			lastClone := Point{rowIndex: last.rowIndex, colIndex: last.colIndex}
			existing, ok := field.processed[lastClone]

			if ok {
				if existing.elevation < newpath.maxElevationIndex.elevation || existing.index > newpath.maxElevationIndex.index || existing.count >= len(newpath.points) {
					field.processed[lastClone] = newpath.maxElevationIndex.clone()
					field.pending = append(field.pending, newpath)
				} else {
					x := 0
					fmt.Println("{}", x)
				}
			} else {
				field.processed[lastClone] = newpath.maxElevationIndex.clone()
				field.pending = append(field.pending, newpath)
			}
		}

	}

	return len(field.shortest.points)

}

func numberOfStepsRequired(input string) int {

	elevationRows := strings.Split(string(input), "\n")

	origin := findPoint(elevationRows, Start)
	destination := findPoint(elevationRows, End)

	input = strings.Replace(input, "S", "a", -1)
	input = strings.Replace(input, "E", "z", -1)

	elevationRows = strings.Split(input, "\n")

	field := NavigationField{
		elevationRows: elevationRows,
		pending:       []NavigationPath{},
		processed:     make(map[Point]ElevationIndex),
		columnCount:   len(elevationRows[0]),
		rowCount:      len(elevationRows),
		origin:        origin,
		destination:   destination,
		shortest:      nil,
	}

	field.pending = append(field.pending, NavigationPath{points: []Point{field.origin}, field: &field, maxElevationIndex: ElevationIndex{elevation: 97, index: 0, count: 1}})

	return field.navigatePendingPaths()

}

func main() {

	input, _ := os.ReadFile("puzzle")
	output := numberOfStepsRequired(string(input))
	fmt.Println(output)

}
